#include <cstdint>
#include <string>

namespace ufo {

using std::size_t;

inline constexpr size_t min(size_t a, size_t b) {
  return a < b ? a : b;
}
inline constexpr size_t max(size_t a, size_t b) {
  return a > b ? a : b;
}

inline constexpr size_t to_unsigned(const char* s, size_t N) {
  size_t total = 0;
  size_t power = 1;
  for (size_t idx = N - 1; idx > 0; idx--) {
    int digit = s[idx - 1] - '0';
    total += digit * power;
    power *= 10;
  }
  return total;
}

inline constexpr size_t to_unsigned(std::string_view minLen) {
  const size_t N = minLen.length() + 1;

  size_t total = 0;
  size_t power = 1;
  for (size_t idx = N - 1; idx > 0; idx--) {
    int digit = minLen[idx - 1] - '0';
    total += digit * power;
    power *= 10;
  }
  return total;
}


template<size_t N>
struct StaticString {
  static_assert(N > 0);

  using CharsRef = char (&)[N];
  using CharsConstRef = const char (&)[N];

  constexpr StaticString(): chars_{} {
  }

  constexpr StaticString(const char (&chars)[N]) {
    *this = StaticString<1>{} + chars;
  }

  constexpr CharsRef chars() {
    return chars_;
  }

  constexpr CharsConstRef chars() const {
    return chars_;
  }

  constexpr size_t len() const {
    return N;
  }

  constexpr operator CharsConstRef() const {
    return chars_;
  }

  constexpr operator std::string_view() const {
    return std::string_view(chars_);
  }

  operator std::string() const {
    return std::string(chars_);
  }

  template <size_t DeleteCount, size_t Len>
  constexpr StaticString<N + Len - DeleteCount - 1>
  splice_at(size_t starting_idx, const char(& other)[Len]) const {
    StaticString<N + Len - DeleteCount - 1> result = {};

    char *target = result.chars_;
    for (size_t idx = 0; idx < starting_idx; target++, idx++) {
      *target = chars_[idx];
    }
    for (size_t idx = 0; idx + 1 < Len; target++, idx++) {
      *target = other[idx];
    }
    for (size_t idx = starting_idx + DeleteCount; idx < N; target++, idx++) {
      *target = chars_[idx];
    }

    result.chars_[N + Len - DeleteCount - 2] = '\0';
    return result;
  }

  template <size_t Len>
  constexpr StaticString<N + Len - 1> operator+(const char(& rhs)[Len]) const {
    // this is equivalent to appending rhs to this string
    return splice_at<0, Len>(N - 1, rhs);
  }

  template <size_t Len>
  constexpr StaticString<N + Len - 1> operator+(const StaticString<Len>& rhs) const {
    return *this + rhs.chars_;
  }

  char chars_[N];
};

inline constexpr StaticString<1> cat() {
  return StaticString<1>{};
}

template<typename... ArgType>
constexpr auto cat(const ArgType &...args) {
  return (cat() + ... + args);
}

template<size_t N>
constexpr StaticString<N> makeStatic(const char(& str)[N]) {
  return StaticString<N>(str);
}


struct FormatSpecU {
  size_t numErrors;

  size_t specBegin;
  size_t specLength;

  size_t fillCount;
  size_t fillSeqBegin;
  size_t fillSeqEnd;

  char alignSide;

  constexpr FormatSpecU(std::string_view fs) {
    numErrors = 0;
    fillCount = 0;
    alignSide = '<'; // default align left
    fillSeqBegin = 0;
    fillSeqEnd = 0;

    specBegin = fs.length();
    specLength = 0;

    const char* p = fs.begin();

    for (; p != fs.end(); p++) {
      if (*p != '{') {
        continue;
      }

      specBegin = std::distance(fs.begin(), p);
      specLength++;

      if (++p == fs.end()) {
        numErrors++;
        break;
      }
      if (*p == '}') {
        break;

      } else if (*p == ':') {
        if (++p == fs.end()) {
          numErrors++;
          break;

        } else if (*p == '<' || *p == '>') {
          // if fill sequence is empty, use the default fill sequence
          fillSeqBegin = std::distance(fs.begin(), p);
          fillSeqEnd = fillSeqBegin;

          alignSide = *p;
          if (++p == fs.end()) {
              numErrors++;
              break;
          }
        } else {
          fillSeqBegin = std::distance(fs.begin(), p);
          fillSeqEnd = fillSeqBegin + 1;

          for (p++; p != fs.end(); p++, fillSeqEnd++) {
            if (*p == '<' || *p == '>') {
              alignSide = *p;
              break;
            }
          }
          if (p == fs.end() || ++p == fs.end()) {
            numErrors++;
            break;
          }
        }
        if (!('0' <= *p && *p <= '9')) {
          numErrors++;
          break;
        }
        auto width_begin = p;
        for (; p != fs.end() && '0' <= *p && *p <= '9'; p++) {
          ;
        }
        if (p == fs.end() || *p != '}') {
          numErrors++;
          break;
        }
        fillCount = to_unsigned(std::string_view(width_begin, p));
        break;

      } else {
        numErrors++;
        break;
      }
    }
    specLength += std::distance(fs.begin(), p) - specBegin;
  }
};


struct FormatSpec {
  size_t numErrors;
  size_t width;
  char alignSide;
  char fillChar;

  size_t specBegin;
  size_t specLength;

  constexpr FormatSpec(std::string_view fs) {
    numErrors = 0;
    width = 0;
    alignSide = '<'; // default align left
    fillChar = ' ';

    specBegin = fs.length();
    specLength = 0;

    const char* p = fs.begin();

    for (; p != fs.end(); p++) {
      if (*p != '{') {
        continue;
      }

      specBegin = std::distance(fs.begin(), p);
      specLength++;

      if (++p == fs.end()) {
        numErrors++;
        break;
      }
      if (*p == '}') {
        break;

      } else if (*p == ':') {
        if (++p == fs.end()) {
          numErrors++;
          break;

        } else if (*p == '<' || *p == '>') {
          // alignment token found, use default fillChar
          alignSide = *p;
          if (++p == fs.end()) {
              numErrors++;
              break;
          }
        } else {
          fillChar = *p;

          if (++p == fs.end()) {
            numErrors++;
            break;
          } else if (*p == '<' || *p == '>') {
            alignSide = *p;
            if (++p == fs.end()) {
              numErrors++;
              break;
            }
          }
        }
        if (!('0' <= *p && *p <= '9')) {
          numErrors++;
          break;
        }
        auto width_begin = p;
        for (; p != fs.end() && '0' <= *p && *p <= '9'; p++) {
          ;
        }
        if (p == fs.end() || *p != '}') {
          numErrors++;
          break;
        }
        width = to_unsigned(std::string_view(width_begin, p));
        break;

      } else {
        numErrors++;
        break;
      }
    }
    specLength += std::distance(fs.begin(), p) - specBegin;
  }
};

template <FormatSpec formatSpec, StaticString value>
constexpr auto applyFormat() {
  constexpr size_t Len = max(formatSpec.width, value.len());

  StaticString<Len> result = {};
  char* target = result.chars();
  int idx = 0;

  if (formatSpec.alignSide == '>') {
    for (; idx < Len - value.len(); idx++, target++) {
      *target = formatSpec.fillChar;
    }
    for (idx = 0; idx < value.len(); idx++, target++) {
      *target = value.chars()[idx];
    }
  } else {
    for (; idx + 1 < value.len(); idx++, target++) {
      *target = value.chars()[idx];
    }
    for (; idx + 1 < Len; idx++, target++) {
      *target = formatSpec.fillChar;
    }
  }
  result.chars()[Len - 1] = '\0';
  return result;
}


template <StaticString fmt>
constexpr auto format() {
  return fmt;
}

template <StaticString fmt, StaticString value, StaticString... values>
constexpr auto format() {
  constexpr FormatSpec formatSpec(fmt.chars());
  static_assert(formatSpec.numErrors == 0, "error parsing a placeholder in the format string");
  static_assert(formatSpec.specLength > 0, "not enough placeholders in the format string");

  constexpr size_t numCharsToDelete = formatSpec.specLength;
  constexpr auto formatted = fmt.template splice_at
    <numCharsToDelete>(
    formatSpec.specBegin,
    applyFormat<formatSpec, value>().chars());

  return format<formatted, values...>();
}


template <typename SeparatorT, typename StringT, typename ...StringTs>
constexpr auto join(const SeparatorT& separator, const StringT& str, const StringTs&... strs) {
  return (cat(str) + ... + cat(separator, strs));
}

} // namespace ufo
