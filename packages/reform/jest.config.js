module.exports = {
  verbose: true,
  roots: ["<rootDir>/src/", "<rootDir>/__tests__/"],
  testMatch: ['**/*_Test.res.js'],
  testPathIgnorePatterns: ['<rootDir>/lib/'],
  collectCoverageFrom: ['src/**/*.res.js'],
  coverageDirectory: '<rootDir>/coverage',
  coverageReporters: ['text', 'lcov'],
  coverageThreshold: {
    global: {
      statements: 10,
      branches: 0,
      functions: 5,
      lines: 10,
    },
    './src/ReForm__Helpers.res.js': {
      statements: 100,
      branches: 100,
      functions: 100,
      lines: 100,
    },
  },
};
