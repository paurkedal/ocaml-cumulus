## Status

This library is for now an experiment. The API may be revised and project
may be discontinued. Easily.

## Synopsis

Cumulus defines a signal-like type, which facilitates differential updates
of the underlying value. When a cumulus signal is changed, it emits a patch
along with the new value, allowing consumers to integrate it into an
existing value rather than rebuilding its value from scratch. Cumulus is
based on the React FRP library.
