{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppSync.Types.CodeError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.CodeError where

import Amazonka.AppSync.Types.CodeErrorLocation
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an AppSync error.
--
-- /See:/ 'newCodeError' smart constructor.
data CodeError = CodeError'
  { -- | The type of code error.
    --
    -- Examples include, but aren\'t limited to: @LINT_ERROR@, @PARSER_ERROR@.
    errorType :: Prelude.Maybe Prelude.Text,
    -- | The line, column, and span location of the error in the code.
    location :: Prelude.Maybe CodeErrorLocation,
    -- | A user presentable error.
    --
    -- Examples include, but aren\'t limited to:
    -- @Parsing error: Unterminated string literal@.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorType', 'codeError_errorType' - The type of code error.
--
-- Examples include, but aren\'t limited to: @LINT_ERROR@, @PARSER_ERROR@.
--
-- 'location', 'codeError_location' - The line, column, and span location of the error in the code.
--
-- 'value', 'codeError_value' - A user presentable error.
--
-- Examples include, but aren\'t limited to:
-- @Parsing error: Unterminated string literal@.
newCodeError ::
  CodeError
newCodeError =
  CodeError'
    { errorType = Prelude.Nothing,
      location = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The type of code error.
--
-- Examples include, but aren\'t limited to: @LINT_ERROR@, @PARSER_ERROR@.
codeError_errorType :: Lens.Lens' CodeError (Prelude.Maybe Prelude.Text)
codeError_errorType = Lens.lens (\CodeError' {errorType} -> errorType) (\s@CodeError' {} a -> s {errorType = a} :: CodeError)

-- | The line, column, and span location of the error in the code.
codeError_location :: Lens.Lens' CodeError (Prelude.Maybe CodeErrorLocation)
codeError_location = Lens.lens (\CodeError' {location} -> location) (\s@CodeError' {} a -> s {location = a} :: CodeError)

-- | A user presentable error.
--
-- Examples include, but aren\'t limited to:
-- @Parsing error: Unterminated string literal@.
codeError_value :: Lens.Lens' CodeError (Prelude.Maybe Prelude.Text)
codeError_value = Lens.lens (\CodeError' {value} -> value) (\s@CodeError' {} a -> s {value = a} :: CodeError)

instance Data.FromJSON CodeError where
  parseJSON =
    Data.withObject
      "CodeError"
      ( \x ->
          CodeError'
            Prelude.<$> (x Data..:? "errorType")
            Prelude.<*> (x Data..:? "location")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable CodeError where
  hashWithSalt _salt CodeError' {..} =
    _salt
      `Prelude.hashWithSalt` errorType
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` value

instance Prelude.NFData CodeError where
  rnf CodeError' {..} =
    Prelude.rnf errorType `Prelude.seq`
      Prelude.rnf location `Prelude.seq`
        Prelude.rnf value
