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
-- Module      : Amazonka.QuickSight.Types.ThemeError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ThemeError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ThemeErrorType

-- | Theme error.
--
-- /See:/ 'newThemeError' smart constructor.
data ThemeError = ThemeError'
  { -- | The type of error.
    type' :: Prelude.Maybe ThemeErrorType,
    -- | The error message.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThemeError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'themeError_type' - The type of error.
--
-- 'message', 'themeError_message' - The error message.
newThemeError ::
  ThemeError
newThemeError =
  ThemeError'
    { type' = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The type of error.
themeError_type :: Lens.Lens' ThemeError (Prelude.Maybe ThemeErrorType)
themeError_type = Lens.lens (\ThemeError' {type'} -> type') (\s@ThemeError' {} a -> s {type' = a} :: ThemeError)

-- | The error message.
themeError_message :: Lens.Lens' ThemeError (Prelude.Maybe Prelude.Text)
themeError_message = Lens.lens (\ThemeError' {message} -> message) (\s@ThemeError' {} a -> s {message = a} :: ThemeError)

instance Core.FromJSON ThemeError where
  parseJSON =
    Core.withObject
      "ThemeError"
      ( \x ->
          ThemeError'
            Prelude.<$> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Message")
      )

instance Prelude.Hashable ThemeError where
  hashWithSalt _salt ThemeError' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` message

instance Prelude.NFData ThemeError where
  rnf ThemeError' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf message
