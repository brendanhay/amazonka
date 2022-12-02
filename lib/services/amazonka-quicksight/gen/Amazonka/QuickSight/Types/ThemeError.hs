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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ThemeError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ThemeErrorType

-- | Theme error.
--
-- /See:/ 'newThemeError' smart constructor.
data ThemeError = ThemeError'
  { -- | The error message.
    message :: Prelude.Maybe Prelude.Text,
    -- | The type of error.
    type' :: Prelude.Maybe ThemeErrorType
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
-- 'message', 'themeError_message' - The error message.
--
-- 'type'', 'themeError_type' - The type of error.
newThemeError ::
  ThemeError
newThemeError =
  ThemeError'
    { message = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The error message.
themeError_message :: Lens.Lens' ThemeError (Prelude.Maybe Prelude.Text)
themeError_message = Lens.lens (\ThemeError' {message} -> message) (\s@ThemeError' {} a -> s {message = a} :: ThemeError)

-- | The type of error.
themeError_type :: Lens.Lens' ThemeError (Prelude.Maybe ThemeErrorType)
themeError_type = Lens.lens (\ThemeError' {type'} -> type') (\s@ThemeError' {} a -> s {type' = a} :: ThemeError)

instance Data.FromJSON ThemeError where
  parseJSON =
    Data.withObject
      "ThemeError"
      ( \x ->
          ThemeError'
            Prelude.<$> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable ThemeError where
  hashWithSalt _salt ThemeError' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ThemeError where
  rnf ThemeError' {..} =
    Prelude.rnf message `Prelude.seq` Prelude.rnf type'
