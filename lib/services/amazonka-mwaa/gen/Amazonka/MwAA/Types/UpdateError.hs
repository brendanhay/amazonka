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
-- Module      : Amazonka.MwAA.Types.UpdateError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MwAA.Types.UpdateError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the error(s) encountered with the last update of the
-- environment.
--
-- /See:/ 'newUpdateError' smart constructor.
data UpdateError = UpdateError'
  { -- | The error code that corresponds to the error with the last update.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The error message that corresponds to the error code.
    errorMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'updateError_errorCode' - The error code that corresponds to the error with the last update.
--
-- 'errorMessage', 'updateError_errorMessage' - The error message that corresponds to the error code.
newUpdateError ::
  UpdateError
newUpdateError =
  UpdateError'
    { errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | The error code that corresponds to the error with the last update.
updateError_errorCode :: Lens.Lens' UpdateError (Prelude.Maybe Prelude.Text)
updateError_errorCode = Lens.lens (\UpdateError' {errorCode} -> errorCode) (\s@UpdateError' {} a -> s {errorCode = a} :: UpdateError)

-- | The error message that corresponds to the error code.
updateError_errorMessage :: Lens.Lens' UpdateError (Prelude.Maybe Prelude.Text)
updateError_errorMessage = Lens.lens (\UpdateError' {errorMessage} -> errorMessage) (\s@UpdateError' {} a -> s {errorMessage = a} :: UpdateError)

instance Data.FromJSON UpdateError where
  parseJSON =
    Data.withObject
      "UpdateError"
      ( \x ->
          UpdateError'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
      )

instance Prelude.Hashable UpdateError where
  hashWithSalt _salt UpdateError' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage

instance Prelude.NFData UpdateError where
  rnf UpdateError' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
