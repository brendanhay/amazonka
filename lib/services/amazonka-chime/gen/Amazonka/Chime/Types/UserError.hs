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
-- Module      : Amazonka.Chime.Types.UserError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.UserError where

import Amazonka.Chime.Types.ErrorCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The list of errors returned when errors are encountered during the
-- BatchSuspendUser, BatchUnsuspendUser, or BatchUpdateUser actions. This
-- includes user IDs, error codes, and error messages.
--
-- /See:/ 'newUserError' smart constructor.
data UserError = UserError'
  { -- | The error message.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The user ID for which the action failed.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The error code.
    errorCode :: Prelude.Maybe ErrorCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'userError_errorMessage' - The error message.
--
-- 'userId', 'userError_userId' - The user ID for which the action failed.
--
-- 'errorCode', 'userError_errorCode' - The error code.
newUserError ::
  UserError
newUserError =
  UserError'
    { errorMessage = Prelude.Nothing,
      userId = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The error message.
userError_errorMessage :: Lens.Lens' UserError (Prelude.Maybe Prelude.Text)
userError_errorMessage = Lens.lens (\UserError' {errorMessage} -> errorMessage) (\s@UserError' {} a -> s {errorMessage = a} :: UserError)

-- | The user ID for which the action failed.
userError_userId :: Lens.Lens' UserError (Prelude.Maybe Prelude.Text)
userError_userId = Lens.lens (\UserError' {userId} -> userId) (\s@UserError' {} a -> s {userId = a} :: UserError)

-- | The error code.
userError_errorCode :: Lens.Lens' UserError (Prelude.Maybe ErrorCode)
userError_errorCode = Lens.lens (\UserError' {errorCode} -> errorCode) (\s@UserError' {} a -> s {errorCode = a} :: UserError)

instance Data.FromJSON UserError where
  parseJSON =
    Data.withObject
      "UserError"
      ( \x ->
          UserError'
            Prelude.<$> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "UserId")
            Prelude.<*> (x Data..:? "ErrorCode")
      )

instance Prelude.Hashable UserError where
  hashWithSalt _salt UserError' {..} =
    _salt `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` errorCode

instance Prelude.NFData UserError where
  rnf UserError' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf errorCode
