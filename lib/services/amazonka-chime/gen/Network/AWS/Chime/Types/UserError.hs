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
-- Module      : Network.AWS.Chime.Types.UserError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Chime.Types.UserError where

import Network.AWS.Chime.Types.ErrorCode
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The list of errors returned when errors are encountered during the
-- BatchSuspendUser, BatchUnsuspendUser, or BatchUpdateUser actions. This
-- includes user IDs, error codes, and error messages.
--
-- /See:/ 'newUserError' smart constructor.
data UserError = UserError'
  { -- | The user ID for which the action failed.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The error code.
    errorCode :: Prelude.Maybe ErrorCode,
    -- | The error message.
    errorMessage :: Prelude.Maybe Prelude.Text
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
-- 'userId', 'userError_userId' - The user ID for which the action failed.
--
-- 'errorCode', 'userError_errorCode' - The error code.
--
-- 'errorMessage', 'userError_errorMessage' - The error message.
newUserError ::
  UserError
newUserError =
  UserError'
    { userId = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | The user ID for which the action failed.
userError_userId :: Lens.Lens' UserError (Prelude.Maybe Prelude.Text)
userError_userId = Lens.lens (\UserError' {userId} -> userId) (\s@UserError' {} a -> s {userId = a} :: UserError)

-- | The error code.
userError_errorCode :: Lens.Lens' UserError (Prelude.Maybe ErrorCode)
userError_errorCode = Lens.lens (\UserError' {errorCode} -> errorCode) (\s@UserError' {} a -> s {errorCode = a} :: UserError)

-- | The error message.
userError_errorMessage :: Lens.Lens' UserError (Prelude.Maybe Prelude.Text)
userError_errorMessage = Lens.lens (\UserError' {errorMessage} -> errorMessage) (\s@UserError' {} a -> s {errorMessage = a} :: UserError)

instance Core.FromJSON UserError where
  parseJSON =
    Core.withObject
      "UserError"
      ( \x ->
          UserError'
            Prelude.<$> (x Core..:? "UserId")
            Prelude.<*> (x Core..:? "ErrorCode")
            Prelude.<*> (x Core..:? "ErrorMessage")
      )

instance Prelude.Hashable UserError

instance Prelude.NFData UserError
