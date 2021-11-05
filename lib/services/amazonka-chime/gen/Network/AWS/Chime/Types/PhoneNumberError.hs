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
-- Module      : Network.AWS.Chime.Types.PhoneNumberError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Chime.Types.PhoneNumberError where

import Network.AWS.Chime.Types.ErrorCode
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | If the phone number action fails for one or more of the phone numbers in
-- the request, a list of the phone numbers is returned, along with error
-- codes and error messages.
--
-- /See:/ 'newPhoneNumberError' smart constructor.
data PhoneNumberError = PhoneNumberError'
  { -- | The phone number ID for which the action failed.
    phoneNumberId :: Prelude.Maybe Prelude.Text,
    -- | The error code.
    errorCode :: Prelude.Maybe ErrorCode,
    -- | The error message.
    errorMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PhoneNumberError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumberId', 'phoneNumberError_phoneNumberId' - The phone number ID for which the action failed.
--
-- 'errorCode', 'phoneNumberError_errorCode' - The error code.
--
-- 'errorMessage', 'phoneNumberError_errorMessage' - The error message.
newPhoneNumberError ::
  PhoneNumberError
newPhoneNumberError =
  PhoneNumberError'
    { phoneNumberId = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | The phone number ID for which the action failed.
phoneNumberError_phoneNumberId :: Lens.Lens' PhoneNumberError (Prelude.Maybe Prelude.Text)
phoneNumberError_phoneNumberId = Lens.lens (\PhoneNumberError' {phoneNumberId} -> phoneNumberId) (\s@PhoneNumberError' {} a -> s {phoneNumberId = a} :: PhoneNumberError)

-- | The error code.
phoneNumberError_errorCode :: Lens.Lens' PhoneNumberError (Prelude.Maybe ErrorCode)
phoneNumberError_errorCode = Lens.lens (\PhoneNumberError' {errorCode} -> errorCode) (\s@PhoneNumberError' {} a -> s {errorCode = a} :: PhoneNumberError)

-- | The error message.
phoneNumberError_errorMessage :: Lens.Lens' PhoneNumberError (Prelude.Maybe Prelude.Text)
phoneNumberError_errorMessage = Lens.lens (\PhoneNumberError' {errorMessage} -> errorMessage) (\s@PhoneNumberError' {} a -> s {errorMessage = a} :: PhoneNumberError)

instance Core.FromJSON PhoneNumberError where
  parseJSON =
    Core.withObject
      "PhoneNumberError"
      ( \x ->
          PhoneNumberError'
            Prelude.<$> (x Core..:? "PhoneNumberId")
            Prelude.<*> (x Core..:? "ErrorCode")
            Prelude.<*> (x Core..:? "ErrorMessage")
      )

instance Prelude.Hashable PhoneNumberError

instance Prelude.NFData PhoneNumberError
