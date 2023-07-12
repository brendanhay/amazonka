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
-- Module      : Amazonka.SNS.Types.SMSSandboxPhoneNumber
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SNS.Types.SMSSandboxPhoneNumber where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SNS.Types.SMSSandboxPhoneNumberVerificationStatus

-- | A verified or pending destination phone number in the SMS sandbox.
--
-- When you start using Amazon SNS to send SMS messages, your Amazon Web
-- Services account is in the /SMS sandbox/. The SMS sandbox provides a
-- safe environment for you to try Amazon SNS features without risking your
-- reputation as an SMS sender. While your Amazon Web Services account is
-- in the SMS sandbox, you can use all of the features of Amazon SNS.
-- However, you can send SMS messages only to verified destination phone
-- numbers. For more information, including how to move out of the sandbox
-- to send messages without restrictions, see
-- <https://docs.aws.amazon.com/sns/latest/dg/sns-sms-sandbox.html SMS sandbox>
-- in the /Amazon SNS Developer Guide/.
--
-- /See:/ 'newSMSSandboxPhoneNumber' smart constructor.
data SMSSandboxPhoneNumber = SMSSandboxPhoneNumber'
  { -- | The destination phone number.
    phoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The destination phone number\'s verification status.
    status :: Prelude.Maybe SMSSandboxPhoneNumberVerificationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SMSSandboxPhoneNumber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumber', 'sMSSandboxPhoneNumber_phoneNumber' - The destination phone number.
--
-- 'status', 'sMSSandboxPhoneNumber_status' - The destination phone number\'s verification status.
newSMSSandboxPhoneNumber ::
  SMSSandboxPhoneNumber
newSMSSandboxPhoneNumber =
  SMSSandboxPhoneNumber'
    { phoneNumber =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The destination phone number.
sMSSandboxPhoneNumber_phoneNumber :: Lens.Lens' SMSSandboxPhoneNumber (Prelude.Maybe Prelude.Text)
sMSSandboxPhoneNumber_phoneNumber = Lens.lens (\SMSSandboxPhoneNumber' {phoneNumber} -> phoneNumber) (\s@SMSSandboxPhoneNumber' {} a -> s {phoneNumber = a} :: SMSSandboxPhoneNumber)

-- | The destination phone number\'s verification status.
sMSSandboxPhoneNumber_status :: Lens.Lens' SMSSandboxPhoneNumber (Prelude.Maybe SMSSandboxPhoneNumberVerificationStatus)
sMSSandboxPhoneNumber_status = Lens.lens (\SMSSandboxPhoneNumber' {status} -> status) (\s@SMSSandboxPhoneNumber' {} a -> s {status = a} :: SMSSandboxPhoneNumber)

instance Data.FromXML SMSSandboxPhoneNumber where
  parseXML x =
    SMSSandboxPhoneNumber'
      Prelude.<$> (x Data..@? "PhoneNumber")
      Prelude.<*> (x Data..@? "Status")

instance Prelude.Hashable SMSSandboxPhoneNumber where
  hashWithSalt _salt SMSSandboxPhoneNumber' {..} =
    _salt
      `Prelude.hashWithSalt` phoneNumber
      `Prelude.hashWithSalt` status

instance Prelude.NFData SMSSandboxPhoneNumber where
  rnf SMSSandboxPhoneNumber' {..} =
    Prelude.rnf phoneNumber
      `Prelude.seq` Prelude.rnf status
