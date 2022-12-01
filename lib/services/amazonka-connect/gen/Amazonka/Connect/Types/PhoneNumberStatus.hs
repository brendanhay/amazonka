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
-- Module      : Amazonka.Connect.Types.PhoneNumberStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.PhoneNumberStatus where

import Amazonka.Connect.Types.PhoneNumberWorkflowStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The status of the phone number.
--
-- -   @CLAIMED@ means the previous
--     <https://docs.aws.amazon.com/connect/latest/APIReference/API_ClaimedPhoneNumber.html ClaimedPhoneNumber>
--     or
--     <https://docs.aws.amazon.com/connect/latest/APIReference/API_UpdatePhoneNumber.html UpdatePhoneNumber>
--     operation succeeded.
--
-- -   @IN_PROGRESS@ means a
--     <https://docs.aws.amazon.com/connect/latest/APIReference/API_ClaimedPhoneNumber.html ClaimedPhoneNumber>
--     or
--     <https://docs.aws.amazon.com/connect/latest/APIReference/API_UpdatePhoneNumber.html UpdatePhoneNumber>
--     operation is still in progress and has not yet completed. You can
--     call
--     <https://docs.aws.amazon.com/connect/latest/APIReference/API_DescribePhoneNumber.html DescribePhoneNumber>
--     at a later time to verify if the previous operation has completed.
--
-- -   @FAILED@ indicates that the previous
--     <https://docs.aws.amazon.com/connect/latest/APIReference/API_ClaimedPhoneNumber.html ClaimedPhoneNumber>
--     or
--     <https://docs.aws.amazon.com/connect/latest/APIReference/API_UpdatePhoneNumber.html UpdatePhoneNumber>
--     operation has failed. It will include a message indicating the
--     failure reason. A common reason for a failure may be that the
--     @TargetArn@ value you are claiming or updating a phone number to has
--     reached its limit of total claimed numbers. If you received a
--     @FAILED@ status from a @ClaimPhoneNumber@ API call, you have one day
--     to retry claiming the phone number before the number is released
--     back to the inventory for other customers to claim.
--
-- /See:/ 'newPhoneNumberStatus' smart constructor.
data PhoneNumberStatus = PhoneNumberStatus'
  { -- | The status message.
    message :: Prelude.Maybe Prelude.Text,
    -- | The status.
    status :: Prelude.Maybe PhoneNumberWorkflowStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PhoneNumberStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'phoneNumberStatus_message' - The status message.
--
-- 'status', 'phoneNumberStatus_status' - The status.
newPhoneNumberStatus ::
  PhoneNumberStatus
newPhoneNumberStatus =
  PhoneNumberStatus'
    { message = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The status message.
phoneNumberStatus_message :: Lens.Lens' PhoneNumberStatus (Prelude.Maybe Prelude.Text)
phoneNumberStatus_message = Lens.lens (\PhoneNumberStatus' {message} -> message) (\s@PhoneNumberStatus' {} a -> s {message = a} :: PhoneNumberStatus)

-- | The status.
phoneNumberStatus_status :: Lens.Lens' PhoneNumberStatus (Prelude.Maybe PhoneNumberWorkflowStatus)
phoneNumberStatus_status = Lens.lens (\PhoneNumberStatus' {status} -> status) (\s@PhoneNumberStatus' {} a -> s {status = a} :: PhoneNumberStatus)

instance Core.FromJSON PhoneNumberStatus where
  parseJSON =
    Core.withObject
      "PhoneNumberStatus"
      ( \x ->
          PhoneNumberStatus'
            Prelude.<$> (x Core..:? "Message")
            Prelude.<*> (x Core..:? "Status")
      )

instance Prelude.Hashable PhoneNumberStatus where
  hashWithSalt _salt PhoneNumberStatus' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` status

instance Prelude.NFData PhoneNumberStatus where
  rnf PhoneNumberStatus' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf status
