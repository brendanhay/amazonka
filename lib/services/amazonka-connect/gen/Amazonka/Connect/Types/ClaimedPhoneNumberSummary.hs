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
-- Module      : Amazonka.Connect.Types.ClaimedPhoneNumberSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.ClaimedPhoneNumberSummary where

import Amazonka.Connect.Types.PhoneNumberCountryCode
import Amazonka.Connect.Types.PhoneNumberStatus
import Amazonka.Connect.Types.PhoneNumberType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a phone number that has been claimed to your Amazon
-- Connect instance or traffic distribution group.
--
-- /See:/ 'newClaimedPhoneNumberSummary' smart constructor.
data ClaimedPhoneNumberSummary = ClaimedPhoneNumberSummary'
  { -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ISO country code.
    phoneNumberCountryCode :: Prelude.Maybe PhoneNumberCountryCode,
    -- | The Amazon Resource Name (ARN) of the phone number.
    phoneNumberArn :: Prelude.Maybe Prelude.Text,
    -- | The type of phone number.
    phoneNumberType :: Prelude.Maybe PhoneNumberType,
    -- | The description of the phone number.
    phoneNumberDescription :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for Amazon Connect instances or traffic
    -- distribution groups that phone numbers are claimed to.
    targetArn :: Prelude.Maybe Prelude.Text,
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
    -- You will not be billed for the phone number during the 1-day period if
    -- number claiming fails.
    phoneNumberStatus :: Prelude.Maybe PhoneNumberStatus,
    -- | A unique identifier for the phone number.
    phoneNumberId :: Prelude.Maybe Prelude.Text,
    -- | The phone number. Phone numbers are formatted
    -- @[+] [country code] [subscriber number including area code]@.
    phoneNumber :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClaimedPhoneNumberSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'claimedPhoneNumberSummary_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
--
-- 'phoneNumberCountryCode', 'claimedPhoneNumberSummary_phoneNumberCountryCode' - The ISO country code.
--
-- 'phoneNumberArn', 'claimedPhoneNumberSummary_phoneNumberArn' - The Amazon Resource Name (ARN) of the phone number.
--
-- 'phoneNumberType', 'claimedPhoneNumberSummary_phoneNumberType' - The type of phone number.
--
-- 'phoneNumberDescription', 'claimedPhoneNumberSummary_phoneNumberDescription' - The description of the phone number.
--
-- 'targetArn', 'claimedPhoneNumberSummary_targetArn' - The Amazon Resource Name (ARN) for Amazon Connect instances or traffic
-- distribution groups that phone numbers are claimed to.
--
-- 'phoneNumberStatus', 'claimedPhoneNumberSummary_phoneNumberStatus' - The status of the phone number.
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
-- You will not be billed for the phone number during the 1-day period if
-- number claiming fails.
--
-- 'phoneNumberId', 'claimedPhoneNumberSummary_phoneNumberId' - A unique identifier for the phone number.
--
-- 'phoneNumber', 'claimedPhoneNumberSummary_phoneNumber' - The phone number. Phone numbers are formatted
-- @[+] [country code] [subscriber number including area code]@.
newClaimedPhoneNumberSummary ::
  ClaimedPhoneNumberSummary
newClaimedPhoneNumberSummary =
  ClaimedPhoneNumberSummary'
    { tags = Prelude.Nothing,
      phoneNumberCountryCode = Prelude.Nothing,
      phoneNumberArn = Prelude.Nothing,
      phoneNumberType = Prelude.Nothing,
      phoneNumberDescription = Prelude.Nothing,
      targetArn = Prelude.Nothing,
      phoneNumberStatus = Prelude.Nothing,
      phoneNumberId = Prelude.Nothing,
      phoneNumber = Prelude.Nothing
    }

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
claimedPhoneNumberSummary_tags :: Lens.Lens' ClaimedPhoneNumberSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
claimedPhoneNumberSummary_tags = Lens.lens (\ClaimedPhoneNumberSummary' {tags} -> tags) (\s@ClaimedPhoneNumberSummary' {} a -> s {tags = a} :: ClaimedPhoneNumberSummary) Prelude.. Lens.mapping Lens.coerced

-- | The ISO country code.
claimedPhoneNumberSummary_phoneNumberCountryCode :: Lens.Lens' ClaimedPhoneNumberSummary (Prelude.Maybe PhoneNumberCountryCode)
claimedPhoneNumberSummary_phoneNumberCountryCode = Lens.lens (\ClaimedPhoneNumberSummary' {phoneNumberCountryCode} -> phoneNumberCountryCode) (\s@ClaimedPhoneNumberSummary' {} a -> s {phoneNumberCountryCode = a} :: ClaimedPhoneNumberSummary)

-- | The Amazon Resource Name (ARN) of the phone number.
claimedPhoneNumberSummary_phoneNumberArn :: Lens.Lens' ClaimedPhoneNumberSummary (Prelude.Maybe Prelude.Text)
claimedPhoneNumberSummary_phoneNumberArn = Lens.lens (\ClaimedPhoneNumberSummary' {phoneNumberArn} -> phoneNumberArn) (\s@ClaimedPhoneNumberSummary' {} a -> s {phoneNumberArn = a} :: ClaimedPhoneNumberSummary)

-- | The type of phone number.
claimedPhoneNumberSummary_phoneNumberType :: Lens.Lens' ClaimedPhoneNumberSummary (Prelude.Maybe PhoneNumberType)
claimedPhoneNumberSummary_phoneNumberType = Lens.lens (\ClaimedPhoneNumberSummary' {phoneNumberType} -> phoneNumberType) (\s@ClaimedPhoneNumberSummary' {} a -> s {phoneNumberType = a} :: ClaimedPhoneNumberSummary)

-- | The description of the phone number.
claimedPhoneNumberSummary_phoneNumberDescription :: Lens.Lens' ClaimedPhoneNumberSummary (Prelude.Maybe Prelude.Text)
claimedPhoneNumberSummary_phoneNumberDescription = Lens.lens (\ClaimedPhoneNumberSummary' {phoneNumberDescription} -> phoneNumberDescription) (\s@ClaimedPhoneNumberSummary' {} a -> s {phoneNumberDescription = a} :: ClaimedPhoneNumberSummary)

-- | The Amazon Resource Name (ARN) for Amazon Connect instances or traffic
-- distribution groups that phone numbers are claimed to.
claimedPhoneNumberSummary_targetArn :: Lens.Lens' ClaimedPhoneNumberSummary (Prelude.Maybe Prelude.Text)
claimedPhoneNumberSummary_targetArn = Lens.lens (\ClaimedPhoneNumberSummary' {targetArn} -> targetArn) (\s@ClaimedPhoneNumberSummary' {} a -> s {targetArn = a} :: ClaimedPhoneNumberSummary)

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
-- You will not be billed for the phone number during the 1-day period if
-- number claiming fails.
claimedPhoneNumberSummary_phoneNumberStatus :: Lens.Lens' ClaimedPhoneNumberSummary (Prelude.Maybe PhoneNumberStatus)
claimedPhoneNumberSummary_phoneNumberStatus = Lens.lens (\ClaimedPhoneNumberSummary' {phoneNumberStatus} -> phoneNumberStatus) (\s@ClaimedPhoneNumberSummary' {} a -> s {phoneNumberStatus = a} :: ClaimedPhoneNumberSummary)

-- | A unique identifier for the phone number.
claimedPhoneNumberSummary_phoneNumberId :: Lens.Lens' ClaimedPhoneNumberSummary (Prelude.Maybe Prelude.Text)
claimedPhoneNumberSummary_phoneNumberId = Lens.lens (\ClaimedPhoneNumberSummary' {phoneNumberId} -> phoneNumberId) (\s@ClaimedPhoneNumberSummary' {} a -> s {phoneNumberId = a} :: ClaimedPhoneNumberSummary)

-- | The phone number. Phone numbers are formatted
-- @[+] [country code] [subscriber number including area code]@.
claimedPhoneNumberSummary_phoneNumber :: Lens.Lens' ClaimedPhoneNumberSummary (Prelude.Maybe Prelude.Text)
claimedPhoneNumberSummary_phoneNumber = Lens.lens (\ClaimedPhoneNumberSummary' {phoneNumber} -> phoneNumber) (\s@ClaimedPhoneNumberSummary' {} a -> s {phoneNumber = a} :: ClaimedPhoneNumberSummary)

instance Data.FromJSON ClaimedPhoneNumberSummary where
  parseJSON =
    Data.withObject
      "ClaimedPhoneNumberSummary"
      ( \x ->
          ClaimedPhoneNumberSummary'
            Prelude.<$> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "PhoneNumberCountryCode")
            Prelude.<*> (x Data..:? "PhoneNumberArn")
            Prelude.<*> (x Data..:? "PhoneNumberType")
            Prelude.<*> (x Data..:? "PhoneNumberDescription")
            Prelude.<*> (x Data..:? "TargetArn")
            Prelude.<*> (x Data..:? "PhoneNumberStatus")
            Prelude.<*> (x Data..:? "PhoneNumberId")
            Prelude.<*> (x Data..:? "PhoneNumber")
      )

instance Prelude.Hashable ClaimedPhoneNumberSummary where
  hashWithSalt _salt ClaimedPhoneNumberSummary' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` phoneNumberCountryCode
      `Prelude.hashWithSalt` phoneNumberArn
      `Prelude.hashWithSalt` phoneNumberType
      `Prelude.hashWithSalt` phoneNumberDescription
      `Prelude.hashWithSalt` targetArn
      `Prelude.hashWithSalt` phoneNumberStatus
      `Prelude.hashWithSalt` phoneNumberId
      `Prelude.hashWithSalt` phoneNumber

instance Prelude.NFData ClaimedPhoneNumberSummary where
  rnf ClaimedPhoneNumberSummary' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf phoneNumberCountryCode
      `Prelude.seq` Prelude.rnf phoneNumberArn
      `Prelude.seq` Prelude.rnf phoneNumberType
      `Prelude.seq` Prelude.rnf phoneNumberDescription
      `Prelude.seq` Prelude.rnf targetArn
      `Prelude.seq` Prelude.rnf phoneNumberStatus
      `Prelude.seq` Prelude.rnf phoneNumberId
      `Prelude.seq` Prelude.rnf phoneNumber
