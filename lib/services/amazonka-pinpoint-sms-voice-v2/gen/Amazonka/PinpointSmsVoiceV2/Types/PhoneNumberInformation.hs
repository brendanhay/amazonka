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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.PhoneNumberInformation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.PhoneNumberInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types.MessageType
import Amazonka.PinpointSmsVoiceV2.Types.NumberCapability
import Amazonka.PinpointSmsVoiceV2.Types.NumberStatus
import Amazonka.PinpointSmsVoiceV2.Types.NumberType
import qualified Amazonka.Prelude as Prelude

-- | The information for a phone number in an Amazon Web Services account.
--
-- /See:/ 'newPhoneNumberInformation' smart constructor.
data PhoneNumberInformation = PhoneNumberInformation'
  { -- | The unique identifier for the phone number.
    phoneNumberId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the pool associated with the phone number.
    poolId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the two way channel.
    twoWayChannelArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) associated with the phone number.
    phoneNumberArn :: Prelude.Text,
    -- | The phone number in E.164 format.
    phoneNumber :: Prelude.Text,
    -- | The current status of the phone number.
    status :: NumberStatus,
    -- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
    -- region.
    isoCountryCode :: Prelude.Text,
    -- | The type of message. Valid values are TRANSACTIONAL for messages that
    -- are critical or time-sensitive and PROMOTIONAL for messages that aren\'t
    -- critical or time-sensitive.
    messageType :: MessageType,
    -- | Describes if the origination identity can be used for text messages,
    -- voice calls or both.
    numberCapabilities :: Prelude.NonEmpty NumberCapability,
    -- | The type of phone number.
    numberType :: NumberType,
    -- | The price, in US dollars, to lease the phone number.
    monthlyLeasingPrice :: Prelude.Text,
    -- | By default this is set to false. When set to true you can receive
    -- incoming text messages from your end recipients using the
    -- TwoWayChannelArn.
    twoWayEnabled :: Prelude.Bool,
    -- | When set to false an end recipient sends a message that begins with HELP
    -- or STOP to one of your dedicated numbers, Amazon Pinpoint automatically
    -- replies with a customizable message and adds the end recipient to the
    -- OptOutList. When set to true you\'re responsible for responding to HELP
    -- and STOP requests. You\'re also responsible for tracking and honoring
    -- opt-out request. For more information see
    -- <https://docs.aws.amazon.com/pinpoint/latest/userguide/settings-sms-managing.html#settings-account-sms-self-managed-opt-out Self-managed opt-outs>
    selfManagedOptOutsEnabled :: Prelude.Bool,
    -- | The name of the OptOutList associated with the phone number.
    optOutListName :: Prelude.Text,
    -- | When set to true the phone number can\'t be deleted.
    deletionProtectionEnabled :: Prelude.Bool,
    -- | The time when the phone number was created, in
    -- <https://www.epochconverter.com/ UNIX epoch time> format.
    createdTimestamp :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PhoneNumberInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumberId', 'phoneNumberInformation_phoneNumberId' - The unique identifier for the phone number.
--
-- 'poolId', 'phoneNumberInformation_poolId' - The unique identifier of the pool associated with the phone number.
--
-- 'twoWayChannelArn', 'phoneNumberInformation_twoWayChannelArn' - The Amazon Resource Name (ARN) of the two way channel.
--
-- 'phoneNumberArn', 'phoneNumberInformation_phoneNumberArn' - The Amazon Resource Name (ARN) associated with the phone number.
--
-- 'phoneNumber', 'phoneNumberInformation_phoneNumber' - The phone number in E.164 format.
--
-- 'status', 'phoneNumberInformation_status' - The current status of the phone number.
--
-- 'isoCountryCode', 'phoneNumberInformation_isoCountryCode' - The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region.
--
-- 'messageType', 'phoneNumberInformation_messageType' - The type of message. Valid values are TRANSACTIONAL for messages that
-- are critical or time-sensitive and PROMOTIONAL for messages that aren\'t
-- critical or time-sensitive.
--
-- 'numberCapabilities', 'phoneNumberInformation_numberCapabilities' - Describes if the origination identity can be used for text messages,
-- voice calls or both.
--
-- 'numberType', 'phoneNumberInformation_numberType' - The type of phone number.
--
-- 'monthlyLeasingPrice', 'phoneNumberInformation_monthlyLeasingPrice' - The price, in US dollars, to lease the phone number.
--
-- 'twoWayEnabled', 'phoneNumberInformation_twoWayEnabled' - By default this is set to false. When set to true you can receive
-- incoming text messages from your end recipients using the
-- TwoWayChannelArn.
--
-- 'selfManagedOptOutsEnabled', 'phoneNumberInformation_selfManagedOptOutsEnabled' - When set to false an end recipient sends a message that begins with HELP
-- or STOP to one of your dedicated numbers, Amazon Pinpoint automatically
-- replies with a customizable message and adds the end recipient to the
-- OptOutList. When set to true you\'re responsible for responding to HELP
-- and STOP requests. You\'re also responsible for tracking and honoring
-- opt-out request. For more information see
-- <https://docs.aws.amazon.com/pinpoint/latest/userguide/settings-sms-managing.html#settings-account-sms-self-managed-opt-out Self-managed opt-outs>
--
-- 'optOutListName', 'phoneNumberInformation_optOutListName' - The name of the OptOutList associated with the phone number.
--
-- 'deletionProtectionEnabled', 'phoneNumberInformation_deletionProtectionEnabled' - When set to true the phone number can\'t be deleted.
--
-- 'createdTimestamp', 'phoneNumberInformation_createdTimestamp' - The time when the phone number was created, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
newPhoneNumberInformation ::
  -- | 'phoneNumberArn'
  Prelude.Text ->
  -- | 'phoneNumber'
  Prelude.Text ->
  -- | 'status'
  NumberStatus ->
  -- | 'isoCountryCode'
  Prelude.Text ->
  -- | 'messageType'
  MessageType ->
  -- | 'numberCapabilities'
  Prelude.NonEmpty NumberCapability ->
  -- | 'numberType'
  NumberType ->
  -- | 'monthlyLeasingPrice'
  Prelude.Text ->
  -- | 'twoWayEnabled'
  Prelude.Bool ->
  -- | 'selfManagedOptOutsEnabled'
  Prelude.Bool ->
  -- | 'optOutListName'
  Prelude.Text ->
  -- | 'deletionProtectionEnabled'
  Prelude.Bool ->
  -- | 'createdTimestamp'
  Prelude.UTCTime ->
  PhoneNumberInformation
newPhoneNumberInformation
  pPhoneNumberArn_
  pPhoneNumber_
  pStatus_
  pIsoCountryCode_
  pMessageType_
  pNumberCapabilities_
  pNumberType_
  pMonthlyLeasingPrice_
  pTwoWayEnabled_
  pSelfManagedOptOutsEnabled_
  pOptOutListName_
  pDeletionProtectionEnabled_
  pCreatedTimestamp_ =
    PhoneNumberInformation'
      { phoneNumberId =
          Prelude.Nothing,
        poolId = Prelude.Nothing,
        twoWayChannelArn = Prelude.Nothing,
        phoneNumberArn = pPhoneNumberArn_,
        phoneNumber = pPhoneNumber_,
        status = pStatus_,
        isoCountryCode = pIsoCountryCode_,
        messageType = pMessageType_,
        numberCapabilities =
          Lens.coerced Lens.# pNumberCapabilities_,
        numberType = pNumberType_,
        monthlyLeasingPrice = pMonthlyLeasingPrice_,
        twoWayEnabled = pTwoWayEnabled_,
        selfManagedOptOutsEnabled =
          pSelfManagedOptOutsEnabled_,
        optOutListName = pOptOutListName_,
        deletionProtectionEnabled =
          pDeletionProtectionEnabled_,
        createdTimestamp =
          Data._Time Lens.# pCreatedTimestamp_
      }

-- | The unique identifier for the phone number.
phoneNumberInformation_phoneNumberId :: Lens.Lens' PhoneNumberInformation (Prelude.Maybe Prelude.Text)
phoneNumberInformation_phoneNumberId = Lens.lens (\PhoneNumberInformation' {phoneNumberId} -> phoneNumberId) (\s@PhoneNumberInformation' {} a -> s {phoneNumberId = a} :: PhoneNumberInformation)

-- | The unique identifier of the pool associated with the phone number.
phoneNumberInformation_poolId :: Lens.Lens' PhoneNumberInformation (Prelude.Maybe Prelude.Text)
phoneNumberInformation_poolId = Lens.lens (\PhoneNumberInformation' {poolId} -> poolId) (\s@PhoneNumberInformation' {} a -> s {poolId = a} :: PhoneNumberInformation)

-- | The Amazon Resource Name (ARN) of the two way channel.
phoneNumberInformation_twoWayChannelArn :: Lens.Lens' PhoneNumberInformation (Prelude.Maybe Prelude.Text)
phoneNumberInformation_twoWayChannelArn = Lens.lens (\PhoneNumberInformation' {twoWayChannelArn} -> twoWayChannelArn) (\s@PhoneNumberInformation' {} a -> s {twoWayChannelArn = a} :: PhoneNumberInformation)

-- | The Amazon Resource Name (ARN) associated with the phone number.
phoneNumberInformation_phoneNumberArn :: Lens.Lens' PhoneNumberInformation Prelude.Text
phoneNumberInformation_phoneNumberArn = Lens.lens (\PhoneNumberInformation' {phoneNumberArn} -> phoneNumberArn) (\s@PhoneNumberInformation' {} a -> s {phoneNumberArn = a} :: PhoneNumberInformation)

-- | The phone number in E.164 format.
phoneNumberInformation_phoneNumber :: Lens.Lens' PhoneNumberInformation Prelude.Text
phoneNumberInformation_phoneNumber = Lens.lens (\PhoneNumberInformation' {phoneNumber} -> phoneNumber) (\s@PhoneNumberInformation' {} a -> s {phoneNumber = a} :: PhoneNumberInformation)

-- | The current status of the phone number.
phoneNumberInformation_status :: Lens.Lens' PhoneNumberInformation NumberStatus
phoneNumberInformation_status = Lens.lens (\PhoneNumberInformation' {status} -> status) (\s@PhoneNumberInformation' {} a -> s {status = a} :: PhoneNumberInformation)

-- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region.
phoneNumberInformation_isoCountryCode :: Lens.Lens' PhoneNumberInformation Prelude.Text
phoneNumberInformation_isoCountryCode = Lens.lens (\PhoneNumberInformation' {isoCountryCode} -> isoCountryCode) (\s@PhoneNumberInformation' {} a -> s {isoCountryCode = a} :: PhoneNumberInformation)

-- | The type of message. Valid values are TRANSACTIONAL for messages that
-- are critical or time-sensitive and PROMOTIONAL for messages that aren\'t
-- critical or time-sensitive.
phoneNumberInformation_messageType :: Lens.Lens' PhoneNumberInformation MessageType
phoneNumberInformation_messageType = Lens.lens (\PhoneNumberInformation' {messageType} -> messageType) (\s@PhoneNumberInformation' {} a -> s {messageType = a} :: PhoneNumberInformation)

-- | Describes if the origination identity can be used for text messages,
-- voice calls or both.
phoneNumberInformation_numberCapabilities :: Lens.Lens' PhoneNumberInformation (Prelude.NonEmpty NumberCapability)
phoneNumberInformation_numberCapabilities = Lens.lens (\PhoneNumberInformation' {numberCapabilities} -> numberCapabilities) (\s@PhoneNumberInformation' {} a -> s {numberCapabilities = a} :: PhoneNumberInformation) Prelude.. Lens.coerced

-- | The type of phone number.
phoneNumberInformation_numberType :: Lens.Lens' PhoneNumberInformation NumberType
phoneNumberInformation_numberType = Lens.lens (\PhoneNumberInformation' {numberType} -> numberType) (\s@PhoneNumberInformation' {} a -> s {numberType = a} :: PhoneNumberInformation)

-- | The price, in US dollars, to lease the phone number.
phoneNumberInformation_monthlyLeasingPrice :: Lens.Lens' PhoneNumberInformation Prelude.Text
phoneNumberInformation_monthlyLeasingPrice = Lens.lens (\PhoneNumberInformation' {monthlyLeasingPrice} -> monthlyLeasingPrice) (\s@PhoneNumberInformation' {} a -> s {monthlyLeasingPrice = a} :: PhoneNumberInformation)

-- | By default this is set to false. When set to true you can receive
-- incoming text messages from your end recipients using the
-- TwoWayChannelArn.
phoneNumberInformation_twoWayEnabled :: Lens.Lens' PhoneNumberInformation Prelude.Bool
phoneNumberInformation_twoWayEnabled = Lens.lens (\PhoneNumberInformation' {twoWayEnabled} -> twoWayEnabled) (\s@PhoneNumberInformation' {} a -> s {twoWayEnabled = a} :: PhoneNumberInformation)

-- | When set to false an end recipient sends a message that begins with HELP
-- or STOP to one of your dedicated numbers, Amazon Pinpoint automatically
-- replies with a customizable message and adds the end recipient to the
-- OptOutList. When set to true you\'re responsible for responding to HELP
-- and STOP requests. You\'re also responsible for tracking and honoring
-- opt-out request. For more information see
-- <https://docs.aws.amazon.com/pinpoint/latest/userguide/settings-sms-managing.html#settings-account-sms-self-managed-opt-out Self-managed opt-outs>
phoneNumberInformation_selfManagedOptOutsEnabled :: Lens.Lens' PhoneNumberInformation Prelude.Bool
phoneNumberInformation_selfManagedOptOutsEnabled = Lens.lens (\PhoneNumberInformation' {selfManagedOptOutsEnabled} -> selfManagedOptOutsEnabled) (\s@PhoneNumberInformation' {} a -> s {selfManagedOptOutsEnabled = a} :: PhoneNumberInformation)

-- | The name of the OptOutList associated with the phone number.
phoneNumberInformation_optOutListName :: Lens.Lens' PhoneNumberInformation Prelude.Text
phoneNumberInformation_optOutListName = Lens.lens (\PhoneNumberInformation' {optOutListName} -> optOutListName) (\s@PhoneNumberInformation' {} a -> s {optOutListName = a} :: PhoneNumberInformation)

-- | When set to true the phone number can\'t be deleted.
phoneNumberInformation_deletionProtectionEnabled :: Lens.Lens' PhoneNumberInformation Prelude.Bool
phoneNumberInformation_deletionProtectionEnabled = Lens.lens (\PhoneNumberInformation' {deletionProtectionEnabled} -> deletionProtectionEnabled) (\s@PhoneNumberInformation' {} a -> s {deletionProtectionEnabled = a} :: PhoneNumberInformation)

-- | The time when the phone number was created, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
phoneNumberInformation_createdTimestamp :: Lens.Lens' PhoneNumberInformation Prelude.UTCTime
phoneNumberInformation_createdTimestamp = Lens.lens (\PhoneNumberInformation' {createdTimestamp} -> createdTimestamp) (\s@PhoneNumberInformation' {} a -> s {createdTimestamp = a} :: PhoneNumberInformation) Prelude.. Data._Time

instance Data.FromJSON PhoneNumberInformation where
  parseJSON =
    Data.withObject
      "PhoneNumberInformation"
      ( \x ->
          PhoneNumberInformation'
            Prelude.<$> (x Data..:? "PhoneNumberId")
            Prelude.<*> (x Data..:? "PoolId")
            Prelude.<*> (x Data..:? "TwoWayChannelArn")
            Prelude.<*> (x Data..: "PhoneNumberArn")
            Prelude.<*> (x Data..: "PhoneNumber")
            Prelude.<*> (x Data..: "Status")
            Prelude.<*> (x Data..: "IsoCountryCode")
            Prelude.<*> (x Data..: "MessageType")
            Prelude.<*> (x Data..: "NumberCapabilities")
            Prelude.<*> (x Data..: "NumberType")
            Prelude.<*> (x Data..: "MonthlyLeasingPrice")
            Prelude.<*> (x Data..: "TwoWayEnabled")
            Prelude.<*> (x Data..: "SelfManagedOptOutsEnabled")
            Prelude.<*> (x Data..: "OptOutListName")
            Prelude.<*> (x Data..: "DeletionProtectionEnabled")
            Prelude.<*> (x Data..: "CreatedTimestamp")
      )

instance Prelude.Hashable PhoneNumberInformation where
  hashWithSalt _salt PhoneNumberInformation' {..} =
    _salt
      `Prelude.hashWithSalt` phoneNumberId
      `Prelude.hashWithSalt` poolId
      `Prelude.hashWithSalt` twoWayChannelArn
      `Prelude.hashWithSalt` phoneNumberArn
      `Prelude.hashWithSalt` phoneNumber
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` isoCountryCode
      `Prelude.hashWithSalt` messageType
      `Prelude.hashWithSalt` numberCapabilities
      `Prelude.hashWithSalt` numberType
      `Prelude.hashWithSalt` monthlyLeasingPrice
      `Prelude.hashWithSalt` twoWayEnabled
      `Prelude.hashWithSalt` selfManagedOptOutsEnabled
      `Prelude.hashWithSalt` optOutListName
      `Prelude.hashWithSalt` deletionProtectionEnabled
      `Prelude.hashWithSalt` createdTimestamp

instance Prelude.NFData PhoneNumberInformation where
  rnf PhoneNumberInformation' {..} =
    Prelude.rnf phoneNumberId
      `Prelude.seq` Prelude.rnf poolId
      `Prelude.seq` Prelude.rnf twoWayChannelArn
      `Prelude.seq` Prelude.rnf phoneNumberArn
      `Prelude.seq` Prelude.rnf phoneNumber
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf isoCountryCode
      `Prelude.seq` Prelude.rnf messageType
      `Prelude.seq` Prelude.rnf numberCapabilities
      `Prelude.seq` Prelude.rnf numberType
      `Prelude.seq` Prelude.rnf monthlyLeasingPrice
      `Prelude.seq` Prelude.rnf twoWayEnabled
      `Prelude.seq` Prelude.rnf selfManagedOptOutsEnabled
      `Prelude.seq` Prelude.rnf optOutListName
      `Prelude.seq` Prelude.rnf deletionProtectionEnabled
      `Prelude.seq` Prelude.rnf createdTimestamp
