{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.PinpointSmsVoiceV2.UpdatePhoneNumber
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration of an existing origination phone number. You
-- can update the opt-out list, enable or disable two-way messaging, change
-- the TwoWayChannelArn, enable or disable self-managed opt-outs, and
-- enable or disable deletion protection.
--
-- If the origination phone number is associated with a pool, an Error is
-- returned.
module Amazonka.PinpointSmsVoiceV2.UpdatePhoneNumber
  ( -- * Creating a Request
    UpdatePhoneNumber (..),
    newUpdatePhoneNumber,

    -- * Request Lenses
    updatePhoneNumber_deletionProtectionEnabled,
    updatePhoneNumber_optOutListName,
    updatePhoneNumber_selfManagedOptOutsEnabled,
    updatePhoneNumber_twoWayChannelArn,
    updatePhoneNumber_twoWayEnabled,
    updatePhoneNumber_phoneNumberId,

    -- * Destructuring the Response
    UpdatePhoneNumberResponse (..),
    newUpdatePhoneNumberResponse,

    -- * Response Lenses
    updatePhoneNumberResponse_createdTimestamp,
    updatePhoneNumberResponse_deletionProtectionEnabled,
    updatePhoneNumberResponse_isoCountryCode,
    updatePhoneNumberResponse_messageType,
    updatePhoneNumberResponse_monthlyLeasingPrice,
    updatePhoneNumberResponse_numberCapabilities,
    updatePhoneNumberResponse_numberType,
    updatePhoneNumberResponse_optOutListName,
    updatePhoneNumberResponse_phoneNumber,
    updatePhoneNumberResponse_phoneNumberArn,
    updatePhoneNumberResponse_phoneNumberId,
    updatePhoneNumberResponse_selfManagedOptOutsEnabled,
    updatePhoneNumberResponse_status,
    updatePhoneNumberResponse_twoWayChannelArn,
    updatePhoneNumberResponse_twoWayEnabled,
    updatePhoneNumberResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePhoneNumber' smart constructor.
data UpdatePhoneNumber = UpdatePhoneNumber'
  { -- | By default this is set to false. When set to true the phone number
    -- can\'t be deleted.
    deletionProtectionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The OptOutList to add the phone number to. Valid values for this field
    -- can be either the OutOutListName or OutOutListArn.
    optOutListName :: Prelude.Maybe Prelude.Text,
    -- | By default this is set to false. When an end recipient sends a message
    -- that begins with HELP or STOP to one of your dedicated numbers, Amazon
    -- Pinpoint automatically replies with a customizable message and adds the
    -- end recipient to the OptOutList. When set to true you\'re responsible
    -- for responding to HELP and STOP requests. You\'re also responsible for
    -- tracking and honoring opt-out requests.
    selfManagedOptOutsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the two way channel.
    twoWayChannelArn :: Prelude.Maybe Prelude.Text,
    -- | By default this is set to false. When set to true you can receive
    -- incoming text messages from your end recipients.
    twoWayEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The unique identifier of the phone number. Valid values for this field
    -- can be either the PhoneNumberId or PhoneNumberArn.
    phoneNumberId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePhoneNumber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionProtectionEnabled', 'updatePhoneNumber_deletionProtectionEnabled' - By default this is set to false. When set to true the phone number
-- can\'t be deleted.
--
-- 'optOutListName', 'updatePhoneNumber_optOutListName' - The OptOutList to add the phone number to. Valid values for this field
-- can be either the OutOutListName or OutOutListArn.
--
-- 'selfManagedOptOutsEnabled', 'updatePhoneNumber_selfManagedOptOutsEnabled' - By default this is set to false. When an end recipient sends a message
-- that begins with HELP or STOP to one of your dedicated numbers, Amazon
-- Pinpoint automatically replies with a customizable message and adds the
-- end recipient to the OptOutList. When set to true you\'re responsible
-- for responding to HELP and STOP requests. You\'re also responsible for
-- tracking and honoring opt-out requests.
--
-- 'twoWayChannelArn', 'updatePhoneNumber_twoWayChannelArn' - The Amazon Resource Name (ARN) of the two way channel.
--
-- 'twoWayEnabled', 'updatePhoneNumber_twoWayEnabled' - By default this is set to false. When set to true you can receive
-- incoming text messages from your end recipients.
--
-- 'phoneNumberId', 'updatePhoneNumber_phoneNumberId' - The unique identifier of the phone number. Valid values for this field
-- can be either the PhoneNumberId or PhoneNumberArn.
newUpdatePhoneNumber ::
  -- | 'phoneNumberId'
  Prelude.Text ->
  UpdatePhoneNumber
newUpdatePhoneNumber pPhoneNumberId_ =
  UpdatePhoneNumber'
    { deletionProtectionEnabled =
        Prelude.Nothing,
      optOutListName = Prelude.Nothing,
      selfManagedOptOutsEnabled = Prelude.Nothing,
      twoWayChannelArn = Prelude.Nothing,
      twoWayEnabled = Prelude.Nothing,
      phoneNumberId = pPhoneNumberId_
    }

-- | By default this is set to false. When set to true the phone number
-- can\'t be deleted.
updatePhoneNumber_deletionProtectionEnabled :: Lens.Lens' UpdatePhoneNumber (Prelude.Maybe Prelude.Bool)
updatePhoneNumber_deletionProtectionEnabled = Lens.lens (\UpdatePhoneNumber' {deletionProtectionEnabled} -> deletionProtectionEnabled) (\s@UpdatePhoneNumber' {} a -> s {deletionProtectionEnabled = a} :: UpdatePhoneNumber)

-- | The OptOutList to add the phone number to. Valid values for this field
-- can be either the OutOutListName or OutOutListArn.
updatePhoneNumber_optOutListName :: Lens.Lens' UpdatePhoneNumber (Prelude.Maybe Prelude.Text)
updatePhoneNumber_optOutListName = Lens.lens (\UpdatePhoneNumber' {optOutListName} -> optOutListName) (\s@UpdatePhoneNumber' {} a -> s {optOutListName = a} :: UpdatePhoneNumber)

-- | By default this is set to false. When an end recipient sends a message
-- that begins with HELP or STOP to one of your dedicated numbers, Amazon
-- Pinpoint automatically replies with a customizable message and adds the
-- end recipient to the OptOutList. When set to true you\'re responsible
-- for responding to HELP and STOP requests. You\'re also responsible for
-- tracking and honoring opt-out requests.
updatePhoneNumber_selfManagedOptOutsEnabled :: Lens.Lens' UpdatePhoneNumber (Prelude.Maybe Prelude.Bool)
updatePhoneNumber_selfManagedOptOutsEnabled = Lens.lens (\UpdatePhoneNumber' {selfManagedOptOutsEnabled} -> selfManagedOptOutsEnabled) (\s@UpdatePhoneNumber' {} a -> s {selfManagedOptOutsEnabled = a} :: UpdatePhoneNumber)

-- | The Amazon Resource Name (ARN) of the two way channel.
updatePhoneNumber_twoWayChannelArn :: Lens.Lens' UpdatePhoneNumber (Prelude.Maybe Prelude.Text)
updatePhoneNumber_twoWayChannelArn = Lens.lens (\UpdatePhoneNumber' {twoWayChannelArn} -> twoWayChannelArn) (\s@UpdatePhoneNumber' {} a -> s {twoWayChannelArn = a} :: UpdatePhoneNumber)

-- | By default this is set to false. When set to true you can receive
-- incoming text messages from your end recipients.
updatePhoneNumber_twoWayEnabled :: Lens.Lens' UpdatePhoneNumber (Prelude.Maybe Prelude.Bool)
updatePhoneNumber_twoWayEnabled = Lens.lens (\UpdatePhoneNumber' {twoWayEnabled} -> twoWayEnabled) (\s@UpdatePhoneNumber' {} a -> s {twoWayEnabled = a} :: UpdatePhoneNumber)

-- | The unique identifier of the phone number. Valid values for this field
-- can be either the PhoneNumberId or PhoneNumberArn.
updatePhoneNumber_phoneNumberId :: Lens.Lens' UpdatePhoneNumber Prelude.Text
updatePhoneNumber_phoneNumberId = Lens.lens (\UpdatePhoneNumber' {phoneNumberId} -> phoneNumberId) (\s@UpdatePhoneNumber' {} a -> s {phoneNumberId = a} :: UpdatePhoneNumber)

instance Core.AWSRequest UpdatePhoneNumber where
  type
    AWSResponse UpdatePhoneNumber =
      UpdatePhoneNumberResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePhoneNumberResponse'
            Prelude.<$> (x Data..?> "CreatedTimestamp")
            Prelude.<*> (x Data..?> "DeletionProtectionEnabled")
            Prelude.<*> (x Data..?> "IsoCountryCode")
            Prelude.<*> (x Data..?> "MessageType")
            Prelude.<*> (x Data..?> "MonthlyLeasingPrice")
            Prelude.<*> (x Data..?> "NumberCapabilities")
            Prelude.<*> (x Data..?> "NumberType")
            Prelude.<*> (x Data..?> "OptOutListName")
            Prelude.<*> (x Data..?> "PhoneNumber")
            Prelude.<*> (x Data..?> "PhoneNumberArn")
            Prelude.<*> (x Data..?> "PhoneNumberId")
            Prelude.<*> (x Data..?> "SelfManagedOptOutsEnabled")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "TwoWayChannelArn")
            Prelude.<*> (x Data..?> "TwoWayEnabled")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePhoneNumber where
  hashWithSalt _salt UpdatePhoneNumber' {..} =
    _salt
      `Prelude.hashWithSalt` deletionProtectionEnabled
      `Prelude.hashWithSalt` optOutListName
      `Prelude.hashWithSalt` selfManagedOptOutsEnabled
      `Prelude.hashWithSalt` twoWayChannelArn
      `Prelude.hashWithSalt` twoWayEnabled
      `Prelude.hashWithSalt` phoneNumberId

instance Prelude.NFData UpdatePhoneNumber where
  rnf UpdatePhoneNumber' {..} =
    Prelude.rnf deletionProtectionEnabled
      `Prelude.seq` Prelude.rnf optOutListName
      `Prelude.seq` Prelude.rnf selfManagedOptOutsEnabled
      `Prelude.seq` Prelude.rnf twoWayChannelArn
      `Prelude.seq` Prelude.rnf twoWayEnabled
      `Prelude.seq` Prelude.rnf phoneNumberId

instance Data.ToHeaders UpdatePhoneNumber where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.UpdatePhoneNumber" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePhoneNumber where
  toJSON UpdatePhoneNumber' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeletionProtectionEnabled" Data..=)
              Prelude.<$> deletionProtectionEnabled,
            ("OptOutListName" Data..=)
              Prelude.<$> optOutListName,
            ("SelfManagedOptOutsEnabled" Data..=)
              Prelude.<$> selfManagedOptOutsEnabled,
            ("TwoWayChannelArn" Data..=)
              Prelude.<$> twoWayChannelArn,
            ("TwoWayEnabled" Data..=) Prelude.<$> twoWayEnabled,
            Prelude.Just
              ("PhoneNumberId" Data..= phoneNumberId)
          ]
      )

instance Data.ToPath UpdatePhoneNumber where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdatePhoneNumber where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePhoneNumberResponse' smart constructor.
data UpdatePhoneNumberResponse = UpdatePhoneNumberResponse'
  { -- | The time when the phone number was created, in
    -- <https://www.epochconverter.com/ UNIX epoch time> format.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | When set to true the phone number can\'t be deleted.
    deletionProtectionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
    -- region.
    isoCountryCode :: Prelude.Maybe Prelude.Text,
    -- | The type of message. Valid values are TRANSACTIONAL for messages that
    -- are critical or time-sensitive and PROMOTIONAL for messages that aren\'t
    -- critical or time-sensitive.
    messageType :: Prelude.Maybe MessageType,
    -- | The monthly leasing price of the phone number, in US dollars.
    monthlyLeasingPrice :: Prelude.Maybe Prelude.Text,
    -- | Specifies if the number could be used for text messages, voice or both.
    numberCapabilities :: Prelude.Maybe (Prelude.NonEmpty NumberCapability),
    -- | The type of number that was requested.
    numberType :: Prelude.Maybe NumberType,
    -- | The name of the OptOutList associated with the phone number.
    optOutListName :: Prelude.Maybe Prelude.Text,
    -- | The phone number that was updated.
    phoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the updated phone number.
    phoneNumberArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the phone number.
    phoneNumberId :: Prelude.Maybe Prelude.Text,
    -- | This is true if self managed opt-out are enabled.
    selfManagedOptOutsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The current status of the request.
    status :: Prelude.Maybe NumberStatus,
    -- | The Amazon Resource Name (ARN) of the two way channel.
    twoWayChannelArn :: Prelude.Maybe Prelude.Text,
    -- | By default this is set to false. When set to true you can receive
    -- incoming text messages from your end recipients.
    twoWayEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePhoneNumberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimestamp', 'updatePhoneNumberResponse_createdTimestamp' - The time when the phone number was created, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
--
-- 'deletionProtectionEnabled', 'updatePhoneNumberResponse_deletionProtectionEnabled' - When set to true the phone number can\'t be deleted.
--
-- 'isoCountryCode', 'updatePhoneNumberResponse_isoCountryCode' - The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region.
--
-- 'messageType', 'updatePhoneNumberResponse_messageType' - The type of message. Valid values are TRANSACTIONAL for messages that
-- are critical or time-sensitive and PROMOTIONAL for messages that aren\'t
-- critical or time-sensitive.
--
-- 'monthlyLeasingPrice', 'updatePhoneNumberResponse_monthlyLeasingPrice' - The monthly leasing price of the phone number, in US dollars.
--
-- 'numberCapabilities', 'updatePhoneNumberResponse_numberCapabilities' - Specifies if the number could be used for text messages, voice or both.
--
-- 'numberType', 'updatePhoneNumberResponse_numberType' - The type of number that was requested.
--
-- 'optOutListName', 'updatePhoneNumberResponse_optOutListName' - The name of the OptOutList associated with the phone number.
--
-- 'phoneNumber', 'updatePhoneNumberResponse_phoneNumber' - The phone number that was updated.
--
-- 'phoneNumberArn', 'updatePhoneNumberResponse_phoneNumberArn' - The Amazon Resource Name (ARN) of the updated phone number.
--
-- 'phoneNumberId', 'updatePhoneNumberResponse_phoneNumberId' - The unique identifier of the phone number.
--
-- 'selfManagedOptOutsEnabled', 'updatePhoneNumberResponse_selfManagedOptOutsEnabled' - This is true if self managed opt-out are enabled.
--
-- 'status', 'updatePhoneNumberResponse_status' - The current status of the request.
--
-- 'twoWayChannelArn', 'updatePhoneNumberResponse_twoWayChannelArn' - The Amazon Resource Name (ARN) of the two way channel.
--
-- 'twoWayEnabled', 'updatePhoneNumberResponse_twoWayEnabled' - By default this is set to false. When set to true you can receive
-- incoming text messages from your end recipients.
--
-- 'httpStatus', 'updatePhoneNumberResponse_httpStatus' - The response's http status code.
newUpdatePhoneNumberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePhoneNumberResponse
newUpdatePhoneNumberResponse pHttpStatus_ =
  UpdatePhoneNumberResponse'
    { createdTimestamp =
        Prelude.Nothing,
      deletionProtectionEnabled = Prelude.Nothing,
      isoCountryCode = Prelude.Nothing,
      messageType = Prelude.Nothing,
      monthlyLeasingPrice = Prelude.Nothing,
      numberCapabilities = Prelude.Nothing,
      numberType = Prelude.Nothing,
      optOutListName = Prelude.Nothing,
      phoneNumber = Prelude.Nothing,
      phoneNumberArn = Prelude.Nothing,
      phoneNumberId = Prelude.Nothing,
      selfManagedOptOutsEnabled = Prelude.Nothing,
      status = Prelude.Nothing,
      twoWayChannelArn = Prelude.Nothing,
      twoWayEnabled = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time when the phone number was created, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
updatePhoneNumberResponse_createdTimestamp :: Lens.Lens' UpdatePhoneNumberResponse (Prelude.Maybe Prelude.UTCTime)
updatePhoneNumberResponse_createdTimestamp = Lens.lens (\UpdatePhoneNumberResponse' {createdTimestamp} -> createdTimestamp) (\s@UpdatePhoneNumberResponse' {} a -> s {createdTimestamp = a} :: UpdatePhoneNumberResponse) Prelude.. Lens.mapping Data._Time

-- | When set to true the phone number can\'t be deleted.
updatePhoneNumberResponse_deletionProtectionEnabled :: Lens.Lens' UpdatePhoneNumberResponse (Prelude.Maybe Prelude.Bool)
updatePhoneNumberResponse_deletionProtectionEnabled = Lens.lens (\UpdatePhoneNumberResponse' {deletionProtectionEnabled} -> deletionProtectionEnabled) (\s@UpdatePhoneNumberResponse' {} a -> s {deletionProtectionEnabled = a} :: UpdatePhoneNumberResponse)

-- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region.
updatePhoneNumberResponse_isoCountryCode :: Lens.Lens' UpdatePhoneNumberResponse (Prelude.Maybe Prelude.Text)
updatePhoneNumberResponse_isoCountryCode = Lens.lens (\UpdatePhoneNumberResponse' {isoCountryCode} -> isoCountryCode) (\s@UpdatePhoneNumberResponse' {} a -> s {isoCountryCode = a} :: UpdatePhoneNumberResponse)

-- | The type of message. Valid values are TRANSACTIONAL for messages that
-- are critical or time-sensitive and PROMOTIONAL for messages that aren\'t
-- critical or time-sensitive.
updatePhoneNumberResponse_messageType :: Lens.Lens' UpdatePhoneNumberResponse (Prelude.Maybe MessageType)
updatePhoneNumberResponse_messageType = Lens.lens (\UpdatePhoneNumberResponse' {messageType} -> messageType) (\s@UpdatePhoneNumberResponse' {} a -> s {messageType = a} :: UpdatePhoneNumberResponse)

-- | The monthly leasing price of the phone number, in US dollars.
updatePhoneNumberResponse_monthlyLeasingPrice :: Lens.Lens' UpdatePhoneNumberResponse (Prelude.Maybe Prelude.Text)
updatePhoneNumberResponse_monthlyLeasingPrice = Lens.lens (\UpdatePhoneNumberResponse' {monthlyLeasingPrice} -> monthlyLeasingPrice) (\s@UpdatePhoneNumberResponse' {} a -> s {monthlyLeasingPrice = a} :: UpdatePhoneNumberResponse)

-- | Specifies if the number could be used for text messages, voice or both.
updatePhoneNumberResponse_numberCapabilities :: Lens.Lens' UpdatePhoneNumberResponse (Prelude.Maybe (Prelude.NonEmpty NumberCapability))
updatePhoneNumberResponse_numberCapabilities = Lens.lens (\UpdatePhoneNumberResponse' {numberCapabilities} -> numberCapabilities) (\s@UpdatePhoneNumberResponse' {} a -> s {numberCapabilities = a} :: UpdatePhoneNumberResponse) Prelude.. Lens.mapping Lens.coerced

-- | The type of number that was requested.
updatePhoneNumberResponse_numberType :: Lens.Lens' UpdatePhoneNumberResponse (Prelude.Maybe NumberType)
updatePhoneNumberResponse_numberType = Lens.lens (\UpdatePhoneNumberResponse' {numberType} -> numberType) (\s@UpdatePhoneNumberResponse' {} a -> s {numberType = a} :: UpdatePhoneNumberResponse)

-- | The name of the OptOutList associated with the phone number.
updatePhoneNumberResponse_optOutListName :: Lens.Lens' UpdatePhoneNumberResponse (Prelude.Maybe Prelude.Text)
updatePhoneNumberResponse_optOutListName = Lens.lens (\UpdatePhoneNumberResponse' {optOutListName} -> optOutListName) (\s@UpdatePhoneNumberResponse' {} a -> s {optOutListName = a} :: UpdatePhoneNumberResponse)

-- | The phone number that was updated.
updatePhoneNumberResponse_phoneNumber :: Lens.Lens' UpdatePhoneNumberResponse (Prelude.Maybe Prelude.Text)
updatePhoneNumberResponse_phoneNumber = Lens.lens (\UpdatePhoneNumberResponse' {phoneNumber} -> phoneNumber) (\s@UpdatePhoneNumberResponse' {} a -> s {phoneNumber = a} :: UpdatePhoneNumberResponse)

-- | The Amazon Resource Name (ARN) of the updated phone number.
updatePhoneNumberResponse_phoneNumberArn :: Lens.Lens' UpdatePhoneNumberResponse (Prelude.Maybe Prelude.Text)
updatePhoneNumberResponse_phoneNumberArn = Lens.lens (\UpdatePhoneNumberResponse' {phoneNumberArn} -> phoneNumberArn) (\s@UpdatePhoneNumberResponse' {} a -> s {phoneNumberArn = a} :: UpdatePhoneNumberResponse)

-- | The unique identifier of the phone number.
updatePhoneNumberResponse_phoneNumberId :: Lens.Lens' UpdatePhoneNumberResponse (Prelude.Maybe Prelude.Text)
updatePhoneNumberResponse_phoneNumberId = Lens.lens (\UpdatePhoneNumberResponse' {phoneNumberId} -> phoneNumberId) (\s@UpdatePhoneNumberResponse' {} a -> s {phoneNumberId = a} :: UpdatePhoneNumberResponse)

-- | This is true if self managed opt-out are enabled.
updatePhoneNumberResponse_selfManagedOptOutsEnabled :: Lens.Lens' UpdatePhoneNumberResponse (Prelude.Maybe Prelude.Bool)
updatePhoneNumberResponse_selfManagedOptOutsEnabled = Lens.lens (\UpdatePhoneNumberResponse' {selfManagedOptOutsEnabled} -> selfManagedOptOutsEnabled) (\s@UpdatePhoneNumberResponse' {} a -> s {selfManagedOptOutsEnabled = a} :: UpdatePhoneNumberResponse)

-- | The current status of the request.
updatePhoneNumberResponse_status :: Lens.Lens' UpdatePhoneNumberResponse (Prelude.Maybe NumberStatus)
updatePhoneNumberResponse_status = Lens.lens (\UpdatePhoneNumberResponse' {status} -> status) (\s@UpdatePhoneNumberResponse' {} a -> s {status = a} :: UpdatePhoneNumberResponse)

-- | The Amazon Resource Name (ARN) of the two way channel.
updatePhoneNumberResponse_twoWayChannelArn :: Lens.Lens' UpdatePhoneNumberResponse (Prelude.Maybe Prelude.Text)
updatePhoneNumberResponse_twoWayChannelArn = Lens.lens (\UpdatePhoneNumberResponse' {twoWayChannelArn} -> twoWayChannelArn) (\s@UpdatePhoneNumberResponse' {} a -> s {twoWayChannelArn = a} :: UpdatePhoneNumberResponse)

-- | By default this is set to false. When set to true you can receive
-- incoming text messages from your end recipients.
updatePhoneNumberResponse_twoWayEnabled :: Lens.Lens' UpdatePhoneNumberResponse (Prelude.Maybe Prelude.Bool)
updatePhoneNumberResponse_twoWayEnabled = Lens.lens (\UpdatePhoneNumberResponse' {twoWayEnabled} -> twoWayEnabled) (\s@UpdatePhoneNumberResponse' {} a -> s {twoWayEnabled = a} :: UpdatePhoneNumberResponse)

-- | The response's http status code.
updatePhoneNumberResponse_httpStatus :: Lens.Lens' UpdatePhoneNumberResponse Prelude.Int
updatePhoneNumberResponse_httpStatus = Lens.lens (\UpdatePhoneNumberResponse' {httpStatus} -> httpStatus) (\s@UpdatePhoneNumberResponse' {} a -> s {httpStatus = a} :: UpdatePhoneNumberResponse)

instance Prelude.NFData UpdatePhoneNumberResponse where
  rnf UpdatePhoneNumberResponse' {..} =
    Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf deletionProtectionEnabled
      `Prelude.seq` Prelude.rnf isoCountryCode
      `Prelude.seq` Prelude.rnf messageType
      `Prelude.seq` Prelude.rnf monthlyLeasingPrice
      `Prelude.seq` Prelude.rnf numberCapabilities
      `Prelude.seq` Prelude.rnf numberType
      `Prelude.seq` Prelude.rnf optOutListName
      `Prelude.seq` Prelude.rnf phoneNumber
      `Prelude.seq` Prelude.rnf phoneNumberArn
      `Prelude.seq` Prelude.rnf phoneNumberId
      `Prelude.seq` Prelude.rnf selfManagedOptOutsEnabled
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf twoWayChannelArn
      `Prelude.seq` Prelude.rnf twoWayEnabled
      `Prelude.seq` Prelude.rnf httpStatus
