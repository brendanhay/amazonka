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
-- Module      : Amazonka.PinpointSmsVoiceV2.ReleasePhoneNumber
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Releases an existing origination phone number in your account. Once
-- released, a phone number is no longer available for sending messages.
--
-- If the origination phone number has deletion protection enabled or is
-- associated with a pool, an Error is returned.
module Amazonka.PinpointSmsVoiceV2.ReleasePhoneNumber
  ( -- * Creating a Request
    ReleasePhoneNumber (..),
    newReleasePhoneNumber,

    -- * Request Lenses
    releasePhoneNumber_phoneNumberId,

    -- * Destructuring the Response
    ReleasePhoneNumberResponse (..),
    newReleasePhoneNumberResponse,

    -- * Response Lenses
    releasePhoneNumberResponse_createdTimestamp,
    releasePhoneNumberResponse_isoCountryCode,
    releasePhoneNumberResponse_messageType,
    releasePhoneNumberResponse_monthlyLeasingPrice,
    releasePhoneNumberResponse_numberCapabilities,
    releasePhoneNumberResponse_numberType,
    releasePhoneNumberResponse_optOutListName,
    releasePhoneNumberResponse_phoneNumber,
    releasePhoneNumberResponse_phoneNumberArn,
    releasePhoneNumberResponse_phoneNumberId,
    releasePhoneNumberResponse_selfManagedOptOutsEnabled,
    releasePhoneNumberResponse_status,
    releasePhoneNumberResponse_twoWayChannelArn,
    releasePhoneNumberResponse_twoWayEnabled,
    releasePhoneNumberResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newReleasePhoneNumber' smart constructor.
data ReleasePhoneNumber = ReleasePhoneNumber'
  { -- | The PhoneNumberId or PhoneNumberArn of the phone number to release. You
    -- can use DescribePhoneNumbers to get the values for PhoneNumberId and
    -- PhoneNumberArn.
    phoneNumberId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReleasePhoneNumber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumberId', 'releasePhoneNumber_phoneNumberId' - The PhoneNumberId or PhoneNumberArn of the phone number to release. You
-- can use DescribePhoneNumbers to get the values for PhoneNumberId and
-- PhoneNumberArn.
newReleasePhoneNumber ::
  -- | 'phoneNumberId'
  Prelude.Text ->
  ReleasePhoneNumber
newReleasePhoneNumber pPhoneNumberId_ =
  ReleasePhoneNumber'
    { phoneNumberId =
        pPhoneNumberId_
    }

-- | The PhoneNumberId or PhoneNumberArn of the phone number to release. You
-- can use DescribePhoneNumbers to get the values for PhoneNumberId and
-- PhoneNumberArn.
releasePhoneNumber_phoneNumberId :: Lens.Lens' ReleasePhoneNumber Prelude.Text
releasePhoneNumber_phoneNumberId = Lens.lens (\ReleasePhoneNumber' {phoneNumberId} -> phoneNumberId) (\s@ReleasePhoneNumber' {} a -> s {phoneNumberId = a} :: ReleasePhoneNumber)

instance Core.AWSRequest ReleasePhoneNumber where
  type
    AWSResponse ReleasePhoneNumber =
      ReleasePhoneNumberResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ReleasePhoneNumberResponse'
            Prelude.<$> (x Data..?> "CreatedTimestamp")
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

instance Prelude.Hashable ReleasePhoneNumber where
  hashWithSalt _salt ReleasePhoneNumber' {..} =
    _salt `Prelude.hashWithSalt` phoneNumberId

instance Prelude.NFData ReleasePhoneNumber where
  rnf ReleasePhoneNumber' {..} =
    Prelude.rnf phoneNumberId

instance Data.ToHeaders ReleasePhoneNumber where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.ReleasePhoneNumber" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ReleasePhoneNumber where
  toJSON ReleasePhoneNumber' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PhoneNumberId" Data..= phoneNumberId)
          ]
      )

instance Data.ToPath ReleasePhoneNumber where
  toPath = Prelude.const "/"

instance Data.ToQuery ReleasePhoneNumber where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newReleasePhoneNumberResponse' smart constructor.
data ReleasePhoneNumberResponse = ReleasePhoneNumberResponse'
  { -- | The time when the phone number was created, in
    -- <https://www.epochconverter.com/ UNIX epoch time> format.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
    -- region.
    isoCountryCode :: Prelude.Maybe Prelude.Text,
    -- | The message type that was associated with the phone number.
    messageType :: Prelude.Maybe MessageType,
    -- | The monthly price of the phone number, in US dollars.
    monthlyLeasingPrice :: Prelude.Maybe Prelude.Text,
    -- | Specifies if the number could be used for text messages, voice, or both.
    numberCapabilities :: Prelude.Maybe (Prelude.NonEmpty NumberCapability),
    -- | The type of number that was released.
    numberType :: Prelude.Maybe NumberType,
    -- | The name of the OptOutList that was associated with the phone number.
    optOutListName :: Prelude.Maybe Prelude.Text,
    -- | The phone number that was released.
    phoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The PhoneNumberArn of the phone number that was released.
    phoneNumberArn :: Prelude.Maybe Prelude.Text,
    -- | The PhoneNumberId of the phone number that was released.
    phoneNumberId :: Prelude.Maybe Prelude.Text,
    -- | By default this is set to false. When an end recipient sends a message
    -- that begins with HELP or STOP to one of your dedicated numbers, Amazon
    -- Pinpoint automatically replies with a customizable message and adds the
    -- end recipient to the OptOutList. When set to true you\'re responsible
    -- for responding to HELP and STOP requests. You\'re also responsible for
    -- tracking and honoring opt-out requests.
    selfManagedOptOutsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The current status of the request.
    status :: Prelude.Maybe NumberStatus,
    -- | The Amazon Resource Name (ARN) of the TwoWayChannel.
    twoWayChannelArn :: Prelude.Maybe Prelude.Text,
    -- | By default this is set to false. When set to true you can receive
    -- incoming text messages from your end recipients.
    twoWayEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReleasePhoneNumberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimestamp', 'releasePhoneNumberResponse_createdTimestamp' - The time when the phone number was created, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
--
-- 'isoCountryCode', 'releasePhoneNumberResponse_isoCountryCode' - The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region.
--
-- 'messageType', 'releasePhoneNumberResponse_messageType' - The message type that was associated with the phone number.
--
-- 'monthlyLeasingPrice', 'releasePhoneNumberResponse_monthlyLeasingPrice' - The monthly price of the phone number, in US dollars.
--
-- 'numberCapabilities', 'releasePhoneNumberResponse_numberCapabilities' - Specifies if the number could be used for text messages, voice, or both.
--
-- 'numberType', 'releasePhoneNumberResponse_numberType' - The type of number that was released.
--
-- 'optOutListName', 'releasePhoneNumberResponse_optOutListName' - The name of the OptOutList that was associated with the phone number.
--
-- 'phoneNumber', 'releasePhoneNumberResponse_phoneNumber' - The phone number that was released.
--
-- 'phoneNumberArn', 'releasePhoneNumberResponse_phoneNumberArn' - The PhoneNumberArn of the phone number that was released.
--
-- 'phoneNumberId', 'releasePhoneNumberResponse_phoneNumberId' - The PhoneNumberId of the phone number that was released.
--
-- 'selfManagedOptOutsEnabled', 'releasePhoneNumberResponse_selfManagedOptOutsEnabled' - By default this is set to false. When an end recipient sends a message
-- that begins with HELP or STOP to one of your dedicated numbers, Amazon
-- Pinpoint automatically replies with a customizable message and adds the
-- end recipient to the OptOutList. When set to true you\'re responsible
-- for responding to HELP and STOP requests. You\'re also responsible for
-- tracking and honoring opt-out requests.
--
-- 'status', 'releasePhoneNumberResponse_status' - The current status of the request.
--
-- 'twoWayChannelArn', 'releasePhoneNumberResponse_twoWayChannelArn' - The Amazon Resource Name (ARN) of the TwoWayChannel.
--
-- 'twoWayEnabled', 'releasePhoneNumberResponse_twoWayEnabled' - By default this is set to false. When set to true you can receive
-- incoming text messages from your end recipients.
--
-- 'httpStatus', 'releasePhoneNumberResponse_httpStatus' - The response's http status code.
newReleasePhoneNumberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ReleasePhoneNumberResponse
newReleasePhoneNumberResponse pHttpStatus_ =
  ReleasePhoneNumberResponse'
    { createdTimestamp =
        Prelude.Nothing,
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
releasePhoneNumberResponse_createdTimestamp :: Lens.Lens' ReleasePhoneNumberResponse (Prelude.Maybe Prelude.UTCTime)
releasePhoneNumberResponse_createdTimestamp = Lens.lens (\ReleasePhoneNumberResponse' {createdTimestamp} -> createdTimestamp) (\s@ReleasePhoneNumberResponse' {} a -> s {createdTimestamp = a} :: ReleasePhoneNumberResponse) Prelude.. Lens.mapping Data._Time

-- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region.
releasePhoneNumberResponse_isoCountryCode :: Lens.Lens' ReleasePhoneNumberResponse (Prelude.Maybe Prelude.Text)
releasePhoneNumberResponse_isoCountryCode = Lens.lens (\ReleasePhoneNumberResponse' {isoCountryCode} -> isoCountryCode) (\s@ReleasePhoneNumberResponse' {} a -> s {isoCountryCode = a} :: ReleasePhoneNumberResponse)

-- | The message type that was associated with the phone number.
releasePhoneNumberResponse_messageType :: Lens.Lens' ReleasePhoneNumberResponse (Prelude.Maybe MessageType)
releasePhoneNumberResponse_messageType = Lens.lens (\ReleasePhoneNumberResponse' {messageType} -> messageType) (\s@ReleasePhoneNumberResponse' {} a -> s {messageType = a} :: ReleasePhoneNumberResponse)

-- | The monthly price of the phone number, in US dollars.
releasePhoneNumberResponse_monthlyLeasingPrice :: Lens.Lens' ReleasePhoneNumberResponse (Prelude.Maybe Prelude.Text)
releasePhoneNumberResponse_monthlyLeasingPrice = Lens.lens (\ReleasePhoneNumberResponse' {monthlyLeasingPrice} -> monthlyLeasingPrice) (\s@ReleasePhoneNumberResponse' {} a -> s {monthlyLeasingPrice = a} :: ReleasePhoneNumberResponse)

-- | Specifies if the number could be used for text messages, voice, or both.
releasePhoneNumberResponse_numberCapabilities :: Lens.Lens' ReleasePhoneNumberResponse (Prelude.Maybe (Prelude.NonEmpty NumberCapability))
releasePhoneNumberResponse_numberCapabilities = Lens.lens (\ReleasePhoneNumberResponse' {numberCapabilities} -> numberCapabilities) (\s@ReleasePhoneNumberResponse' {} a -> s {numberCapabilities = a} :: ReleasePhoneNumberResponse) Prelude.. Lens.mapping Lens.coerced

-- | The type of number that was released.
releasePhoneNumberResponse_numberType :: Lens.Lens' ReleasePhoneNumberResponse (Prelude.Maybe NumberType)
releasePhoneNumberResponse_numberType = Lens.lens (\ReleasePhoneNumberResponse' {numberType} -> numberType) (\s@ReleasePhoneNumberResponse' {} a -> s {numberType = a} :: ReleasePhoneNumberResponse)

-- | The name of the OptOutList that was associated with the phone number.
releasePhoneNumberResponse_optOutListName :: Lens.Lens' ReleasePhoneNumberResponse (Prelude.Maybe Prelude.Text)
releasePhoneNumberResponse_optOutListName = Lens.lens (\ReleasePhoneNumberResponse' {optOutListName} -> optOutListName) (\s@ReleasePhoneNumberResponse' {} a -> s {optOutListName = a} :: ReleasePhoneNumberResponse)

-- | The phone number that was released.
releasePhoneNumberResponse_phoneNumber :: Lens.Lens' ReleasePhoneNumberResponse (Prelude.Maybe Prelude.Text)
releasePhoneNumberResponse_phoneNumber = Lens.lens (\ReleasePhoneNumberResponse' {phoneNumber} -> phoneNumber) (\s@ReleasePhoneNumberResponse' {} a -> s {phoneNumber = a} :: ReleasePhoneNumberResponse)

-- | The PhoneNumberArn of the phone number that was released.
releasePhoneNumberResponse_phoneNumberArn :: Lens.Lens' ReleasePhoneNumberResponse (Prelude.Maybe Prelude.Text)
releasePhoneNumberResponse_phoneNumberArn = Lens.lens (\ReleasePhoneNumberResponse' {phoneNumberArn} -> phoneNumberArn) (\s@ReleasePhoneNumberResponse' {} a -> s {phoneNumberArn = a} :: ReleasePhoneNumberResponse)

-- | The PhoneNumberId of the phone number that was released.
releasePhoneNumberResponse_phoneNumberId :: Lens.Lens' ReleasePhoneNumberResponse (Prelude.Maybe Prelude.Text)
releasePhoneNumberResponse_phoneNumberId = Lens.lens (\ReleasePhoneNumberResponse' {phoneNumberId} -> phoneNumberId) (\s@ReleasePhoneNumberResponse' {} a -> s {phoneNumberId = a} :: ReleasePhoneNumberResponse)

-- | By default this is set to false. When an end recipient sends a message
-- that begins with HELP or STOP to one of your dedicated numbers, Amazon
-- Pinpoint automatically replies with a customizable message and adds the
-- end recipient to the OptOutList. When set to true you\'re responsible
-- for responding to HELP and STOP requests. You\'re also responsible for
-- tracking and honoring opt-out requests.
releasePhoneNumberResponse_selfManagedOptOutsEnabled :: Lens.Lens' ReleasePhoneNumberResponse (Prelude.Maybe Prelude.Bool)
releasePhoneNumberResponse_selfManagedOptOutsEnabled = Lens.lens (\ReleasePhoneNumberResponse' {selfManagedOptOutsEnabled} -> selfManagedOptOutsEnabled) (\s@ReleasePhoneNumberResponse' {} a -> s {selfManagedOptOutsEnabled = a} :: ReleasePhoneNumberResponse)

-- | The current status of the request.
releasePhoneNumberResponse_status :: Lens.Lens' ReleasePhoneNumberResponse (Prelude.Maybe NumberStatus)
releasePhoneNumberResponse_status = Lens.lens (\ReleasePhoneNumberResponse' {status} -> status) (\s@ReleasePhoneNumberResponse' {} a -> s {status = a} :: ReleasePhoneNumberResponse)

-- | The Amazon Resource Name (ARN) of the TwoWayChannel.
releasePhoneNumberResponse_twoWayChannelArn :: Lens.Lens' ReleasePhoneNumberResponse (Prelude.Maybe Prelude.Text)
releasePhoneNumberResponse_twoWayChannelArn = Lens.lens (\ReleasePhoneNumberResponse' {twoWayChannelArn} -> twoWayChannelArn) (\s@ReleasePhoneNumberResponse' {} a -> s {twoWayChannelArn = a} :: ReleasePhoneNumberResponse)

-- | By default this is set to false. When set to true you can receive
-- incoming text messages from your end recipients.
releasePhoneNumberResponse_twoWayEnabled :: Lens.Lens' ReleasePhoneNumberResponse (Prelude.Maybe Prelude.Bool)
releasePhoneNumberResponse_twoWayEnabled = Lens.lens (\ReleasePhoneNumberResponse' {twoWayEnabled} -> twoWayEnabled) (\s@ReleasePhoneNumberResponse' {} a -> s {twoWayEnabled = a} :: ReleasePhoneNumberResponse)

-- | The response's http status code.
releasePhoneNumberResponse_httpStatus :: Lens.Lens' ReleasePhoneNumberResponse Prelude.Int
releasePhoneNumberResponse_httpStatus = Lens.lens (\ReleasePhoneNumberResponse' {httpStatus} -> httpStatus) (\s@ReleasePhoneNumberResponse' {} a -> s {httpStatus = a} :: ReleasePhoneNumberResponse)

instance Prelude.NFData ReleasePhoneNumberResponse where
  rnf ReleasePhoneNumberResponse' {..} =
    Prelude.rnf createdTimestamp
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
