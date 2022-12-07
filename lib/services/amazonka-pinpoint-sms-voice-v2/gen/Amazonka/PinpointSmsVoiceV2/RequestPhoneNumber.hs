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
-- Module      : Amazonka.PinpointSmsVoiceV2.RequestPhoneNumber
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Request an origination phone number for use in your account. For more
-- information on phone number request see
-- <https://docs.aws.amazon.com/pinpoint/latest/userguide/settings-sms-request-number.html Requesting a number>
-- in the /Amazon Pinpoint User Guide/.
module Amazonka.PinpointSmsVoiceV2.RequestPhoneNumber
  ( -- * Creating a Request
    RequestPhoneNumber (..),
    newRequestPhoneNumber,

    -- * Request Lenses
    requestPhoneNumber_deletionProtectionEnabled,
    requestPhoneNumber_tags,
    requestPhoneNumber_clientToken,
    requestPhoneNumber_registrationId,
    requestPhoneNumber_optOutListName,
    requestPhoneNumber_poolId,
    requestPhoneNumber_isoCountryCode,
    requestPhoneNumber_messageType,
    requestPhoneNumber_numberCapabilities,
    requestPhoneNumber_numberType,

    -- * Destructuring the Response
    RequestPhoneNumberResponse (..),
    newRequestPhoneNumberResponse,

    -- * Response Lenses
    requestPhoneNumberResponse_deletionProtectionEnabled,
    requestPhoneNumberResponse_tags,
    requestPhoneNumberResponse_isoCountryCode,
    requestPhoneNumberResponse_phoneNumberArn,
    requestPhoneNumberResponse_messageType,
    requestPhoneNumberResponse_selfManagedOptOutsEnabled,
    requestPhoneNumberResponse_createdTimestamp,
    requestPhoneNumberResponse_status,
    requestPhoneNumberResponse_numberCapabilities,
    requestPhoneNumberResponse_twoWayEnabled,
    requestPhoneNumberResponse_optOutListName,
    requestPhoneNumberResponse_numberType,
    requestPhoneNumberResponse_phoneNumberId,
    requestPhoneNumberResponse_poolId,
    requestPhoneNumberResponse_twoWayChannelArn,
    requestPhoneNumberResponse_phoneNumber,
    requestPhoneNumberResponse_monthlyLeasingPrice,
    requestPhoneNumberResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRequestPhoneNumber' smart constructor.
data RequestPhoneNumber = RequestPhoneNumber'
  { -- | By default this is set to false. When set to true the phone number
    -- can\'t be deleted.
    deletionProtectionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | An array of tags (key and value pairs) associate with the requested
    -- phone number.
    tags :: Prelude.Maybe [Tag],
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you don\'t specify a client token, a
    -- randomly generated token is used for the request to ensure idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Use this field to attach your phone number for an external registration
    -- process.
    registrationId :: Prelude.Maybe Prelude.Text,
    -- | The name of the OptOutList to associate with the phone number. You can
    -- use the OutOutListName or OptPutListArn.
    optOutListName :: Prelude.Maybe Prelude.Text,
    -- | The pool to associated with the phone number. You can use the PoolId or
    -- PoolArn.
    poolId :: Prelude.Maybe Prelude.Text,
    -- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
    -- region.
    isoCountryCode :: Prelude.Text,
    -- | The type of message. Valid values are TRANSACTIONAL for messages that
    -- are critical or time-sensitive and PROMOTIONAL for messages that aren\'t
    -- critical or time-sensitive.
    messageType :: MessageType,
    -- | Indicates if the phone number will be used for text messages, voice
    -- messages, or both.
    numberCapabilities :: Prelude.NonEmpty NumberCapability,
    -- | The type of phone number to request.
    numberType :: RequestableNumberType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequestPhoneNumber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionProtectionEnabled', 'requestPhoneNumber_deletionProtectionEnabled' - By default this is set to false. When set to true the phone number
-- can\'t be deleted.
--
-- 'tags', 'requestPhoneNumber_tags' - An array of tags (key and value pairs) associate with the requested
-- phone number.
--
-- 'clientToken', 'requestPhoneNumber_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don\'t specify a client token, a
-- randomly generated token is used for the request to ensure idempotency.
--
-- 'registrationId', 'requestPhoneNumber_registrationId' - Use this field to attach your phone number for an external registration
-- process.
--
-- 'optOutListName', 'requestPhoneNumber_optOutListName' - The name of the OptOutList to associate with the phone number. You can
-- use the OutOutListName or OptPutListArn.
--
-- 'poolId', 'requestPhoneNumber_poolId' - The pool to associated with the phone number. You can use the PoolId or
-- PoolArn.
--
-- 'isoCountryCode', 'requestPhoneNumber_isoCountryCode' - The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region.
--
-- 'messageType', 'requestPhoneNumber_messageType' - The type of message. Valid values are TRANSACTIONAL for messages that
-- are critical or time-sensitive and PROMOTIONAL for messages that aren\'t
-- critical or time-sensitive.
--
-- 'numberCapabilities', 'requestPhoneNumber_numberCapabilities' - Indicates if the phone number will be used for text messages, voice
-- messages, or both.
--
-- 'numberType', 'requestPhoneNumber_numberType' - The type of phone number to request.
newRequestPhoneNumber ::
  -- | 'isoCountryCode'
  Prelude.Text ->
  -- | 'messageType'
  MessageType ->
  -- | 'numberCapabilities'
  Prelude.NonEmpty NumberCapability ->
  -- | 'numberType'
  RequestableNumberType ->
  RequestPhoneNumber
newRequestPhoneNumber
  pIsoCountryCode_
  pMessageType_
  pNumberCapabilities_
  pNumberType_ =
    RequestPhoneNumber'
      { deletionProtectionEnabled =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        registrationId = Prelude.Nothing,
        optOutListName = Prelude.Nothing,
        poolId = Prelude.Nothing,
        isoCountryCode = pIsoCountryCode_,
        messageType = pMessageType_,
        numberCapabilities =
          Lens.coerced Lens.# pNumberCapabilities_,
        numberType = pNumberType_
      }

-- | By default this is set to false. When set to true the phone number
-- can\'t be deleted.
requestPhoneNumber_deletionProtectionEnabled :: Lens.Lens' RequestPhoneNumber (Prelude.Maybe Prelude.Bool)
requestPhoneNumber_deletionProtectionEnabled = Lens.lens (\RequestPhoneNumber' {deletionProtectionEnabled} -> deletionProtectionEnabled) (\s@RequestPhoneNumber' {} a -> s {deletionProtectionEnabled = a} :: RequestPhoneNumber)

-- | An array of tags (key and value pairs) associate with the requested
-- phone number.
requestPhoneNumber_tags :: Lens.Lens' RequestPhoneNumber (Prelude.Maybe [Tag])
requestPhoneNumber_tags = Lens.lens (\RequestPhoneNumber' {tags} -> tags) (\s@RequestPhoneNumber' {} a -> s {tags = a} :: RequestPhoneNumber) Prelude.. Lens.mapping Lens.coerced

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don\'t specify a client token, a
-- randomly generated token is used for the request to ensure idempotency.
requestPhoneNumber_clientToken :: Lens.Lens' RequestPhoneNumber (Prelude.Maybe Prelude.Text)
requestPhoneNumber_clientToken = Lens.lens (\RequestPhoneNumber' {clientToken} -> clientToken) (\s@RequestPhoneNumber' {} a -> s {clientToken = a} :: RequestPhoneNumber)

-- | Use this field to attach your phone number for an external registration
-- process.
requestPhoneNumber_registrationId :: Lens.Lens' RequestPhoneNumber (Prelude.Maybe Prelude.Text)
requestPhoneNumber_registrationId = Lens.lens (\RequestPhoneNumber' {registrationId} -> registrationId) (\s@RequestPhoneNumber' {} a -> s {registrationId = a} :: RequestPhoneNumber)

-- | The name of the OptOutList to associate with the phone number. You can
-- use the OutOutListName or OptPutListArn.
requestPhoneNumber_optOutListName :: Lens.Lens' RequestPhoneNumber (Prelude.Maybe Prelude.Text)
requestPhoneNumber_optOutListName = Lens.lens (\RequestPhoneNumber' {optOutListName} -> optOutListName) (\s@RequestPhoneNumber' {} a -> s {optOutListName = a} :: RequestPhoneNumber)

-- | The pool to associated with the phone number. You can use the PoolId or
-- PoolArn.
requestPhoneNumber_poolId :: Lens.Lens' RequestPhoneNumber (Prelude.Maybe Prelude.Text)
requestPhoneNumber_poolId = Lens.lens (\RequestPhoneNumber' {poolId} -> poolId) (\s@RequestPhoneNumber' {} a -> s {poolId = a} :: RequestPhoneNumber)

-- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region.
requestPhoneNumber_isoCountryCode :: Lens.Lens' RequestPhoneNumber Prelude.Text
requestPhoneNumber_isoCountryCode = Lens.lens (\RequestPhoneNumber' {isoCountryCode} -> isoCountryCode) (\s@RequestPhoneNumber' {} a -> s {isoCountryCode = a} :: RequestPhoneNumber)

-- | The type of message. Valid values are TRANSACTIONAL for messages that
-- are critical or time-sensitive and PROMOTIONAL for messages that aren\'t
-- critical or time-sensitive.
requestPhoneNumber_messageType :: Lens.Lens' RequestPhoneNumber MessageType
requestPhoneNumber_messageType = Lens.lens (\RequestPhoneNumber' {messageType} -> messageType) (\s@RequestPhoneNumber' {} a -> s {messageType = a} :: RequestPhoneNumber)

-- | Indicates if the phone number will be used for text messages, voice
-- messages, or both.
requestPhoneNumber_numberCapabilities :: Lens.Lens' RequestPhoneNumber (Prelude.NonEmpty NumberCapability)
requestPhoneNumber_numberCapabilities = Lens.lens (\RequestPhoneNumber' {numberCapabilities} -> numberCapabilities) (\s@RequestPhoneNumber' {} a -> s {numberCapabilities = a} :: RequestPhoneNumber) Prelude.. Lens.coerced

-- | The type of phone number to request.
requestPhoneNumber_numberType :: Lens.Lens' RequestPhoneNumber RequestableNumberType
requestPhoneNumber_numberType = Lens.lens (\RequestPhoneNumber' {numberType} -> numberType) (\s@RequestPhoneNumber' {} a -> s {numberType = a} :: RequestPhoneNumber)

instance Core.AWSRequest RequestPhoneNumber where
  type
    AWSResponse RequestPhoneNumber =
      RequestPhoneNumberResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RequestPhoneNumberResponse'
            Prelude.<$> (x Data..?> "DeletionProtectionEnabled")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "IsoCountryCode")
            Prelude.<*> (x Data..?> "PhoneNumberArn")
            Prelude.<*> (x Data..?> "MessageType")
            Prelude.<*> (x Data..?> "SelfManagedOptOutsEnabled")
            Prelude.<*> (x Data..?> "CreatedTimestamp")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "NumberCapabilities")
            Prelude.<*> (x Data..?> "TwoWayEnabled")
            Prelude.<*> (x Data..?> "OptOutListName")
            Prelude.<*> (x Data..?> "NumberType")
            Prelude.<*> (x Data..?> "PhoneNumberId")
            Prelude.<*> (x Data..?> "PoolId")
            Prelude.<*> (x Data..?> "TwoWayChannelArn")
            Prelude.<*> (x Data..?> "PhoneNumber")
            Prelude.<*> (x Data..?> "MonthlyLeasingPrice")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RequestPhoneNumber where
  hashWithSalt _salt RequestPhoneNumber' {..} =
    _salt
      `Prelude.hashWithSalt` deletionProtectionEnabled
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` registrationId
      `Prelude.hashWithSalt` optOutListName
      `Prelude.hashWithSalt` poolId
      `Prelude.hashWithSalt` isoCountryCode
      `Prelude.hashWithSalt` messageType
      `Prelude.hashWithSalt` numberCapabilities
      `Prelude.hashWithSalt` numberType

instance Prelude.NFData RequestPhoneNumber where
  rnf RequestPhoneNumber' {..} =
    Prelude.rnf deletionProtectionEnabled
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf registrationId
      `Prelude.seq` Prelude.rnf optOutListName
      `Prelude.seq` Prelude.rnf poolId
      `Prelude.seq` Prelude.rnf isoCountryCode
      `Prelude.seq` Prelude.rnf messageType
      `Prelude.seq` Prelude.rnf numberCapabilities
      `Prelude.seq` Prelude.rnf numberType

instance Data.ToHeaders RequestPhoneNumber where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.RequestPhoneNumber" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RequestPhoneNumber where
  toJSON RequestPhoneNumber' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeletionProtectionEnabled" Data..=)
              Prelude.<$> deletionProtectionEnabled,
            ("Tags" Data..=) Prelude.<$> tags,
            ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("RegistrationId" Data..=)
              Prelude.<$> registrationId,
            ("OptOutListName" Data..=)
              Prelude.<$> optOutListName,
            ("PoolId" Data..=) Prelude.<$> poolId,
            Prelude.Just
              ("IsoCountryCode" Data..= isoCountryCode),
            Prelude.Just ("MessageType" Data..= messageType),
            Prelude.Just
              ("NumberCapabilities" Data..= numberCapabilities),
            Prelude.Just ("NumberType" Data..= numberType)
          ]
      )

instance Data.ToPath RequestPhoneNumber where
  toPath = Prelude.const "/"

instance Data.ToQuery RequestPhoneNumber where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRequestPhoneNumberResponse' smart constructor.
data RequestPhoneNumberResponse = RequestPhoneNumberResponse'
  { -- | By default this is set to false. When set to true the phone number
    -- can\'t be deleted.
    deletionProtectionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | An array of key and value pair tags that are associated with the phone
    -- number.
    tags :: Prelude.Maybe [Tag],
    -- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
    -- region.
    isoCountryCode :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the requested phone number.
    phoneNumberArn :: Prelude.Maybe Prelude.Text,
    -- | The type of message. Valid values are TRANSACTIONAL for messages that
    -- are critical or time-sensitive and PROMOTIONAL for messages that aren\'t
    -- critical or time-sensitive.
    messageType :: Prelude.Maybe MessageType,
    -- | By default this is set to false. When an end recipient sends a message
    -- that begins with HELP or STOP to one of your dedicated numbers, Amazon
    -- Pinpoint automatically replies with a customizable message and adds the
    -- end recipient to the OptOutList. When set to true you\'re responsible
    -- for responding to HELP and STOP requests. You\'re also responsible for
    -- tracking and honoring opt-out requests.
    selfManagedOptOutsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The time when the phone number was created, in
    -- <https://www.epochconverter.com/ UNIX epoch time> format.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The current status of the request.
    status :: Prelude.Maybe NumberStatus,
    -- | Indicates if the phone number will be used for text messages, voice
    -- messages or both.
    numberCapabilities :: Prelude.Maybe (Prelude.NonEmpty NumberCapability),
    -- | By default this is set to false. When set to true you can receive
    -- incoming text messages from your end recipients.
    twoWayEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the OptOutList that is associated with the requested phone
    -- number.
    optOutListName :: Prelude.Maybe Prelude.Text,
    -- | The type of number that was released.
    numberType :: Prelude.Maybe RequestableNumberType,
    -- | The unique identifier of the new phone number.
    phoneNumberId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the pool associated with the phone number
    poolId :: Prelude.Maybe Prelude.Text,
    -- | The ARN used to identify the two way channel.
    twoWayChannelArn :: Prelude.Maybe Prelude.Text,
    -- | The new phone number that was requested.
    phoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The monthly price, in US dollars, to lease the phone number.
    monthlyLeasingPrice :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequestPhoneNumberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionProtectionEnabled', 'requestPhoneNumberResponse_deletionProtectionEnabled' - By default this is set to false. When set to true the phone number
-- can\'t be deleted.
--
-- 'tags', 'requestPhoneNumberResponse_tags' - An array of key and value pair tags that are associated with the phone
-- number.
--
-- 'isoCountryCode', 'requestPhoneNumberResponse_isoCountryCode' - The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region.
--
-- 'phoneNumberArn', 'requestPhoneNumberResponse_phoneNumberArn' - The Amazon Resource Name (ARN) of the requested phone number.
--
-- 'messageType', 'requestPhoneNumberResponse_messageType' - The type of message. Valid values are TRANSACTIONAL for messages that
-- are critical or time-sensitive and PROMOTIONAL for messages that aren\'t
-- critical or time-sensitive.
--
-- 'selfManagedOptOutsEnabled', 'requestPhoneNumberResponse_selfManagedOptOutsEnabled' - By default this is set to false. When an end recipient sends a message
-- that begins with HELP or STOP to one of your dedicated numbers, Amazon
-- Pinpoint automatically replies with a customizable message and adds the
-- end recipient to the OptOutList. When set to true you\'re responsible
-- for responding to HELP and STOP requests. You\'re also responsible for
-- tracking and honoring opt-out requests.
--
-- 'createdTimestamp', 'requestPhoneNumberResponse_createdTimestamp' - The time when the phone number was created, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
--
-- 'status', 'requestPhoneNumberResponse_status' - The current status of the request.
--
-- 'numberCapabilities', 'requestPhoneNumberResponse_numberCapabilities' - Indicates if the phone number will be used for text messages, voice
-- messages or both.
--
-- 'twoWayEnabled', 'requestPhoneNumberResponse_twoWayEnabled' - By default this is set to false. When set to true you can receive
-- incoming text messages from your end recipients.
--
-- 'optOutListName', 'requestPhoneNumberResponse_optOutListName' - The name of the OptOutList that is associated with the requested phone
-- number.
--
-- 'numberType', 'requestPhoneNumberResponse_numberType' - The type of number that was released.
--
-- 'phoneNumberId', 'requestPhoneNumberResponse_phoneNumberId' - The unique identifier of the new phone number.
--
-- 'poolId', 'requestPhoneNumberResponse_poolId' - The unique identifier of the pool associated with the phone number
--
-- 'twoWayChannelArn', 'requestPhoneNumberResponse_twoWayChannelArn' - The ARN used to identify the two way channel.
--
-- 'phoneNumber', 'requestPhoneNumberResponse_phoneNumber' - The new phone number that was requested.
--
-- 'monthlyLeasingPrice', 'requestPhoneNumberResponse_monthlyLeasingPrice' - The monthly price, in US dollars, to lease the phone number.
--
-- 'httpStatus', 'requestPhoneNumberResponse_httpStatus' - The response's http status code.
newRequestPhoneNumberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RequestPhoneNumberResponse
newRequestPhoneNumberResponse pHttpStatus_ =
  RequestPhoneNumberResponse'
    { deletionProtectionEnabled =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      isoCountryCode = Prelude.Nothing,
      phoneNumberArn = Prelude.Nothing,
      messageType = Prelude.Nothing,
      selfManagedOptOutsEnabled = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      status = Prelude.Nothing,
      numberCapabilities = Prelude.Nothing,
      twoWayEnabled = Prelude.Nothing,
      optOutListName = Prelude.Nothing,
      numberType = Prelude.Nothing,
      phoneNumberId = Prelude.Nothing,
      poolId = Prelude.Nothing,
      twoWayChannelArn = Prelude.Nothing,
      phoneNumber = Prelude.Nothing,
      monthlyLeasingPrice = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | By default this is set to false. When set to true the phone number
-- can\'t be deleted.
requestPhoneNumberResponse_deletionProtectionEnabled :: Lens.Lens' RequestPhoneNumberResponse (Prelude.Maybe Prelude.Bool)
requestPhoneNumberResponse_deletionProtectionEnabled = Lens.lens (\RequestPhoneNumberResponse' {deletionProtectionEnabled} -> deletionProtectionEnabled) (\s@RequestPhoneNumberResponse' {} a -> s {deletionProtectionEnabled = a} :: RequestPhoneNumberResponse)

-- | An array of key and value pair tags that are associated with the phone
-- number.
requestPhoneNumberResponse_tags :: Lens.Lens' RequestPhoneNumberResponse (Prelude.Maybe [Tag])
requestPhoneNumberResponse_tags = Lens.lens (\RequestPhoneNumberResponse' {tags} -> tags) (\s@RequestPhoneNumberResponse' {} a -> s {tags = a} :: RequestPhoneNumberResponse) Prelude.. Lens.mapping Lens.coerced

-- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region.
requestPhoneNumberResponse_isoCountryCode :: Lens.Lens' RequestPhoneNumberResponse (Prelude.Maybe Prelude.Text)
requestPhoneNumberResponse_isoCountryCode = Lens.lens (\RequestPhoneNumberResponse' {isoCountryCode} -> isoCountryCode) (\s@RequestPhoneNumberResponse' {} a -> s {isoCountryCode = a} :: RequestPhoneNumberResponse)

-- | The Amazon Resource Name (ARN) of the requested phone number.
requestPhoneNumberResponse_phoneNumberArn :: Lens.Lens' RequestPhoneNumberResponse (Prelude.Maybe Prelude.Text)
requestPhoneNumberResponse_phoneNumberArn = Lens.lens (\RequestPhoneNumberResponse' {phoneNumberArn} -> phoneNumberArn) (\s@RequestPhoneNumberResponse' {} a -> s {phoneNumberArn = a} :: RequestPhoneNumberResponse)

-- | The type of message. Valid values are TRANSACTIONAL for messages that
-- are critical or time-sensitive and PROMOTIONAL for messages that aren\'t
-- critical or time-sensitive.
requestPhoneNumberResponse_messageType :: Lens.Lens' RequestPhoneNumberResponse (Prelude.Maybe MessageType)
requestPhoneNumberResponse_messageType = Lens.lens (\RequestPhoneNumberResponse' {messageType} -> messageType) (\s@RequestPhoneNumberResponse' {} a -> s {messageType = a} :: RequestPhoneNumberResponse)

-- | By default this is set to false. When an end recipient sends a message
-- that begins with HELP or STOP to one of your dedicated numbers, Amazon
-- Pinpoint automatically replies with a customizable message and adds the
-- end recipient to the OptOutList. When set to true you\'re responsible
-- for responding to HELP and STOP requests. You\'re also responsible for
-- tracking and honoring opt-out requests.
requestPhoneNumberResponse_selfManagedOptOutsEnabled :: Lens.Lens' RequestPhoneNumberResponse (Prelude.Maybe Prelude.Bool)
requestPhoneNumberResponse_selfManagedOptOutsEnabled = Lens.lens (\RequestPhoneNumberResponse' {selfManagedOptOutsEnabled} -> selfManagedOptOutsEnabled) (\s@RequestPhoneNumberResponse' {} a -> s {selfManagedOptOutsEnabled = a} :: RequestPhoneNumberResponse)

-- | The time when the phone number was created, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
requestPhoneNumberResponse_createdTimestamp :: Lens.Lens' RequestPhoneNumberResponse (Prelude.Maybe Prelude.UTCTime)
requestPhoneNumberResponse_createdTimestamp = Lens.lens (\RequestPhoneNumberResponse' {createdTimestamp} -> createdTimestamp) (\s@RequestPhoneNumberResponse' {} a -> s {createdTimestamp = a} :: RequestPhoneNumberResponse) Prelude.. Lens.mapping Data._Time

-- | The current status of the request.
requestPhoneNumberResponse_status :: Lens.Lens' RequestPhoneNumberResponse (Prelude.Maybe NumberStatus)
requestPhoneNumberResponse_status = Lens.lens (\RequestPhoneNumberResponse' {status} -> status) (\s@RequestPhoneNumberResponse' {} a -> s {status = a} :: RequestPhoneNumberResponse)

-- | Indicates if the phone number will be used for text messages, voice
-- messages or both.
requestPhoneNumberResponse_numberCapabilities :: Lens.Lens' RequestPhoneNumberResponse (Prelude.Maybe (Prelude.NonEmpty NumberCapability))
requestPhoneNumberResponse_numberCapabilities = Lens.lens (\RequestPhoneNumberResponse' {numberCapabilities} -> numberCapabilities) (\s@RequestPhoneNumberResponse' {} a -> s {numberCapabilities = a} :: RequestPhoneNumberResponse) Prelude.. Lens.mapping Lens.coerced

-- | By default this is set to false. When set to true you can receive
-- incoming text messages from your end recipients.
requestPhoneNumberResponse_twoWayEnabled :: Lens.Lens' RequestPhoneNumberResponse (Prelude.Maybe Prelude.Bool)
requestPhoneNumberResponse_twoWayEnabled = Lens.lens (\RequestPhoneNumberResponse' {twoWayEnabled} -> twoWayEnabled) (\s@RequestPhoneNumberResponse' {} a -> s {twoWayEnabled = a} :: RequestPhoneNumberResponse)

-- | The name of the OptOutList that is associated with the requested phone
-- number.
requestPhoneNumberResponse_optOutListName :: Lens.Lens' RequestPhoneNumberResponse (Prelude.Maybe Prelude.Text)
requestPhoneNumberResponse_optOutListName = Lens.lens (\RequestPhoneNumberResponse' {optOutListName} -> optOutListName) (\s@RequestPhoneNumberResponse' {} a -> s {optOutListName = a} :: RequestPhoneNumberResponse)

-- | The type of number that was released.
requestPhoneNumberResponse_numberType :: Lens.Lens' RequestPhoneNumberResponse (Prelude.Maybe RequestableNumberType)
requestPhoneNumberResponse_numberType = Lens.lens (\RequestPhoneNumberResponse' {numberType} -> numberType) (\s@RequestPhoneNumberResponse' {} a -> s {numberType = a} :: RequestPhoneNumberResponse)

-- | The unique identifier of the new phone number.
requestPhoneNumberResponse_phoneNumberId :: Lens.Lens' RequestPhoneNumberResponse (Prelude.Maybe Prelude.Text)
requestPhoneNumberResponse_phoneNumberId = Lens.lens (\RequestPhoneNumberResponse' {phoneNumberId} -> phoneNumberId) (\s@RequestPhoneNumberResponse' {} a -> s {phoneNumberId = a} :: RequestPhoneNumberResponse)

-- | The unique identifier of the pool associated with the phone number
requestPhoneNumberResponse_poolId :: Lens.Lens' RequestPhoneNumberResponse (Prelude.Maybe Prelude.Text)
requestPhoneNumberResponse_poolId = Lens.lens (\RequestPhoneNumberResponse' {poolId} -> poolId) (\s@RequestPhoneNumberResponse' {} a -> s {poolId = a} :: RequestPhoneNumberResponse)

-- | The ARN used to identify the two way channel.
requestPhoneNumberResponse_twoWayChannelArn :: Lens.Lens' RequestPhoneNumberResponse (Prelude.Maybe Prelude.Text)
requestPhoneNumberResponse_twoWayChannelArn = Lens.lens (\RequestPhoneNumberResponse' {twoWayChannelArn} -> twoWayChannelArn) (\s@RequestPhoneNumberResponse' {} a -> s {twoWayChannelArn = a} :: RequestPhoneNumberResponse)

-- | The new phone number that was requested.
requestPhoneNumberResponse_phoneNumber :: Lens.Lens' RequestPhoneNumberResponse (Prelude.Maybe Prelude.Text)
requestPhoneNumberResponse_phoneNumber = Lens.lens (\RequestPhoneNumberResponse' {phoneNumber} -> phoneNumber) (\s@RequestPhoneNumberResponse' {} a -> s {phoneNumber = a} :: RequestPhoneNumberResponse)

-- | The monthly price, in US dollars, to lease the phone number.
requestPhoneNumberResponse_monthlyLeasingPrice :: Lens.Lens' RequestPhoneNumberResponse (Prelude.Maybe Prelude.Text)
requestPhoneNumberResponse_monthlyLeasingPrice = Lens.lens (\RequestPhoneNumberResponse' {monthlyLeasingPrice} -> monthlyLeasingPrice) (\s@RequestPhoneNumberResponse' {} a -> s {monthlyLeasingPrice = a} :: RequestPhoneNumberResponse)

-- | The response's http status code.
requestPhoneNumberResponse_httpStatus :: Lens.Lens' RequestPhoneNumberResponse Prelude.Int
requestPhoneNumberResponse_httpStatus = Lens.lens (\RequestPhoneNumberResponse' {httpStatus} -> httpStatus) (\s@RequestPhoneNumberResponse' {} a -> s {httpStatus = a} :: RequestPhoneNumberResponse)

instance Prelude.NFData RequestPhoneNumberResponse where
  rnf RequestPhoneNumberResponse' {..} =
    Prelude.rnf deletionProtectionEnabled
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf isoCountryCode
      `Prelude.seq` Prelude.rnf phoneNumberArn
      `Prelude.seq` Prelude.rnf messageType
      `Prelude.seq` Prelude.rnf selfManagedOptOutsEnabled
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf numberCapabilities
      `Prelude.seq` Prelude.rnf twoWayEnabled
      `Prelude.seq` Prelude.rnf optOutListName
      `Prelude.seq` Prelude.rnf numberType
      `Prelude.seq` Prelude.rnf phoneNumberId
      `Prelude.seq` Prelude.rnf poolId
      `Prelude.seq` Prelude.rnf twoWayChannelArn
      `Prelude.seq` Prelude.rnf phoneNumber
      `Prelude.seq` Prelude.rnf monthlyLeasingPrice
      `Prelude.seq` Prelude.rnf httpStatus
