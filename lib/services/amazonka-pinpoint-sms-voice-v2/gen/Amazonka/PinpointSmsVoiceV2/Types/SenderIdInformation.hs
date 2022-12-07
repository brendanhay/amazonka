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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.SenderIdInformation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.SenderIdInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types.MessageType
import qualified Amazonka.Prelude as Prelude

-- | The information for all SenderIds in an Amazon Web Services account.
--
-- /See:/ 'newSenderIdInformation' smart constructor.
data SenderIdInformation = SenderIdInformation'
  { -- | The Amazon Resource Name (ARN) associated with the SenderId.
    senderIdArn :: Prelude.Text,
    -- | The alphanumeric sender ID in a specific country that you\'d like to
    -- describe.
    senderId :: Prelude.Text,
    -- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
    -- region.
    isoCountryCode :: Prelude.Text,
    -- | The type of message. Valid values are TRANSACTIONAL for messages that
    -- are critical or time-sensitive and PROMOTIONAL for messages that aren\'t
    -- critical or time-sensitive.
    messageTypes :: [MessageType],
    -- | The monthly leasing price, in US dollars.
    monthlyLeasingPrice :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SenderIdInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'senderIdArn', 'senderIdInformation_senderIdArn' - The Amazon Resource Name (ARN) associated with the SenderId.
--
-- 'senderId', 'senderIdInformation_senderId' - The alphanumeric sender ID in a specific country that you\'d like to
-- describe.
--
-- 'isoCountryCode', 'senderIdInformation_isoCountryCode' - The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region.
--
-- 'messageTypes', 'senderIdInformation_messageTypes' - The type of message. Valid values are TRANSACTIONAL for messages that
-- are critical or time-sensitive and PROMOTIONAL for messages that aren\'t
-- critical or time-sensitive.
--
-- 'monthlyLeasingPrice', 'senderIdInformation_monthlyLeasingPrice' - The monthly leasing price, in US dollars.
newSenderIdInformation ::
  -- | 'senderIdArn'
  Prelude.Text ->
  -- | 'senderId'
  Prelude.Text ->
  -- | 'isoCountryCode'
  Prelude.Text ->
  -- | 'monthlyLeasingPrice'
  Prelude.Text ->
  SenderIdInformation
newSenderIdInformation
  pSenderIdArn_
  pSenderId_
  pIsoCountryCode_
  pMonthlyLeasingPrice_ =
    SenderIdInformation'
      { senderIdArn = pSenderIdArn_,
        senderId = pSenderId_,
        isoCountryCode = pIsoCountryCode_,
        messageTypes = Prelude.mempty,
        monthlyLeasingPrice = pMonthlyLeasingPrice_
      }

-- | The Amazon Resource Name (ARN) associated with the SenderId.
senderIdInformation_senderIdArn :: Lens.Lens' SenderIdInformation Prelude.Text
senderIdInformation_senderIdArn = Lens.lens (\SenderIdInformation' {senderIdArn} -> senderIdArn) (\s@SenderIdInformation' {} a -> s {senderIdArn = a} :: SenderIdInformation)

-- | The alphanumeric sender ID in a specific country that you\'d like to
-- describe.
senderIdInformation_senderId :: Lens.Lens' SenderIdInformation Prelude.Text
senderIdInformation_senderId = Lens.lens (\SenderIdInformation' {senderId} -> senderId) (\s@SenderIdInformation' {} a -> s {senderId = a} :: SenderIdInformation)

-- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region.
senderIdInformation_isoCountryCode :: Lens.Lens' SenderIdInformation Prelude.Text
senderIdInformation_isoCountryCode = Lens.lens (\SenderIdInformation' {isoCountryCode} -> isoCountryCode) (\s@SenderIdInformation' {} a -> s {isoCountryCode = a} :: SenderIdInformation)

-- | The type of message. Valid values are TRANSACTIONAL for messages that
-- are critical or time-sensitive and PROMOTIONAL for messages that aren\'t
-- critical or time-sensitive.
senderIdInformation_messageTypes :: Lens.Lens' SenderIdInformation [MessageType]
senderIdInformation_messageTypes = Lens.lens (\SenderIdInformation' {messageTypes} -> messageTypes) (\s@SenderIdInformation' {} a -> s {messageTypes = a} :: SenderIdInformation) Prelude.. Lens.coerced

-- | The monthly leasing price, in US dollars.
senderIdInformation_monthlyLeasingPrice :: Lens.Lens' SenderIdInformation Prelude.Text
senderIdInformation_monthlyLeasingPrice = Lens.lens (\SenderIdInformation' {monthlyLeasingPrice} -> monthlyLeasingPrice) (\s@SenderIdInformation' {} a -> s {monthlyLeasingPrice = a} :: SenderIdInformation)

instance Data.FromJSON SenderIdInformation where
  parseJSON =
    Data.withObject
      "SenderIdInformation"
      ( \x ->
          SenderIdInformation'
            Prelude.<$> (x Data..: "SenderIdArn")
            Prelude.<*> (x Data..: "SenderId")
            Prelude.<*> (x Data..: "IsoCountryCode")
            Prelude.<*> (x Data..:? "MessageTypes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "MonthlyLeasingPrice")
      )

instance Prelude.Hashable SenderIdInformation where
  hashWithSalt _salt SenderIdInformation' {..} =
    _salt `Prelude.hashWithSalt` senderIdArn
      `Prelude.hashWithSalt` senderId
      `Prelude.hashWithSalt` isoCountryCode
      `Prelude.hashWithSalt` messageTypes
      `Prelude.hashWithSalt` monthlyLeasingPrice

instance Prelude.NFData SenderIdInformation where
  rnf SenderIdInformation' {..} =
    Prelude.rnf senderIdArn
      `Prelude.seq` Prelude.rnf senderId
      `Prelude.seq` Prelude.rnf isoCountryCode
      `Prelude.seq` Prelude.rnf messageTypes
      `Prelude.seq` Prelude.rnf monthlyLeasingPrice
