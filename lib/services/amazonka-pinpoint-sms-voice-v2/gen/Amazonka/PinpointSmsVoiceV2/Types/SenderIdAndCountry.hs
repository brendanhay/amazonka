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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.SenderIdAndCountry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.SenderIdAndCountry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The alphanumeric sender ID in a specific country that you want to
-- describe. For more information on sender IDs see
-- <https://docs.aws.amazon.com/pinpoint/latest/userguide/channels-sms-awssupport-sender-id.html Requesting sender IDs for SMS messaging with Amazon Pinpoint>
-- in the /Amazon Pinpoint User Guide/.
--
-- /See:/ 'newSenderIdAndCountry' smart constructor.
data SenderIdAndCountry = SenderIdAndCountry'
  { -- | The unique identifier of the sender.
    senderId :: Prelude.Text,
    -- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
    -- region.
    isoCountryCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SenderIdAndCountry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'senderId', 'senderIdAndCountry_senderId' - The unique identifier of the sender.
--
-- 'isoCountryCode', 'senderIdAndCountry_isoCountryCode' - The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region.
newSenderIdAndCountry ::
  -- | 'senderId'
  Prelude.Text ->
  -- | 'isoCountryCode'
  Prelude.Text ->
  SenderIdAndCountry
newSenderIdAndCountry pSenderId_ pIsoCountryCode_ =
  SenderIdAndCountry'
    { senderId = pSenderId_,
      isoCountryCode = pIsoCountryCode_
    }

-- | The unique identifier of the sender.
senderIdAndCountry_senderId :: Lens.Lens' SenderIdAndCountry Prelude.Text
senderIdAndCountry_senderId = Lens.lens (\SenderIdAndCountry' {senderId} -> senderId) (\s@SenderIdAndCountry' {} a -> s {senderId = a} :: SenderIdAndCountry)

-- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region.
senderIdAndCountry_isoCountryCode :: Lens.Lens' SenderIdAndCountry Prelude.Text
senderIdAndCountry_isoCountryCode = Lens.lens (\SenderIdAndCountry' {isoCountryCode} -> isoCountryCode) (\s@SenderIdAndCountry' {} a -> s {isoCountryCode = a} :: SenderIdAndCountry)

instance Prelude.Hashable SenderIdAndCountry where
  hashWithSalt _salt SenderIdAndCountry' {..} =
    _salt
      `Prelude.hashWithSalt` senderId
      `Prelude.hashWithSalt` isoCountryCode

instance Prelude.NFData SenderIdAndCountry where
  rnf SenderIdAndCountry' {..} =
    Prelude.rnf senderId `Prelude.seq`
      Prelude.rnf isoCountryCode

instance Data.ToJSON SenderIdAndCountry where
  toJSON SenderIdAndCountry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SenderId" Data..= senderId),
            Prelude.Just
              ("IsoCountryCode" Data..= isoCountryCode)
          ]
      )
