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
-- Module      : Amazonka.Chime.Types.TelephonySettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.TelephonySettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Settings that allow management of telephony permissions for an Amazon
-- Chime user, such as inbound and outbound calling and text messaging.
--
-- /See:/ 'newTelephonySettings' smart constructor.
data TelephonySettings = TelephonySettings'
  { -- | Allows or denies inbound calling.
    inboundCalling :: Prelude.Bool,
    -- | Allows or denies outbound calling.
    outboundCalling :: Prelude.Bool,
    -- | Allows or denies SMS messaging.
    sms :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TelephonySettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inboundCalling', 'telephonySettings_inboundCalling' - Allows or denies inbound calling.
--
-- 'outboundCalling', 'telephonySettings_outboundCalling' - Allows or denies outbound calling.
--
-- 'sms', 'telephonySettings_sms' - Allows or denies SMS messaging.
newTelephonySettings ::
  -- | 'inboundCalling'
  Prelude.Bool ->
  -- | 'outboundCalling'
  Prelude.Bool ->
  -- | 'sms'
  Prelude.Bool ->
  TelephonySettings
newTelephonySettings
  pInboundCalling_
  pOutboundCalling_
  pSMS_ =
    TelephonySettings'
      { inboundCalling =
          pInboundCalling_,
        outboundCalling = pOutboundCalling_,
        sms = pSMS_
      }

-- | Allows or denies inbound calling.
telephonySettings_inboundCalling :: Lens.Lens' TelephonySettings Prelude.Bool
telephonySettings_inboundCalling = Lens.lens (\TelephonySettings' {inboundCalling} -> inboundCalling) (\s@TelephonySettings' {} a -> s {inboundCalling = a} :: TelephonySettings)

-- | Allows or denies outbound calling.
telephonySettings_outboundCalling :: Lens.Lens' TelephonySettings Prelude.Bool
telephonySettings_outboundCalling = Lens.lens (\TelephonySettings' {outboundCalling} -> outboundCalling) (\s@TelephonySettings' {} a -> s {outboundCalling = a} :: TelephonySettings)

-- | Allows or denies SMS messaging.
telephonySettings_sms :: Lens.Lens' TelephonySettings Prelude.Bool
telephonySettings_sms = Lens.lens (\TelephonySettings' {sms} -> sms) (\s@TelephonySettings' {} a -> s {sms = a} :: TelephonySettings)

instance Data.FromJSON TelephonySettings where
  parseJSON =
    Data.withObject
      "TelephonySettings"
      ( \x ->
          TelephonySettings'
            Prelude.<$> (x Data..: "InboundCalling")
            Prelude.<*> (x Data..: "OutboundCalling")
            Prelude.<*> (x Data..: "SMS")
      )

instance Prelude.Hashable TelephonySettings where
  hashWithSalt _salt TelephonySettings' {..} =
    _salt `Prelude.hashWithSalt` inboundCalling
      `Prelude.hashWithSalt` outboundCalling
      `Prelude.hashWithSalt` sms

instance Prelude.NFData TelephonySettings where
  rnf TelephonySettings' {..} =
    Prelude.rnf inboundCalling
      `Prelude.seq` Prelude.rnf outboundCalling
      `Prelude.seq` Prelude.rnf sms

instance Data.ToJSON TelephonySettings where
  toJSON TelephonySettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("InboundCalling" Data..= inboundCalling),
            Prelude.Just
              ("OutboundCalling" Data..= outboundCalling),
            Prelude.Just ("SMS" Data..= sms)
          ]
      )
