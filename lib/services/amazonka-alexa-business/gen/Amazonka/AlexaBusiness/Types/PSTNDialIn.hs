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
-- Module      : Amazonka.AlexaBusiness.Types.PSTNDialIn
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.PSTNDialIn where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The information for public switched telephone network (PSTN)
-- conferencing.
--
-- /See:/ 'newPSTNDialIn' smart constructor.
data PSTNDialIn = PSTNDialIn'
  { -- | The zip code.
    countryCode :: Prelude.Text,
    -- | The phone number to call to join the conference.
    phoneNumber :: Prelude.Text,
    -- | The delay duration before Alexa enters the conference ID with dual-tone
    -- multi-frequency (DTMF). Each number on the dial pad corresponds to a
    -- DTMF tone, which is how we send data over the telephone network.
    oneClickIdDelay :: Prelude.Text,
    -- | The delay duration before Alexa enters the conference pin with dual-tone
    -- multi-frequency (DTMF). Each number on the dial pad corresponds to a
    -- DTMF tone, which is how we send data over the telephone network.
    oneClickPinDelay :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PSTNDialIn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'countryCode', 'pSTNDialIn_countryCode' - The zip code.
--
-- 'phoneNumber', 'pSTNDialIn_phoneNumber' - The phone number to call to join the conference.
--
-- 'oneClickIdDelay', 'pSTNDialIn_oneClickIdDelay' - The delay duration before Alexa enters the conference ID with dual-tone
-- multi-frequency (DTMF). Each number on the dial pad corresponds to a
-- DTMF tone, which is how we send data over the telephone network.
--
-- 'oneClickPinDelay', 'pSTNDialIn_oneClickPinDelay' - The delay duration before Alexa enters the conference pin with dual-tone
-- multi-frequency (DTMF). Each number on the dial pad corresponds to a
-- DTMF tone, which is how we send data over the telephone network.
newPSTNDialIn ::
  -- | 'countryCode'
  Prelude.Text ->
  -- | 'phoneNumber'
  Prelude.Text ->
  -- | 'oneClickIdDelay'
  Prelude.Text ->
  -- | 'oneClickPinDelay'
  Prelude.Text ->
  PSTNDialIn
newPSTNDialIn
  pCountryCode_
  pPhoneNumber_
  pOneClickIdDelay_
  pOneClickPinDelay_ =
    PSTNDialIn'
      { countryCode = pCountryCode_,
        phoneNumber = pPhoneNumber_,
        oneClickIdDelay = pOneClickIdDelay_,
        oneClickPinDelay = pOneClickPinDelay_
      }

-- | The zip code.
pSTNDialIn_countryCode :: Lens.Lens' PSTNDialIn Prelude.Text
pSTNDialIn_countryCode = Lens.lens (\PSTNDialIn' {countryCode} -> countryCode) (\s@PSTNDialIn' {} a -> s {countryCode = a} :: PSTNDialIn)

-- | The phone number to call to join the conference.
pSTNDialIn_phoneNumber :: Lens.Lens' PSTNDialIn Prelude.Text
pSTNDialIn_phoneNumber = Lens.lens (\PSTNDialIn' {phoneNumber} -> phoneNumber) (\s@PSTNDialIn' {} a -> s {phoneNumber = a} :: PSTNDialIn)

-- | The delay duration before Alexa enters the conference ID with dual-tone
-- multi-frequency (DTMF). Each number on the dial pad corresponds to a
-- DTMF tone, which is how we send data over the telephone network.
pSTNDialIn_oneClickIdDelay :: Lens.Lens' PSTNDialIn Prelude.Text
pSTNDialIn_oneClickIdDelay = Lens.lens (\PSTNDialIn' {oneClickIdDelay} -> oneClickIdDelay) (\s@PSTNDialIn' {} a -> s {oneClickIdDelay = a} :: PSTNDialIn)

-- | The delay duration before Alexa enters the conference pin with dual-tone
-- multi-frequency (DTMF). Each number on the dial pad corresponds to a
-- DTMF tone, which is how we send data over the telephone network.
pSTNDialIn_oneClickPinDelay :: Lens.Lens' PSTNDialIn Prelude.Text
pSTNDialIn_oneClickPinDelay = Lens.lens (\PSTNDialIn' {oneClickPinDelay} -> oneClickPinDelay) (\s@PSTNDialIn' {} a -> s {oneClickPinDelay = a} :: PSTNDialIn)

instance Data.FromJSON PSTNDialIn where
  parseJSON =
    Data.withObject
      "PSTNDialIn"
      ( \x ->
          PSTNDialIn'
            Prelude.<$> (x Data..: "CountryCode")
            Prelude.<*> (x Data..: "PhoneNumber")
            Prelude.<*> (x Data..: "OneClickIdDelay")
            Prelude.<*> (x Data..: "OneClickPinDelay")
      )

instance Prelude.Hashable PSTNDialIn where
  hashWithSalt _salt PSTNDialIn' {..} =
    _salt `Prelude.hashWithSalt` countryCode
      `Prelude.hashWithSalt` phoneNumber
      `Prelude.hashWithSalt` oneClickIdDelay
      `Prelude.hashWithSalt` oneClickPinDelay

instance Prelude.NFData PSTNDialIn where
  rnf PSTNDialIn' {..} =
    Prelude.rnf countryCode
      `Prelude.seq` Prelude.rnf phoneNumber
      `Prelude.seq` Prelude.rnf oneClickIdDelay
      `Prelude.seq` Prelude.rnf oneClickPinDelay

instance Data.ToJSON PSTNDialIn where
  toJSON PSTNDialIn' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("CountryCode" Data..= countryCode),
            Prelude.Just ("PhoneNumber" Data..= phoneNumber),
            Prelude.Just
              ("OneClickIdDelay" Data..= oneClickIdDelay),
            Prelude.Just
              ("OneClickPinDelay" Data..= oneClickPinDelay)
          ]
      )
