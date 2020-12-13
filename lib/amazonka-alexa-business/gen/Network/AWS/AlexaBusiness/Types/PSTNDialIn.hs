{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.PSTNDialIn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.PSTNDialIn
  ( PSTNDialIn (..),

    -- * Smart constructor
    mkPSTNDialIn,

    -- * Lenses
    pstndiOneClickIdDelay,
    pstndiOneClickPinDelay,
    pstndiPhoneNumber,
    pstndiCountryCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The information for public switched telephone network (PSTN) conferencing.
--
-- /See:/ 'mkPSTNDialIn' smart constructor.
data PSTNDialIn = PSTNDialIn'
  { -- | The delay duration before Alexa enters the conference ID with dual-tone multi-frequency (DTMF). Each number on the dial pad corresponds to a DTMF tone, which is how we send data over the telephone network.
    oneClickIdDelay :: Lude.Text,
    -- | The delay duration before Alexa enters the conference pin with dual-tone multi-frequency (DTMF). Each number on the dial pad corresponds to a DTMF tone, which is how we send data over the telephone network.
    oneClickPinDelay :: Lude.Text,
    -- | The phone number to call to join the conference.
    phoneNumber :: Lude.Text,
    -- | The zip code.
    countryCode :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PSTNDialIn' with the minimum fields required to make a request.
--
-- * 'oneClickIdDelay' - The delay duration before Alexa enters the conference ID with dual-tone multi-frequency (DTMF). Each number on the dial pad corresponds to a DTMF tone, which is how we send data over the telephone network.
-- * 'oneClickPinDelay' - The delay duration before Alexa enters the conference pin with dual-tone multi-frequency (DTMF). Each number on the dial pad corresponds to a DTMF tone, which is how we send data over the telephone network.
-- * 'phoneNumber' - The phone number to call to join the conference.
-- * 'countryCode' - The zip code.
mkPSTNDialIn ::
  -- | 'oneClickIdDelay'
  Lude.Text ->
  -- | 'oneClickPinDelay'
  Lude.Text ->
  -- | 'phoneNumber'
  Lude.Text ->
  -- | 'countryCode'
  Lude.Text ->
  PSTNDialIn
mkPSTNDialIn
  pOneClickIdDelay_
  pOneClickPinDelay_
  pPhoneNumber_
  pCountryCode_ =
    PSTNDialIn'
      { oneClickIdDelay = pOneClickIdDelay_,
        oneClickPinDelay = pOneClickPinDelay_,
        phoneNumber = pPhoneNumber_,
        countryCode = pCountryCode_
      }

-- | The delay duration before Alexa enters the conference ID with dual-tone multi-frequency (DTMF). Each number on the dial pad corresponds to a DTMF tone, which is how we send data over the telephone network.
--
-- /Note:/ Consider using 'oneClickIdDelay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstndiOneClickIdDelay :: Lens.Lens' PSTNDialIn Lude.Text
pstndiOneClickIdDelay = Lens.lens (oneClickIdDelay :: PSTNDialIn -> Lude.Text) (\s a -> s {oneClickIdDelay = a} :: PSTNDialIn)
{-# DEPRECATED pstndiOneClickIdDelay "Use generic-lens or generic-optics with 'oneClickIdDelay' instead." #-}

-- | The delay duration before Alexa enters the conference pin with dual-tone multi-frequency (DTMF). Each number on the dial pad corresponds to a DTMF tone, which is how we send data over the telephone network.
--
-- /Note:/ Consider using 'oneClickPinDelay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstndiOneClickPinDelay :: Lens.Lens' PSTNDialIn Lude.Text
pstndiOneClickPinDelay = Lens.lens (oneClickPinDelay :: PSTNDialIn -> Lude.Text) (\s a -> s {oneClickPinDelay = a} :: PSTNDialIn)
{-# DEPRECATED pstndiOneClickPinDelay "Use generic-lens or generic-optics with 'oneClickPinDelay' instead." #-}

-- | The phone number to call to join the conference.
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstndiPhoneNumber :: Lens.Lens' PSTNDialIn Lude.Text
pstndiPhoneNumber = Lens.lens (phoneNumber :: PSTNDialIn -> Lude.Text) (\s a -> s {phoneNumber = a} :: PSTNDialIn)
{-# DEPRECATED pstndiPhoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead." #-}

-- | The zip code.
--
-- /Note:/ Consider using 'countryCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstndiCountryCode :: Lens.Lens' PSTNDialIn Lude.Text
pstndiCountryCode = Lens.lens (countryCode :: PSTNDialIn -> Lude.Text) (\s a -> s {countryCode = a} :: PSTNDialIn)
{-# DEPRECATED pstndiCountryCode "Use generic-lens or generic-optics with 'countryCode' instead." #-}

instance Lude.FromJSON PSTNDialIn where
  parseJSON =
    Lude.withObject
      "PSTNDialIn"
      ( \x ->
          PSTNDialIn'
            Lude.<$> (x Lude..: "OneClickIdDelay")
            Lude.<*> (x Lude..: "OneClickPinDelay")
            Lude.<*> (x Lude..: "PhoneNumber")
            Lude.<*> (x Lude..: "CountryCode")
      )

instance Lude.ToJSON PSTNDialIn where
  toJSON PSTNDialIn' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("OneClickIdDelay" Lude..= oneClickIdDelay),
            Lude.Just ("OneClickPinDelay" Lude..= oneClickPinDelay),
            Lude.Just ("PhoneNumber" Lude..= phoneNumber),
            Lude.Just ("CountryCode" Lude..= countryCode)
          ]
      )
