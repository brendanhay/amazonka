{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.PSTNDialIn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.PSTNDialIn
  ( PSTNDialIn (..)
  -- * Smart constructor
  , mkPSTNDialIn
  -- * Lenses
  , pstndiCountryCode
  , pstndiPhoneNumber
  , pstndiOneClickIdDelay
  , pstndiOneClickPinDelay
  ) where

import qualified Network.AWS.AlexaBusiness.Types.CountryCode as Types
import qualified Network.AWS.AlexaBusiness.Types.OneClickIdDelay as Types
import qualified Network.AWS.AlexaBusiness.Types.OneClickPinDelay as Types
import qualified Network.AWS.AlexaBusiness.Types.OutboundPhoneNumber as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The information for public switched telephone network (PSTN) conferencing.
--
-- /See:/ 'mkPSTNDialIn' smart constructor.
data PSTNDialIn = PSTNDialIn'
  { countryCode :: Types.CountryCode
    -- ^ The zip code.
  , phoneNumber :: Types.OutboundPhoneNumber
    -- ^ The phone number to call to join the conference.
  , oneClickIdDelay :: Types.OneClickIdDelay
    -- ^ The delay duration before Alexa enters the conference ID with dual-tone multi-frequency (DTMF). Each number on the dial pad corresponds to a DTMF tone, which is how we send data over the telephone network.
  , oneClickPinDelay :: Types.OneClickPinDelay
    -- ^ The delay duration before Alexa enters the conference pin with dual-tone multi-frequency (DTMF). Each number on the dial pad corresponds to a DTMF tone, which is how we send data over the telephone network.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PSTNDialIn' value with any optional fields omitted.
mkPSTNDialIn
    :: Types.CountryCode -- ^ 'countryCode'
    -> Types.OutboundPhoneNumber -- ^ 'phoneNumber'
    -> Types.OneClickIdDelay -- ^ 'oneClickIdDelay'
    -> Types.OneClickPinDelay -- ^ 'oneClickPinDelay'
    -> PSTNDialIn
mkPSTNDialIn countryCode phoneNumber oneClickIdDelay
  oneClickPinDelay
  = PSTNDialIn'{countryCode, phoneNumber, oneClickIdDelay,
                oneClickPinDelay}

-- | The zip code.
--
-- /Note:/ Consider using 'countryCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstndiCountryCode :: Lens.Lens' PSTNDialIn Types.CountryCode
pstndiCountryCode = Lens.field @"countryCode"
{-# INLINEABLE pstndiCountryCode #-}
{-# DEPRECATED countryCode "Use generic-lens or generic-optics with 'countryCode' instead"  #-}

-- | The phone number to call to join the conference.
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstndiPhoneNumber :: Lens.Lens' PSTNDialIn Types.OutboundPhoneNumber
pstndiPhoneNumber = Lens.field @"phoneNumber"
{-# INLINEABLE pstndiPhoneNumber #-}
{-# DEPRECATED phoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead"  #-}

-- | The delay duration before Alexa enters the conference ID with dual-tone multi-frequency (DTMF). Each number on the dial pad corresponds to a DTMF tone, which is how we send data over the telephone network.
--
-- /Note:/ Consider using 'oneClickIdDelay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstndiOneClickIdDelay :: Lens.Lens' PSTNDialIn Types.OneClickIdDelay
pstndiOneClickIdDelay = Lens.field @"oneClickIdDelay"
{-# INLINEABLE pstndiOneClickIdDelay #-}
{-# DEPRECATED oneClickIdDelay "Use generic-lens or generic-optics with 'oneClickIdDelay' instead"  #-}

-- | The delay duration before Alexa enters the conference pin with dual-tone multi-frequency (DTMF). Each number on the dial pad corresponds to a DTMF tone, which is how we send data over the telephone network.
--
-- /Note:/ Consider using 'oneClickPinDelay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pstndiOneClickPinDelay :: Lens.Lens' PSTNDialIn Types.OneClickPinDelay
pstndiOneClickPinDelay = Lens.field @"oneClickPinDelay"
{-# INLINEABLE pstndiOneClickPinDelay #-}
{-# DEPRECATED oneClickPinDelay "Use generic-lens or generic-optics with 'oneClickPinDelay' instead"  #-}

instance Core.FromJSON PSTNDialIn where
        toJSON PSTNDialIn{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CountryCode" Core..= countryCode),
                  Core.Just ("PhoneNumber" Core..= phoneNumber),
                  Core.Just ("OneClickIdDelay" Core..= oneClickIdDelay),
                  Core.Just ("OneClickPinDelay" Core..= oneClickPinDelay)])

instance Core.FromJSON PSTNDialIn where
        parseJSON
          = Core.withObject "PSTNDialIn" Core.$
              \ x ->
                PSTNDialIn' Core.<$>
                  (x Core..: "CountryCode") Core.<*> x Core..: "PhoneNumber" Core.<*>
                    x Core..: "OneClickIdDelay"
                    Core.<*> x Core..: "OneClickPinDelay"
