{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35TimeSignalApos
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35TimeSignalApos
  ( Scte35TimeSignalApos (..),

    -- * Smart constructor
    mkScte35TimeSignalApos,

    -- * Lenses
    stsaWebDeliveryAllowedFlag,
    stsaAdAvailOffset,
    stsaNoRegionalBlackoutFlag,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.Scte35AposNoRegionalBlackoutBehavior
import Network.AWS.MediaLive.Types.Scte35AposWebDeliveryAllowedBehavior
import qualified Network.AWS.Prelude as Lude

-- | Scte35 Time Signal Apos
--
-- /See:/ 'mkScte35TimeSignalApos' smart constructor.
data Scte35TimeSignalApos = Scte35TimeSignalApos'
  { -- | When set to ignore, Segment Descriptors with webDeliveryAllowedFlag set to 0 will no longer trigger blackouts or Ad Avail slates
    webDeliveryAllowedFlag :: Lude.Maybe Scte35AposWebDeliveryAllowedBehavior,
    -- | When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time. This only applies to embedded SCTE 104/35 messages and does not apply to OOB messages.
    adAvailOffset :: Lude.Maybe Lude.Int,
    -- | When set to ignore, Segment Descriptors with noRegionalBlackoutFlag set to 0 will no longer trigger blackouts or Ad Avail slates
    noRegionalBlackoutFlag :: Lude.Maybe Scte35AposNoRegionalBlackoutBehavior
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Scte35TimeSignalApos' with the minimum fields required to make a request.
--
-- * 'webDeliveryAllowedFlag' - When set to ignore, Segment Descriptors with webDeliveryAllowedFlag set to 0 will no longer trigger blackouts or Ad Avail slates
-- * 'adAvailOffset' - When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time. This only applies to embedded SCTE 104/35 messages and does not apply to OOB messages.
-- * 'noRegionalBlackoutFlag' - When set to ignore, Segment Descriptors with noRegionalBlackoutFlag set to 0 will no longer trigger blackouts or Ad Avail slates
mkScte35TimeSignalApos ::
  Scte35TimeSignalApos
mkScte35TimeSignalApos =
  Scte35TimeSignalApos'
    { webDeliveryAllowedFlag = Lude.Nothing,
      adAvailOffset = Lude.Nothing,
      noRegionalBlackoutFlag = Lude.Nothing
    }

-- | When set to ignore, Segment Descriptors with webDeliveryAllowedFlag set to 0 will no longer trigger blackouts or Ad Avail slates
--
-- /Note:/ Consider using 'webDeliveryAllowedFlag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsaWebDeliveryAllowedFlag :: Lens.Lens' Scte35TimeSignalApos (Lude.Maybe Scte35AposWebDeliveryAllowedBehavior)
stsaWebDeliveryAllowedFlag = Lens.lens (webDeliveryAllowedFlag :: Scte35TimeSignalApos -> Lude.Maybe Scte35AposWebDeliveryAllowedBehavior) (\s a -> s {webDeliveryAllowedFlag = a} :: Scte35TimeSignalApos)
{-# DEPRECATED stsaWebDeliveryAllowedFlag "Use generic-lens or generic-optics with 'webDeliveryAllowedFlag' instead." #-}

-- | When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time. This only applies to embedded SCTE 104/35 messages and does not apply to OOB messages.
--
-- /Note:/ Consider using 'adAvailOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsaAdAvailOffset :: Lens.Lens' Scte35TimeSignalApos (Lude.Maybe Lude.Int)
stsaAdAvailOffset = Lens.lens (adAvailOffset :: Scte35TimeSignalApos -> Lude.Maybe Lude.Int) (\s a -> s {adAvailOffset = a} :: Scte35TimeSignalApos)
{-# DEPRECATED stsaAdAvailOffset "Use generic-lens or generic-optics with 'adAvailOffset' instead." #-}

-- | When set to ignore, Segment Descriptors with noRegionalBlackoutFlag set to 0 will no longer trigger blackouts or Ad Avail slates
--
-- /Note:/ Consider using 'noRegionalBlackoutFlag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsaNoRegionalBlackoutFlag :: Lens.Lens' Scte35TimeSignalApos (Lude.Maybe Scte35AposNoRegionalBlackoutBehavior)
stsaNoRegionalBlackoutFlag = Lens.lens (noRegionalBlackoutFlag :: Scte35TimeSignalApos -> Lude.Maybe Scte35AposNoRegionalBlackoutBehavior) (\s a -> s {noRegionalBlackoutFlag = a} :: Scte35TimeSignalApos)
{-# DEPRECATED stsaNoRegionalBlackoutFlag "Use generic-lens or generic-optics with 'noRegionalBlackoutFlag' instead." #-}

instance Lude.FromJSON Scte35TimeSignalApos where
  parseJSON =
    Lude.withObject
      "Scte35TimeSignalApos"
      ( \x ->
          Scte35TimeSignalApos'
            Lude.<$> (x Lude..:? "webDeliveryAllowedFlag")
            Lude.<*> (x Lude..:? "adAvailOffset")
            Lude.<*> (x Lude..:? "noRegionalBlackoutFlag")
      )

instance Lude.ToJSON Scte35TimeSignalApos where
  toJSON Scte35TimeSignalApos' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("webDeliveryAllowedFlag" Lude..=)
              Lude.<$> webDeliveryAllowedFlag,
            ("adAvailOffset" Lude..=) Lude.<$> adAvailOffset,
            ("noRegionalBlackoutFlag" Lude..=)
              Lude.<$> noRegionalBlackoutFlag
          ]
      )
