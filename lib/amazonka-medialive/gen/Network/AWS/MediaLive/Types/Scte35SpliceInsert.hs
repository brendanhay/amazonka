{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35SpliceInsert
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35SpliceInsert
  ( Scte35SpliceInsert (..),

    -- * Smart constructor
    mkScte35SpliceInsert,

    -- * Lenses
    ssiWebDeliveryAllowedFlag,
    ssiAdAvailOffset,
    ssiNoRegionalBlackoutFlag,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.Scte35SpliceInsertNoRegionalBlackoutBehavior
import Network.AWS.MediaLive.Types.Scte35SpliceInsertWebDeliveryAllowedBehavior
import qualified Network.AWS.Prelude as Lude

-- | Scte35 Splice Insert
--
-- /See:/ 'mkScte35SpliceInsert' smart constructor.
data Scte35SpliceInsert = Scte35SpliceInsert'
  { -- | When set to ignore, Segment Descriptors with webDeliveryAllowedFlag set to 0 will no longer trigger blackouts or Ad Avail slates
    webDeliveryAllowedFlag :: Lude.Maybe Scte35SpliceInsertWebDeliveryAllowedBehavior,
    -- | When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time. This only applies to embedded SCTE 104/35 messages and does not apply to OOB messages.
    adAvailOffset :: Lude.Maybe Lude.Int,
    -- | When set to ignore, Segment Descriptors with noRegionalBlackoutFlag set to 0 will no longer trigger blackouts or Ad Avail slates
    noRegionalBlackoutFlag :: Lude.Maybe Scte35SpliceInsertNoRegionalBlackoutBehavior
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Scte35SpliceInsert' with the minimum fields required to make a request.
--
-- * 'webDeliveryAllowedFlag' - When set to ignore, Segment Descriptors with webDeliveryAllowedFlag set to 0 will no longer trigger blackouts or Ad Avail slates
-- * 'adAvailOffset' - When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time. This only applies to embedded SCTE 104/35 messages and does not apply to OOB messages.
-- * 'noRegionalBlackoutFlag' - When set to ignore, Segment Descriptors with noRegionalBlackoutFlag set to 0 will no longer trigger blackouts or Ad Avail slates
mkScte35SpliceInsert ::
  Scte35SpliceInsert
mkScte35SpliceInsert =
  Scte35SpliceInsert'
    { webDeliveryAllowedFlag = Lude.Nothing,
      adAvailOffset = Lude.Nothing,
      noRegionalBlackoutFlag = Lude.Nothing
    }

-- | When set to ignore, Segment Descriptors with webDeliveryAllowedFlag set to 0 will no longer trigger blackouts or Ad Avail slates
--
-- /Note:/ Consider using 'webDeliveryAllowedFlag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssiWebDeliveryAllowedFlag :: Lens.Lens' Scte35SpliceInsert (Lude.Maybe Scte35SpliceInsertWebDeliveryAllowedBehavior)
ssiWebDeliveryAllowedFlag = Lens.lens (webDeliveryAllowedFlag :: Scte35SpliceInsert -> Lude.Maybe Scte35SpliceInsertWebDeliveryAllowedBehavior) (\s a -> s {webDeliveryAllowedFlag = a} :: Scte35SpliceInsert)
{-# DEPRECATED ssiWebDeliveryAllowedFlag "Use generic-lens or generic-optics with 'webDeliveryAllowedFlag' instead." #-}

-- | When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time. This only applies to embedded SCTE 104/35 messages and does not apply to OOB messages.
--
-- /Note:/ Consider using 'adAvailOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssiAdAvailOffset :: Lens.Lens' Scte35SpliceInsert (Lude.Maybe Lude.Int)
ssiAdAvailOffset = Lens.lens (adAvailOffset :: Scte35SpliceInsert -> Lude.Maybe Lude.Int) (\s a -> s {adAvailOffset = a} :: Scte35SpliceInsert)
{-# DEPRECATED ssiAdAvailOffset "Use generic-lens or generic-optics with 'adAvailOffset' instead." #-}

-- | When set to ignore, Segment Descriptors with noRegionalBlackoutFlag set to 0 will no longer trigger blackouts or Ad Avail slates
--
-- /Note:/ Consider using 'noRegionalBlackoutFlag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssiNoRegionalBlackoutFlag :: Lens.Lens' Scte35SpliceInsert (Lude.Maybe Scte35SpliceInsertNoRegionalBlackoutBehavior)
ssiNoRegionalBlackoutFlag = Lens.lens (noRegionalBlackoutFlag :: Scte35SpliceInsert -> Lude.Maybe Scte35SpliceInsertNoRegionalBlackoutBehavior) (\s a -> s {noRegionalBlackoutFlag = a} :: Scte35SpliceInsert)
{-# DEPRECATED ssiNoRegionalBlackoutFlag "Use generic-lens or generic-optics with 'noRegionalBlackoutFlag' instead." #-}

instance Lude.FromJSON Scte35SpliceInsert where
  parseJSON =
    Lude.withObject
      "Scte35SpliceInsert"
      ( \x ->
          Scte35SpliceInsert'
            Lude.<$> (x Lude..:? "webDeliveryAllowedFlag")
            Lude.<*> (x Lude..:? "adAvailOffset")
            Lude.<*> (x Lude..:? "noRegionalBlackoutFlag")
      )

instance Lude.ToJSON Scte35SpliceInsert where
  toJSON Scte35SpliceInsert' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("webDeliveryAllowedFlag" Lude..=)
              Lude.<$> webDeliveryAllowedFlag,
            ("adAvailOffset" Lude..=) Lude.<$> adAvailOffset,
            ("noRegionalBlackoutFlag" Lude..=)
              Lude.<$> noRegionalBlackoutFlag
          ]
      )
