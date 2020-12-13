{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.StreamSelection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.StreamSelection
  ( StreamSelection (..),

    -- * Smart constructor
    mkStreamSelection,

    -- * Lenses
    ssStreamOrder,
    ssMinVideoBitsPerSecond,
    ssMaxVideoBitsPerSecond,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.StreamOrder
import qualified Network.AWS.Prelude as Lude

-- | A StreamSelection configuration.
--
-- /See:/ 'mkStreamSelection' smart constructor.
data StreamSelection = StreamSelection'
  { -- | A directive that determines the order of streams in the output.
    streamOrder :: Lude.Maybe StreamOrder,
    -- | The minimum video bitrate (bps) to include in output.
    minVideoBitsPerSecond :: Lude.Maybe Lude.Int,
    -- | The maximum video bitrate (bps) to include in output.
    maxVideoBitsPerSecond :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StreamSelection' with the minimum fields required to make a request.
--
-- * 'streamOrder' - A directive that determines the order of streams in the output.
-- * 'minVideoBitsPerSecond' - The minimum video bitrate (bps) to include in output.
-- * 'maxVideoBitsPerSecond' - The maximum video bitrate (bps) to include in output.
mkStreamSelection ::
  StreamSelection
mkStreamSelection =
  StreamSelection'
    { streamOrder = Lude.Nothing,
      minVideoBitsPerSecond = Lude.Nothing,
      maxVideoBitsPerSecond = Lude.Nothing
    }

-- | A directive that determines the order of streams in the output.
--
-- /Note:/ Consider using 'streamOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStreamOrder :: Lens.Lens' StreamSelection (Lude.Maybe StreamOrder)
ssStreamOrder = Lens.lens (streamOrder :: StreamSelection -> Lude.Maybe StreamOrder) (\s a -> s {streamOrder = a} :: StreamSelection)
{-# DEPRECATED ssStreamOrder "Use generic-lens or generic-optics with 'streamOrder' instead." #-}

-- | The minimum video bitrate (bps) to include in output.
--
-- /Note:/ Consider using 'minVideoBitsPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssMinVideoBitsPerSecond :: Lens.Lens' StreamSelection (Lude.Maybe Lude.Int)
ssMinVideoBitsPerSecond = Lens.lens (minVideoBitsPerSecond :: StreamSelection -> Lude.Maybe Lude.Int) (\s a -> s {minVideoBitsPerSecond = a} :: StreamSelection)
{-# DEPRECATED ssMinVideoBitsPerSecond "Use generic-lens or generic-optics with 'minVideoBitsPerSecond' instead." #-}

-- | The maximum video bitrate (bps) to include in output.
--
-- /Note:/ Consider using 'maxVideoBitsPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssMaxVideoBitsPerSecond :: Lens.Lens' StreamSelection (Lude.Maybe Lude.Int)
ssMaxVideoBitsPerSecond = Lens.lens (maxVideoBitsPerSecond :: StreamSelection -> Lude.Maybe Lude.Int) (\s a -> s {maxVideoBitsPerSecond = a} :: StreamSelection)
{-# DEPRECATED ssMaxVideoBitsPerSecond "Use generic-lens or generic-optics with 'maxVideoBitsPerSecond' instead." #-}

instance Lude.FromJSON StreamSelection where
  parseJSON =
    Lude.withObject
      "StreamSelection"
      ( \x ->
          StreamSelection'
            Lude.<$> (x Lude..:? "streamOrder")
            Lude.<*> (x Lude..:? "minVideoBitsPerSecond")
            Lude.<*> (x Lude..:? "maxVideoBitsPerSecond")
      )

instance Lude.ToJSON StreamSelection where
  toJSON StreamSelection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("streamOrder" Lude..=) Lude.<$> streamOrder,
            ("minVideoBitsPerSecond" Lude..=) Lude.<$> minVideoBitsPerSecond,
            ("maxVideoBitsPerSecond" Lude..=) Lude.<$> maxVideoBitsPerSecond
          ]
      )
