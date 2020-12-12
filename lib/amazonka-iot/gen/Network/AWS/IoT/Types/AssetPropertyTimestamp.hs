{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AssetPropertyTimestamp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AssetPropertyTimestamp
  ( AssetPropertyTimestamp (..),

    -- * Smart constructor
    mkAssetPropertyTimestamp,

    -- * Lenses
    aptOffsetInNanos,
    aptTimeInSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An asset property timestamp entry containing the following information.
--
-- /See:/ 'mkAssetPropertyTimestamp' smart constructor.
data AssetPropertyTimestamp = AssetPropertyTimestamp'
  { offsetInNanos ::
      Lude.Maybe Lude.Text,
    timeInSeconds :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssetPropertyTimestamp' with the minimum fields required to make a request.
--
-- * 'offsetInNanos' - Optional. A string that contains the nanosecond time offset. Accepts substitution templates.
-- * 'timeInSeconds' - A string that contains the time in seconds since epoch. Accepts substitution templates.
mkAssetPropertyTimestamp ::
  -- | 'timeInSeconds'
  Lude.Text ->
  AssetPropertyTimestamp
mkAssetPropertyTimestamp pTimeInSeconds_ =
  AssetPropertyTimestamp'
    { offsetInNanos = Lude.Nothing,
      timeInSeconds = pTimeInSeconds_
    }

-- | Optional. A string that contains the nanosecond time offset. Accepts substitution templates.
--
-- /Note:/ Consider using 'offsetInNanos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aptOffsetInNanos :: Lens.Lens' AssetPropertyTimestamp (Lude.Maybe Lude.Text)
aptOffsetInNanos = Lens.lens (offsetInNanos :: AssetPropertyTimestamp -> Lude.Maybe Lude.Text) (\s a -> s {offsetInNanos = a} :: AssetPropertyTimestamp)
{-# DEPRECATED aptOffsetInNanos "Use generic-lens or generic-optics with 'offsetInNanos' instead." #-}

-- | A string that contains the time in seconds since epoch. Accepts substitution templates.
--
-- /Note:/ Consider using 'timeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aptTimeInSeconds :: Lens.Lens' AssetPropertyTimestamp Lude.Text
aptTimeInSeconds = Lens.lens (timeInSeconds :: AssetPropertyTimestamp -> Lude.Text) (\s a -> s {timeInSeconds = a} :: AssetPropertyTimestamp)
{-# DEPRECATED aptTimeInSeconds "Use generic-lens or generic-optics with 'timeInSeconds' instead." #-}

instance Lude.FromJSON AssetPropertyTimestamp where
  parseJSON =
    Lude.withObject
      "AssetPropertyTimestamp"
      ( \x ->
          AssetPropertyTimestamp'
            Lude.<$> (x Lude..:? "offsetInNanos") Lude.<*> (x Lude..: "timeInSeconds")
      )

instance Lude.ToJSON AssetPropertyTimestamp where
  toJSON AssetPropertyTimestamp' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("offsetInNanos" Lude..=) Lude.<$> offsetInNanos,
            Lude.Just ("timeInSeconds" Lude..= timeInSeconds)
          ]
      )
