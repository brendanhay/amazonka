{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.DurationRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.DurationRange
  ( DurationRange (..),

    -- * Smart constructor
    mkDurationRange,

    -- * Lenses
    drMinSeconds,
    drMaxSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This data type is used in the 'AssessmentTemplateFilter' data type.
--
-- /See:/ 'mkDurationRange' smart constructor.
data DurationRange = DurationRange'
  { minSeconds ::
      Lude.Maybe Lude.Natural,
    maxSeconds :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DurationRange' with the minimum fields required to make a request.
--
-- * 'maxSeconds' - The maximum value of the duration range. Must be less than or equal to 604800 seconds (1 week).
-- * 'minSeconds' - The minimum value of the duration range. Must be greater than zero.
mkDurationRange ::
  DurationRange
mkDurationRange =
  DurationRange'
    { minSeconds = Lude.Nothing,
      maxSeconds = Lude.Nothing
    }

-- | The minimum value of the duration range. Must be greater than zero.
--
-- /Note:/ Consider using 'minSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drMinSeconds :: Lens.Lens' DurationRange (Lude.Maybe Lude.Natural)
drMinSeconds = Lens.lens (minSeconds :: DurationRange -> Lude.Maybe Lude.Natural) (\s a -> s {minSeconds = a} :: DurationRange)
{-# DEPRECATED drMinSeconds "Use generic-lens or generic-optics with 'minSeconds' instead." #-}

-- | The maximum value of the duration range. Must be less than or equal to 604800 seconds (1 week).
--
-- /Note:/ Consider using 'maxSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drMaxSeconds :: Lens.Lens' DurationRange (Lude.Maybe Lude.Natural)
drMaxSeconds = Lens.lens (maxSeconds :: DurationRange -> Lude.Maybe Lude.Natural) (\s a -> s {maxSeconds = a} :: DurationRange)
{-# DEPRECATED drMaxSeconds "Use generic-lens or generic-optics with 'maxSeconds' instead." #-}

instance Lude.ToJSON DurationRange where
  toJSON DurationRange' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("minSeconds" Lude..=) Lude.<$> minSeconds,
            ("maxSeconds" Lude..=) Lude.<$> maxSeconds
          ]
      )
