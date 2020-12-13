{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.RecencyDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.RecencyDimension
  ( RecencyDimension (..),

    -- * Smart constructor
    mkRecencyDimension,

    -- * Lenses
    rdRecencyType,
    rdDuration,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Duration
import Network.AWS.Pinpoint.Types.RecencyType
import qualified Network.AWS.Prelude as Lude

-- | Specifies criteria for including or excluding endpoints from a segment based on how recently an endpoint was active.
--
-- /See:/ 'mkRecencyDimension' smart constructor.
data RecencyDimension = RecencyDimension'
  { -- | The type of recency dimension to use for the segment. Valid values are: ACTIVE, endpoints that were active within the specified duration are included in the segment; and, INACTIVE, endpoints that weren't active within the specified duration are included in the segment.
    recencyType :: RecencyType,
    -- | The duration to use when determining whether an endpoint is active or inactive.
    duration :: Duration
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RecencyDimension' with the minimum fields required to make a request.
--
-- * 'recencyType' - The type of recency dimension to use for the segment. Valid values are: ACTIVE, endpoints that were active within the specified duration are included in the segment; and, INACTIVE, endpoints that weren't active within the specified duration are included in the segment.
-- * 'duration' - The duration to use when determining whether an endpoint is active or inactive.
mkRecencyDimension ::
  -- | 'recencyType'
  RecencyType ->
  -- | 'duration'
  Duration ->
  RecencyDimension
mkRecencyDimension pRecencyType_ pDuration_ =
  RecencyDimension'
    { recencyType = pRecencyType_,
      duration = pDuration_
    }

-- | The type of recency dimension to use for the segment. Valid values are: ACTIVE, endpoints that were active within the specified duration are included in the segment; and, INACTIVE, endpoints that weren't active within the specified duration are included in the segment.
--
-- /Note:/ Consider using 'recencyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdRecencyType :: Lens.Lens' RecencyDimension RecencyType
rdRecencyType = Lens.lens (recencyType :: RecencyDimension -> RecencyType) (\s a -> s {recencyType = a} :: RecencyDimension)
{-# DEPRECATED rdRecencyType "Use generic-lens or generic-optics with 'recencyType' instead." #-}

-- | The duration to use when determining whether an endpoint is active or inactive.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdDuration :: Lens.Lens' RecencyDimension Duration
rdDuration = Lens.lens (duration :: RecencyDimension -> Duration) (\s a -> s {duration = a} :: RecencyDimension)
{-# DEPRECATED rdDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

instance Lude.FromJSON RecencyDimension where
  parseJSON =
    Lude.withObject
      "RecencyDimension"
      ( \x ->
          RecencyDimension'
            Lude.<$> (x Lude..: "RecencyType") Lude.<*> (x Lude..: "Duration")
      )

instance Lude.ToJSON RecencyDimension where
  toJSON RecencyDimension' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("RecencyType" Lude..= recencyType),
            Lude.Just ("Duration" Lude..= duration)
          ]
      )
