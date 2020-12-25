{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.ChannelNameCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types.ChannelNameCondition
  ( ChannelNameCondition (..),

    -- * Smart constructor
    mkChannelNameCondition,

    -- * Lenses
    cncComparisonOperator,
    cncComparisonValue,
  )
where

import qualified Network.AWS.KinesisVideo.Types.ComparisonOperator as Types
import qualified Network.AWS.KinesisVideo.Types.ComparisonValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An optional input parameter for the @ListSignalingChannels@ API. When this parameter is specified while invoking @ListSignalingChannels@ , the API returns only the channels that satisfy a condition specified in @ChannelNameCondition@ .
--
-- /See:/ 'mkChannelNameCondition' smart constructor.
data ChannelNameCondition = ChannelNameCondition'
  { -- | A comparison operator. Currently, you can only specify the @BEGINS_WITH@ operator, which finds signaling channels whose names begin with a given prefix.
    comparisonOperator :: Core.Maybe Types.ComparisonOperator,
    -- | A value to compare.
    comparisonValue :: Core.Maybe Types.ComparisonValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChannelNameCondition' value with any optional fields omitted.
mkChannelNameCondition ::
  ChannelNameCondition
mkChannelNameCondition =
  ChannelNameCondition'
    { comparisonOperator = Core.Nothing,
      comparisonValue = Core.Nothing
    }

-- | A comparison operator. Currently, you can only specify the @BEGINS_WITH@ operator, which finds signaling channels whose names begin with a given prefix.
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cncComparisonOperator :: Lens.Lens' ChannelNameCondition (Core.Maybe Types.ComparisonOperator)
cncComparisonOperator = Lens.field @"comparisonOperator"
{-# DEPRECATED cncComparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead." #-}

-- | A value to compare.
--
-- /Note:/ Consider using 'comparisonValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cncComparisonValue :: Lens.Lens' ChannelNameCondition (Core.Maybe Types.ComparisonValue)
cncComparisonValue = Lens.field @"comparisonValue"
{-# DEPRECATED cncComparisonValue "Use generic-lens or generic-optics with 'comparisonValue' instead." #-}

instance Core.FromJSON ChannelNameCondition where
  toJSON ChannelNameCondition {..} =
    Core.object
      ( Core.catMaybes
          [ ("ComparisonOperator" Core..=) Core.<$> comparisonOperator,
            ("ComparisonValue" Core..=) Core.<$> comparisonValue
          ]
      )
