{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.StreamNameCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types.StreamNameCondition
  ( StreamNameCondition (..),

    -- * Smart constructor
    mkStreamNameCondition,

    -- * Lenses
    sncComparisonOperator,
    sncComparisonValue,
  )
where

import qualified Network.AWS.KinesisVideo.Types.ComparisonOperator as Types
import qualified Network.AWS.KinesisVideo.Types.ComparisonValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the condition that streams must satisfy to be returned when you list streams (see the @ListStreams@ API). A condition has a comparison operation and a value. Currently, you can specify only the @BEGINS_WITH@ operator, which finds streams whose names start with a given prefix.
--
-- /See:/ 'mkStreamNameCondition' smart constructor.
data StreamNameCondition = StreamNameCondition'
  { -- | A comparison operator. Currently, you can specify only the @BEGINS_WITH@ operator, which finds streams whose names start with a given prefix.
    comparisonOperator :: Core.Maybe Types.ComparisonOperator,
    -- | A value to compare.
    comparisonValue :: Core.Maybe Types.ComparisonValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StreamNameCondition' value with any optional fields omitted.
mkStreamNameCondition ::
  StreamNameCondition
mkStreamNameCondition =
  StreamNameCondition'
    { comparisonOperator = Core.Nothing,
      comparisonValue = Core.Nothing
    }

-- | A comparison operator. Currently, you can specify only the @BEGINS_WITH@ operator, which finds streams whose names start with a given prefix.
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sncComparisonOperator :: Lens.Lens' StreamNameCondition (Core.Maybe Types.ComparisonOperator)
sncComparisonOperator = Lens.field @"comparisonOperator"
{-# DEPRECATED sncComparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead." #-}

-- | A value to compare.
--
-- /Note:/ Consider using 'comparisonValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sncComparisonValue :: Lens.Lens' StreamNameCondition (Core.Maybe Types.ComparisonValue)
sncComparisonValue = Lens.field @"comparisonValue"
{-# DEPRECATED sncComparisonValue "Use generic-lens or generic-optics with 'comparisonValue' instead." #-}

instance Core.FromJSON StreamNameCondition where
  toJSON StreamNameCondition {..} =
    Core.object
      ( Core.catMaybes
          [ ("ComparisonOperator" Core..=) Core.<$> comparisonOperator,
            ("ComparisonValue" Core..=) Core.<$> comparisonValue
          ]
      )
