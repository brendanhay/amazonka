{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.PlacementConstraint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.PlacementConstraint
  ( PlacementConstraint (..)
  -- * Smart constructor
  , mkPlacementConstraint
  -- * Lenses
  , pcExpression
  , pcType
  ) where

import qualified Network.AWS.ECS.Types.PlacementConstraintType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing a constraint on task placement. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html Task Placement Constraints> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkPlacementConstraint' smart constructor.
data PlacementConstraint = PlacementConstraint'
  { expression :: Core.Maybe Core.Text
    -- ^ A cluster query language expression to apply to the constraint. You cannot specify an expression if the constraint type is @distinctInstance@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon Elastic Container Service Developer Guide/ .
  , type' :: Core.Maybe Types.PlacementConstraintType
    -- ^ The type of constraint. Use @distinctInstance@ to ensure that each task in a particular group is running on a different container instance. Use @memberOf@ to restrict the selection to a group of valid candidates.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PlacementConstraint' value with any optional fields omitted.
mkPlacementConstraint
    :: PlacementConstraint
mkPlacementConstraint
  = PlacementConstraint'{expression = Core.Nothing,
                         type' = Core.Nothing}

-- | A cluster query language expression to apply to the constraint. You cannot specify an expression if the constraint type is @distinctInstance@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcExpression :: Lens.Lens' PlacementConstraint (Core.Maybe Core.Text)
pcExpression = Lens.field @"expression"
{-# INLINEABLE pcExpression #-}
{-# DEPRECATED expression "Use generic-lens or generic-optics with 'expression' instead"  #-}

-- | The type of constraint. Use @distinctInstance@ to ensure that each task in a particular group is running on a different container instance. Use @memberOf@ to restrict the selection to a group of valid candidates.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcType :: Lens.Lens' PlacementConstraint (Core.Maybe Types.PlacementConstraintType)
pcType = Lens.field @"type'"
{-# INLINEABLE pcType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON PlacementConstraint where
        toJSON PlacementConstraint{..}
          = Core.object
              (Core.catMaybes
                 [("expression" Core..=) Core.<$> expression,
                  ("type" Core..=) Core.<$> type'])

instance Core.FromJSON PlacementConstraint where
        parseJSON
          = Core.withObject "PlacementConstraint" Core.$
              \ x ->
                PlacementConstraint' Core.<$>
                  (x Core..:? "expression") Core.<*> x Core..:? "type"
