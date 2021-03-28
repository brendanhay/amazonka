{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.TaskDefinitionPlacementConstraint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.TaskDefinitionPlacementConstraint
  ( TaskDefinitionPlacementConstraint (..)
  -- * Smart constructor
  , mkTaskDefinitionPlacementConstraint
  -- * Lenses
  , tdpcExpression
  , tdpcType
  ) where

import qualified Network.AWS.ECS.Types.TaskDefinitionPlacementConstraintType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing a constraint on task placement in the task definition. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html Task Placement Constraints> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkTaskDefinitionPlacementConstraint' smart constructor.
data TaskDefinitionPlacementConstraint = TaskDefinitionPlacementConstraint'
  { expression :: Core.Maybe Core.Text
    -- ^ A cluster query language expression to apply to the constraint. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon Elastic Container Service Developer Guide/ .
  , type' :: Core.Maybe Types.TaskDefinitionPlacementConstraintType
    -- ^ The type of constraint. The @MemberOf@ constraint restricts selection to be from a group of valid candidates.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TaskDefinitionPlacementConstraint' value with any optional fields omitted.
mkTaskDefinitionPlacementConstraint
    :: TaskDefinitionPlacementConstraint
mkTaskDefinitionPlacementConstraint
  = TaskDefinitionPlacementConstraint'{expression = Core.Nothing,
                                       type' = Core.Nothing}

-- | A cluster query language expression to apply to the constraint. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdpcExpression :: Lens.Lens' TaskDefinitionPlacementConstraint (Core.Maybe Core.Text)
tdpcExpression = Lens.field @"expression"
{-# INLINEABLE tdpcExpression #-}
{-# DEPRECATED expression "Use generic-lens or generic-optics with 'expression' instead"  #-}

-- | The type of constraint. The @MemberOf@ constraint restricts selection to be from a group of valid candidates.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdpcType :: Lens.Lens' TaskDefinitionPlacementConstraint (Core.Maybe Types.TaskDefinitionPlacementConstraintType)
tdpcType = Lens.field @"type'"
{-# INLINEABLE tdpcType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON TaskDefinitionPlacementConstraint where
        toJSON TaskDefinitionPlacementConstraint{..}
          = Core.object
              (Core.catMaybes
                 [("expression" Core..=) Core.<$> expression,
                  ("type" Core..=) Core.<$> type'])

instance Core.FromJSON TaskDefinitionPlacementConstraint where
        parseJSON
          = Core.withObject "TaskDefinitionPlacementConstraint" Core.$
              \ x ->
                TaskDefinitionPlacementConstraint' Core.<$>
                  (x Core..:? "expression") Core.<*> x Core..:? "type"
