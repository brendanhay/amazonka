-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.TaskDefinitionPlacementConstraint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.TaskDefinitionPlacementConstraint
  ( TaskDefinitionPlacementConstraint (..),

    -- * Smart constructor
    mkTaskDefinitionPlacementConstraint,

    -- * Lenses
    tdpcExpression,
    tdpcType,
  )
where

import Network.AWS.ECS.Types.TaskDefinitionPlacementConstraintType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing a constraint on task placement in the task definition. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html Task Placement Constraints> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkTaskDefinitionPlacementConstraint' smart constructor.
data TaskDefinitionPlacementConstraint = TaskDefinitionPlacementConstraint'
  { expression ::
      Lude.Maybe Lude.Text,
    type' ::
      Lude.Maybe
        TaskDefinitionPlacementConstraintType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TaskDefinitionPlacementConstraint' with the minimum fields required to make a request.
--
-- * 'expression' - A cluster query language expression to apply to the constraint. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'type'' - The type of constraint. The @MemberOf@ constraint restricts selection to be from a group of valid candidates.
mkTaskDefinitionPlacementConstraint ::
  TaskDefinitionPlacementConstraint
mkTaskDefinitionPlacementConstraint =
  TaskDefinitionPlacementConstraint'
    { expression = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | A cluster query language expression to apply to the constraint. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdpcExpression :: Lens.Lens' TaskDefinitionPlacementConstraint (Lude.Maybe Lude.Text)
tdpcExpression = Lens.lens (expression :: TaskDefinitionPlacementConstraint -> Lude.Maybe Lude.Text) (\s a -> s {expression = a} :: TaskDefinitionPlacementConstraint)
{-# DEPRECATED tdpcExpression "Use generic-lens or generic-optics with 'expression' instead." #-}

-- | The type of constraint. The @MemberOf@ constraint restricts selection to be from a group of valid candidates.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdpcType :: Lens.Lens' TaskDefinitionPlacementConstraint (Lude.Maybe TaskDefinitionPlacementConstraintType)
tdpcType = Lens.lens (type' :: TaskDefinitionPlacementConstraint -> Lude.Maybe TaskDefinitionPlacementConstraintType) (\s a -> s {type' = a} :: TaskDefinitionPlacementConstraint)
{-# DEPRECATED tdpcType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON TaskDefinitionPlacementConstraint where
  parseJSON =
    Lude.withObject
      "TaskDefinitionPlacementConstraint"
      ( \x ->
          TaskDefinitionPlacementConstraint'
            Lude.<$> (x Lude..:? "expression") Lude.<*> (x Lude..:? "type")
      )

instance Lude.ToJSON TaskDefinitionPlacementConstraint where
  toJSON TaskDefinitionPlacementConstraint' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("expression" Lude..=) Lude.<$> expression,
            ("type" Lude..=) Lude.<$> type'
          ]
      )
