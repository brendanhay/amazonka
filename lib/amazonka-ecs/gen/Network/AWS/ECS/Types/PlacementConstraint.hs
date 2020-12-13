{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.PlacementConstraint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.PlacementConstraint
  ( PlacementConstraint (..),

    -- * Smart constructor
    mkPlacementConstraint,

    -- * Lenses
    pcExpression,
    pcType,
  )
where

import Network.AWS.ECS.Types.PlacementConstraintType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing a constraint on task placement. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html Task Placement Constraints> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkPlacementConstraint' smart constructor.
data PlacementConstraint = PlacementConstraint'
  { -- | A cluster query language expression to apply to the constraint. You cannot specify an expression if the constraint type is @distinctInstance@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon Elastic Container Service Developer Guide/ .
    expression :: Lude.Maybe Lude.Text,
    -- | The type of constraint. Use @distinctInstance@ to ensure that each task in a particular group is running on a different container instance. Use @memberOf@ to restrict the selection to a group of valid candidates.
    type' :: Lude.Maybe PlacementConstraintType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PlacementConstraint' with the minimum fields required to make a request.
--
-- * 'expression' - A cluster query language expression to apply to the constraint. You cannot specify an expression if the constraint type is @distinctInstance@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon Elastic Container Service Developer Guide/ .
-- * 'type'' - The type of constraint. Use @distinctInstance@ to ensure that each task in a particular group is running on a different container instance. Use @memberOf@ to restrict the selection to a group of valid candidates.
mkPlacementConstraint ::
  PlacementConstraint
mkPlacementConstraint =
  PlacementConstraint'
    { expression = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | A cluster query language expression to apply to the constraint. You cannot specify an expression if the constraint type is @distinctInstance@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcExpression :: Lens.Lens' PlacementConstraint (Lude.Maybe Lude.Text)
pcExpression = Lens.lens (expression :: PlacementConstraint -> Lude.Maybe Lude.Text) (\s a -> s {expression = a} :: PlacementConstraint)
{-# DEPRECATED pcExpression "Use generic-lens or generic-optics with 'expression' instead." #-}

-- | The type of constraint. Use @distinctInstance@ to ensure that each task in a particular group is running on a different container instance. Use @memberOf@ to restrict the selection to a group of valid candidates.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcType :: Lens.Lens' PlacementConstraint (Lude.Maybe PlacementConstraintType)
pcType = Lens.lens (type' :: PlacementConstraint -> Lude.Maybe PlacementConstraintType) (\s a -> s {type' = a} :: PlacementConstraint)
{-# DEPRECATED pcType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON PlacementConstraint where
  parseJSON =
    Lude.withObject
      "PlacementConstraint"
      ( \x ->
          PlacementConstraint'
            Lude.<$> (x Lude..:? "expression") Lude.<*> (x Lude..:? "type")
      )

instance Lude.ToJSON PlacementConstraint where
  toJSON PlacementConstraint' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("expression" Lude..=) Lude.<$> expression,
            ("type" Lude..=) Lude.<$> type'
          ]
      )
