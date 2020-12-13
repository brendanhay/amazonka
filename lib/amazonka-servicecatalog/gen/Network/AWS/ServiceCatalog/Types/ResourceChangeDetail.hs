{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ResourceChangeDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ResourceChangeDetail
  ( ResourceChangeDetail (..),

    -- * Smart constructor
    mkResourceChangeDetail,

    -- * Lenses
    rcdCausingEntity,
    rcdEvaluation,
    rcdTarget,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServiceCatalog.Types.EvaluationType
import Network.AWS.ServiceCatalog.Types.ResourceTargetDefinition

-- | Information about a change to a resource attribute.
--
-- /See:/ 'mkResourceChangeDetail' smart constructor.
data ResourceChangeDetail = ResourceChangeDetail'
  { -- | The ID of the entity that caused the change.
    causingEntity :: Lude.Maybe Lude.Text,
    -- | For static evaluations, the value of the resource attribute will change and the new value is known. For dynamic evaluations, the value might change, and any new value will be determined when the plan is updated.
    evaluation :: Lude.Maybe EvaluationType,
    -- | Information about the resource attribute to be modified.
    target :: Lude.Maybe ResourceTargetDefinition
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceChangeDetail' with the minimum fields required to make a request.
--
-- * 'causingEntity' - The ID of the entity that caused the change.
-- * 'evaluation' - For static evaluations, the value of the resource attribute will change and the new value is known. For dynamic evaluations, the value might change, and any new value will be determined when the plan is updated.
-- * 'target' - Information about the resource attribute to be modified.
mkResourceChangeDetail ::
  ResourceChangeDetail
mkResourceChangeDetail =
  ResourceChangeDetail'
    { causingEntity = Lude.Nothing,
      evaluation = Lude.Nothing,
      target = Lude.Nothing
    }

-- | The ID of the entity that caused the change.
--
-- /Note:/ Consider using 'causingEntity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcdCausingEntity :: Lens.Lens' ResourceChangeDetail (Lude.Maybe Lude.Text)
rcdCausingEntity = Lens.lens (causingEntity :: ResourceChangeDetail -> Lude.Maybe Lude.Text) (\s a -> s {causingEntity = a} :: ResourceChangeDetail)
{-# DEPRECATED rcdCausingEntity "Use generic-lens or generic-optics with 'causingEntity' instead." #-}

-- | For static evaluations, the value of the resource attribute will change and the new value is known. For dynamic evaluations, the value might change, and any new value will be determined when the plan is updated.
--
-- /Note:/ Consider using 'evaluation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcdEvaluation :: Lens.Lens' ResourceChangeDetail (Lude.Maybe EvaluationType)
rcdEvaluation = Lens.lens (evaluation :: ResourceChangeDetail -> Lude.Maybe EvaluationType) (\s a -> s {evaluation = a} :: ResourceChangeDetail)
{-# DEPRECATED rcdEvaluation "Use generic-lens or generic-optics with 'evaluation' instead." #-}

-- | Information about the resource attribute to be modified.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcdTarget :: Lens.Lens' ResourceChangeDetail (Lude.Maybe ResourceTargetDefinition)
rcdTarget = Lens.lens (target :: ResourceChangeDetail -> Lude.Maybe ResourceTargetDefinition) (\s a -> s {target = a} :: ResourceChangeDetail)
{-# DEPRECATED rcdTarget "Use generic-lens or generic-optics with 'target' instead." #-}

instance Lude.FromJSON ResourceChangeDetail where
  parseJSON =
    Lude.withObject
      "ResourceChangeDetail"
      ( \x ->
          ResourceChangeDetail'
            Lude.<$> (x Lude..:? "CausingEntity")
            Lude.<*> (x Lude..:? "Evaluation")
            Lude.<*> (x Lude..:? "Target")
      )
