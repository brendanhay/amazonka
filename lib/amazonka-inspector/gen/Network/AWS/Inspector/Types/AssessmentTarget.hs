{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentTarget
  ( AssessmentTarget (..),

    -- * Smart constructor
    mkAssessmentTarget,

    -- * Lenses
    aArn,
    aCreatedAt,
    aResourceGroupARN,
    aName,
    aUpdatedAt,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an Amazon Inspector application. This data type is used as the response element in the 'DescribeAssessmentTargets' action.
--
-- /See:/ 'mkAssessmentTarget' smart constructor.
data AssessmentTarget = AssessmentTarget'
  { -- | The ARN that specifies the Amazon Inspector assessment target.
    arn :: Lude.Text,
    -- | The time at which the assessment target is created.
    createdAt :: Lude.Timestamp,
    -- | The ARN that specifies the resource group that is associated with the assessment target.
    resourceGroupARN :: Lude.Maybe Lude.Text,
    -- | The name of the Amazon Inspector assessment target.
    name :: Lude.Text,
    -- | The time at which 'UpdateAssessmentTarget' is called.
    updatedAt :: Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssessmentTarget' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN that specifies the Amazon Inspector assessment target.
-- * 'createdAt' - The time at which the assessment target is created.
-- * 'resourceGroupARN' - The ARN that specifies the resource group that is associated with the assessment target.
-- * 'name' - The name of the Amazon Inspector assessment target.
-- * 'updatedAt' - The time at which 'UpdateAssessmentTarget' is called.
mkAssessmentTarget ::
  -- | 'arn'
  Lude.Text ->
  -- | 'createdAt'
  Lude.Timestamp ->
  -- | 'name'
  Lude.Text ->
  -- | 'updatedAt'
  Lude.Timestamp ->
  AssessmentTarget
mkAssessmentTarget pArn_ pCreatedAt_ pName_ pUpdatedAt_ =
  AssessmentTarget'
    { arn = pArn_,
      createdAt = pCreatedAt_,
      resourceGroupARN = Lude.Nothing,
      name = pName_,
      updatedAt = pUpdatedAt_
    }

-- | The ARN that specifies the Amazon Inspector assessment target.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aArn :: Lens.Lens' AssessmentTarget Lude.Text
aArn = Lens.lens (arn :: AssessmentTarget -> Lude.Text) (\s a -> s {arn = a} :: AssessmentTarget)
{-# DEPRECATED aArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time at which the assessment target is created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCreatedAt :: Lens.Lens' AssessmentTarget Lude.Timestamp
aCreatedAt = Lens.lens (createdAt :: AssessmentTarget -> Lude.Timestamp) (\s a -> s {createdAt = a} :: AssessmentTarget)
{-# DEPRECATED aCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The ARN that specifies the resource group that is associated with the assessment target.
--
-- /Note:/ Consider using 'resourceGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aResourceGroupARN :: Lens.Lens' AssessmentTarget (Lude.Maybe Lude.Text)
aResourceGroupARN = Lens.lens (resourceGroupARN :: AssessmentTarget -> Lude.Maybe Lude.Text) (\s a -> s {resourceGroupARN = a} :: AssessmentTarget)
{-# DEPRECATED aResourceGroupARN "Use generic-lens or generic-optics with 'resourceGroupARN' instead." #-}

-- | The name of the Amazon Inspector assessment target.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' AssessmentTarget Lude.Text
aName = Lens.lens (name :: AssessmentTarget -> Lude.Text) (\s a -> s {name = a} :: AssessmentTarget)
{-# DEPRECATED aName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time at which 'UpdateAssessmentTarget' is called.
--
-- /Note:/ Consider using 'updatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aUpdatedAt :: Lens.Lens' AssessmentTarget Lude.Timestamp
aUpdatedAt = Lens.lens (updatedAt :: AssessmentTarget -> Lude.Timestamp) (\s a -> s {updatedAt = a} :: AssessmentTarget)
{-# DEPRECATED aUpdatedAt "Use generic-lens or generic-optics with 'updatedAt' instead." #-}

instance Lude.FromJSON AssessmentTarget where
  parseJSON =
    Lude.withObject
      "AssessmentTarget"
      ( \x ->
          AssessmentTarget'
            Lude.<$> (x Lude..: "arn")
            Lude.<*> (x Lude..: "createdAt")
            Lude.<*> (x Lude..:? "resourceGroupArn")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..: "updatedAt")
      )
