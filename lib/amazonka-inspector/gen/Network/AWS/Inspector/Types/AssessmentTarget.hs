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
    aName,
    aCreatedAt,
    aUpdatedAt,
    aResourceGroupArn,
  )
where

import qualified Network.AWS.Inspector.Types.Arn as Types
import qualified Network.AWS.Inspector.Types.AssessmentTargetName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an Amazon Inspector application. This data type is used as the response element in the 'DescribeAssessmentTargets' action.
--
-- /See:/ 'mkAssessmentTarget' smart constructor.
data AssessmentTarget = AssessmentTarget'
  { -- | The ARN that specifies the Amazon Inspector assessment target.
    arn :: Types.Arn,
    -- | The name of the Amazon Inspector assessment target.
    name :: Types.AssessmentTargetName,
    -- | The time at which the assessment target is created.
    createdAt :: Core.NominalDiffTime,
    -- | The time at which 'UpdateAssessmentTarget' is called.
    updatedAt :: Core.NominalDiffTime,
    -- | The ARN that specifies the resource group that is associated with the assessment target.
    resourceGroupArn :: Core.Maybe Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AssessmentTarget' value with any optional fields omitted.
mkAssessmentTarget ::
  -- | 'arn'
  Types.Arn ->
  -- | 'name'
  Types.AssessmentTargetName ->
  -- | 'createdAt'
  Core.NominalDiffTime ->
  -- | 'updatedAt'
  Core.NominalDiffTime ->
  AssessmentTarget
mkAssessmentTarget arn name createdAt updatedAt =
  AssessmentTarget'
    { arn,
      name,
      createdAt,
      updatedAt,
      resourceGroupArn = Core.Nothing
    }

-- | The ARN that specifies the Amazon Inspector assessment target.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aArn :: Lens.Lens' AssessmentTarget Types.Arn
aArn = Lens.field @"arn"
{-# DEPRECATED aArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the Amazon Inspector assessment target.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' AssessmentTarget Types.AssessmentTargetName
aName = Lens.field @"name"
{-# DEPRECATED aName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time at which the assessment target is created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCreatedAt :: Lens.Lens' AssessmentTarget Core.NominalDiffTime
aCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED aCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The time at which 'UpdateAssessmentTarget' is called.
--
-- /Note:/ Consider using 'updatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aUpdatedAt :: Lens.Lens' AssessmentTarget Core.NominalDiffTime
aUpdatedAt = Lens.field @"updatedAt"
{-# DEPRECATED aUpdatedAt "Use generic-lens or generic-optics with 'updatedAt' instead." #-}

-- | The ARN that specifies the resource group that is associated with the assessment target.
--
-- /Note:/ Consider using 'resourceGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aResourceGroupArn :: Lens.Lens' AssessmentTarget (Core.Maybe Types.Arn)
aResourceGroupArn = Lens.field @"resourceGroupArn"
{-# DEPRECATED aResourceGroupArn "Use generic-lens or generic-optics with 'resourceGroupArn' instead." #-}

instance Core.FromJSON AssessmentTarget where
  parseJSON =
    Core.withObject "AssessmentTarget" Core.$
      \x ->
        AssessmentTarget'
          Core.<$> (x Core..: "arn")
          Core.<*> (x Core..: "name")
          Core.<*> (x Core..: "createdAt")
          Core.<*> (x Core..: "updatedAt")
          Core.<*> (x Core..:? "resourceGroupArn")
