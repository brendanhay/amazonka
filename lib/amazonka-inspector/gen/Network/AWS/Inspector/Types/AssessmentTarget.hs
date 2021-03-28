{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.AssessmentTarget
  ( AssessmentTarget (..)
  -- * Smart constructor
  , mkAssessmentTarget
  -- * Lenses
  , aArn
  , aName
  , aCreatedAt
  , aUpdatedAt
  , aResourceGroupArn
  ) where

import qualified Network.AWS.Inspector.Types.Arn as Types
import qualified Network.AWS.Inspector.Types.AssessmentTargetName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an Amazon Inspector application. This data type is used as the response element in the 'DescribeAssessmentTargets' action.
--
-- /See:/ 'mkAssessmentTarget' smart constructor.
data AssessmentTarget = AssessmentTarget'
  { arn :: Types.Arn
    -- ^ The ARN that specifies the Amazon Inspector assessment target.
  , name :: Types.AssessmentTargetName
    -- ^ The name of the Amazon Inspector assessment target.
  , createdAt :: Core.NominalDiffTime
    -- ^ The time at which the assessment target is created.
  , updatedAt :: Core.NominalDiffTime
    -- ^ The time at which 'UpdateAssessmentTarget' is called.
  , resourceGroupArn :: Core.Maybe Types.Arn
    -- ^ The ARN that specifies the resource group that is associated with the assessment target.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AssessmentTarget' value with any optional fields omitted.
mkAssessmentTarget
    :: Types.Arn -- ^ 'arn'
    -> Types.AssessmentTargetName -- ^ 'name'
    -> Core.NominalDiffTime -- ^ 'createdAt'
    -> Core.NominalDiffTime -- ^ 'updatedAt'
    -> AssessmentTarget
mkAssessmentTarget arn name createdAt updatedAt
  = AssessmentTarget'{arn, name, createdAt, updatedAt,
                      resourceGroupArn = Core.Nothing}

-- | The ARN that specifies the Amazon Inspector assessment target.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aArn :: Lens.Lens' AssessmentTarget Types.Arn
aArn = Lens.field @"arn"
{-# INLINEABLE aArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The name of the Amazon Inspector assessment target.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' AssessmentTarget Types.AssessmentTargetName
aName = Lens.field @"name"
{-# INLINEABLE aName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The time at which the assessment target is created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCreatedAt :: Lens.Lens' AssessmentTarget Core.NominalDiffTime
aCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE aCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The time at which 'UpdateAssessmentTarget' is called.
--
-- /Note:/ Consider using 'updatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aUpdatedAt :: Lens.Lens' AssessmentTarget Core.NominalDiffTime
aUpdatedAt = Lens.field @"updatedAt"
{-# INLINEABLE aUpdatedAt #-}
{-# DEPRECATED updatedAt "Use generic-lens or generic-optics with 'updatedAt' instead"  #-}

-- | The ARN that specifies the resource group that is associated with the assessment target.
--
-- /Note:/ Consider using 'resourceGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aResourceGroupArn :: Lens.Lens' AssessmentTarget (Core.Maybe Types.Arn)
aResourceGroupArn = Lens.field @"resourceGroupArn"
{-# INLINEABLE aResourceGroupArn #-}
{-# DEPRECATED resourceGroupArn "Use generic-lens or generic-optics with 'resourceGroupArn' instead"  #-}

instance Core.FromJSON AssessmentTarget where
        parseJSON
          = Core.withObject "AssessmentTarget" Core.$
              \ x ->
                AssessmentTarget' Core.<$>
                  (x Core..: "arn") Core.<*> x Core..: "name" Core.<*>
                    x Core..: "createdAt"
                    Core.<*> x Core..: "updatedAt"
                    Core.<*> x Core..:? "resourceGroupArn"
