{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ResourceSpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ResourceSpec
  ( ResourceSpec (..)
  -- * Smart constructor
  , mkResourceSpec
  -- * Lenses
  , rsInstanceType
  , rsSageMakerImageArn
  , rsSageMakerImageVersionArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.AppInstanceType as Types
import qualified Network.AWS.SageMaker.Types.SageMakerImageArn as Types
import qualified Network.AWS.SageMaker.Types.SageMakerImageVersionArn as Types

-- | Specifies the ARN's of a SageMaker image and SageMaker image version, and the instance type that the version runs on.
--
-- /See:/ 'mkResourceSpec' smart constructor.
data ResourceSpec = ResourceSpec'
  { instanceType :: Core.Maybe Types.AppInstanceType
    -- ^ The instance type that the image version runs on.
  , sageMakerImageArn :: Core.Maybe Types.SageMakerImageArn
    -- ^ The ARN of the SageMaker image that the image version belongs to.
  , sageMakerImageVersionArn :: Core.Maybe Types.SageMakerImageVersionArn
    -- ^ The ARN of the image version created on the instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceSpec' value with any optional fields omitted.
mkResourceSpec
    :: ResourceSpec
mkResourceSpec
  = ResourceSpec'{instanceType = Core.Nothing,
                  sageMakerImageArn = Core.Nothing,
                  sageMakerImageVersionArn = Core.Nothing}

-- | The instance type that the image version runs on.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsInstanceType :: Lens.Lens' ResourceSpec (Core.Maybe Types.AppInstanceType)
rsInstanceType = Lens.field @"instanceType"
{-# INLINEABLE rsInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The ARN of the SageMaker image that the image version belongs to.
--
-- /Note:/ Consider using 'sageMakerImageArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsSageMakerImageArn :: Lens.Lens' ResourceSpec (Core.Maybe Types.SageMakerImageArn)
rsSageMakerImageArn = Lens.field @"sageMakerImageArn"
{-# INLINEABLE rsSageMakerImageArn #-}
{-# DEPRECATED sageMakerImageArn "Use generic-lens or generic-optics with 'sageMakerImageArn' instead"  #-}

-- | The ARN of the image version created on the instance.
--
-- /Note:/ Consider using 'sageMakerImageVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsSageMakerImageVersionArn :: Lens.Lens' ResourceSpec (Core.Maybe Types.SageMakerImageVersionArn)
rsSageMakerImageVersionArn = Lens.field @"sageMakerImageVersionArn"
{-# INLINEABLE rsSageMakerImageVersionArn #-}
{-# DEPRECATED sageMakerImageVersionArn "Use generic-lens or generic-optics with 'sageMakerImageVersionArn' instead"  #-}

instance Core.FromJSON ResourceSpec where
        toJSON ResourceSpec{..}
          = Core.object
              (Core.catMaybes
                 [("InstanceType" Core..=) Core.<$> instanceType,
                  ("SageMakerImageArn" Core..=) Core.<$> sageMakerImageArn,
                  ("SageMakerImageVersionArn" Core..=) Core.<$>
                    sageMakerImageVersionArn])

instance Core.FromJSON ResourceSpec where
        parseJSON
          = Core.withObject "ResourceSpec" Core.$
              \ x ->
                ResourceSpec' Core.<$>
                  (x Core..:? "InstanceType") Core.<*> x Core..:? "SageMakerImageArn"
                    Core.<*> x Core..:? "SageMakerImageVersionArn"
