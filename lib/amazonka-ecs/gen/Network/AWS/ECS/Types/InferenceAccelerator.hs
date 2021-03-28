{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.InferenceAccelerator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.InferenceAccelerator
  ( InferenceAccelerator (..)
  -- * Smart constructor
  , mkInferenceAccelerator
  -- * Lenses
  , iaDeviceName
  , iaDeviceType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details on a Elastic Inference accelerator. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-eia.html Working with Amazon Elastic Inference on Amazon ECS> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkInferenceAccelerator' smart constructor.
data InferenceAccelerator = InferenceAccelerator'
  { deviceName :: Core.Text
    -- ^ The Elastic Inference accelerator device name. The @deviceName@ must also be referenced in a container definition as a 'ResourceRequirement' .
  , deviceType :: Core.Text
    -- ^ The Elastic Inference accelerator type to use.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InferenceAccelerator' value with any optional fields omitted.
mkInferenceAccelerator
    :: Core.Text -- ^ 'deviceName'
    -> Core.Text -- ^ 'deviceType'
    -> InferenceAccelerator
mkInferenceAccelerator deviceName deviceType
  = InferenceAccelerator'{deviceName, deviceType}

-- | The Elastic Inference accelerator device name. The @deviceName@ must also be referenced in a container definition as a 'ResourceRequirement' .
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaDeviceName :: Lens.Lens' InferenceAccelerator Core.Text
iaDeviceName = Lens.field @"deviceName"
{-# INLINEABLE iaDeviceName #-}
{-# DEPRECATED deviceName "Use generic-lens or generic-optics with 'deviceName' instead"  #-}

-- | The Elastic Inference accelerator type to use.
--
-- /Note:/ Consider using 'deviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaDeviceType :: Lens.Lens' InferenceAccelerator Core.Text
iaDeviceType = Lens.field @"deviceType"
{-# INLINEABLE iaDeviceType #-}
{-# DEPRECATED deviceType "Use generic-lens or generic-optics with 'deviceType' instead"  #-}

instance Core.FromJSON InferenceAccelerator where
        toJSON InferenceAccelerator{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("deviceName" Core..= deviceName),
                  Core.Just ("deviceType" Core..= deviceType)])

instance Core.FromJSON InferenceAccelerator where
        parseJSON
          = Core.withObject "InferenceAccelerator" Core.$
              \ x ->
                InferenceAccelerator' Core.<$>
                  (x Core..: "deviceName") Core.<*> x Core..: "deviceType"
