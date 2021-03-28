{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.InferenceAcceleratorOverride
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.InferenceAcceleratorOverride
  ( InferenceAcceleratorOverride (..)
  -- * Smart constructor
  , mkInferenceAcceleratorOverride
  -- * Lenses
  , iaoDeviceName
  , iaoDeviceType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details on an Elastic Inference accelerator task override. This parameter is used to override the Elastic Inference accelerator specified in the task definition. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-eia.html Working with Amazon Elastic Inference on Amazon ECS> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkInferenceAcceleratorOverride' smart constructor.
data InferenceAcceleratorOverride = InferenceAcceleratorOverride'
  { deviceName :: Core.Maybe Core.Text
    -- ^ The Elastic Inference accelerator device name to override for the task. This parameter must match a @deviceName@ specified in the task definition.
  , deviceType :: Core.Maybe Core.Text
    -- ^ The Elastic Inference accelerator type to use.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InferenceAcceleratorOverride' value with any optional fields omitted.
mkInferenceAcceleratorOverride
    :: InferenceAcceleratorOverride
mkInferenceAcceleratorOverride
  = InferenceAcceleratorOverride'{deviceName = Core.Nothing,
                                  deviceType = Core.Nothing}

-- | The Elastic Inference accelerator device name to override for the task. This parameter must match a @deviceName@ specified in the task definition.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaoDeviceName :: Lens.Lens' InferenceAcceleratorOverride (Core.Maybe Core.Text)
iaoDeviceName = Lens.field @"deviceName"
{-# INLINEABLE iaoDeviceName #-}
{-# DEPRECATED deviceName "Use generic-lens or generic-optics with 'deviceName' instead"  #-}

-- | The Elastic Inference accelerator type to use.
--
-- /Note:/ Consider using 'deviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaoDeviceType :: Lens.Lens' InferenceAcceleratorOverride (Core.Maybe Core.Text)
iaoDeviceType = Lens.field @"deviceType"
{-# INLINEABLE iaoDeviceType #-}
{-# DEPRECATED deviceType "Use generic-lens or generic-optics with 'deviceType' instead"  #-}

instance Core.FromJSON InferenceAcceleratorOverride where
        toJSON InferenceAcceleratorOverride{..}
          = Core.object
              (Core.catMaybes
                 [("deviceName" Core..=) Core.<$> deviceName,
                  ("deviceType" Core..=) Core.<$> deviceType])

instance Core.FromJSON InferenceAcceleratorOverride where
        parseJSON
          = Core.withObject "InferenceAcceleratorOverride" Core.$
              \ x ->
                InferenceAcceleratorOverride' Core.<$>
                  (x Core..:? "deviceName") Core.<*> x Core..:? "deviceType"
