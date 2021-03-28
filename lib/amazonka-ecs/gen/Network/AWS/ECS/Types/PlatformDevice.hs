{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.PlatformDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.PlatformDevice
  ( PlatformDevice (..)
  -- * Smart constructor
  , mkPlatformDevice
  -- * Lenses
  , pdId
  , pdType
  ) where

import qualified Network.AWS.ECS.Types.PlatformDeviceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The devices that are available on the container instance. The only supported device type is a GPU.
--
-- /See:/ 'mkPlatformDevice' smart constructor.
data PlatformDevice = PlatformDevice'
  { id :: Core.Text
    -- ^ The ID for the GPU(s) on the container instance. The available GPU IDs can also be obtained on the container instance in the @/var/lib/ecs/gpu/nvidia_gpu_info.json@ file.
  , type' :: Types.PlatformDeviceType
    -- ^ The type of device that is available on the container instance. The only supported value is @GPU@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PlatformDevice' value with any optional fields omitted.
mkPlatformDevice
    :: Core.Text -- ^ 'id'
    -> Types.PlatformDeviceType -- ^ 'type\''
    -> PlatformDevice
mkPlatformDevice id type' = PlatformDevice'{id, type'}

-- | The ID for the GPU(s) on the container instance. The available GPU IDs can also be obtained on the container instance in the @/var/lib/ecs/gpu/nvidia_gpu_info.json@ file.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdId :: Lens.Lens' PlatformDevice Core.Text
pdId = Lens.field @"id"
{-# INLINEABLE pdId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The type of device that is available on the container instance. The only supported value is @GPU@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdType :: Lens.Lens' PlatformDevice Types.PlatformDeviceType
pdType = Lens.field @"type'"
{-# INLINEABLE pdType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON PlatformDevice where
        toJSON PlatformDevice{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("id" Core..= id), Core.Just ("type" Core..= type')])
