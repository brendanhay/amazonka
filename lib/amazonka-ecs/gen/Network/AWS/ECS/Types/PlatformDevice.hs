{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.PlatformDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.PlatformDevice
  ( PlatformDevice (..),

    -- * Smart constructor
    mkPlatformDevice,

    -- * Lenses
    pdId,
    pdType,
  )
where

import Network.AWS.ECS.Types.PlatformDeviceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The devices that are available on the container instance. The only supported device type is a GPU.
--
-- /See:/ 'mkPlatformDevice' smart constructor.
data PlatformDevice = PlatformDevice'
  { id :: Lude.Text,
    type' :: PlatformDeviceType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PlatformDevice' with the minimum fields required to make a request.
--
-- * 'id' - The ID for the GPU(s) on the container instance. The available GPU IDs can also be obtained on the container instance in the @/var/lib/ecs/gpu/nvidia_gpu_info.json@ file.
-- * 'type'' - The type of device that is available on the container instance. The only supported value is @GPU@ .
mkPlatformDevice ::
  -- | 'id'
  Lude.Text ->
  -- | 'type''
  PlatformDeviceType ->
  PlatformDevice
mkPlatformDevice pId_ pType_ =
  PlatformDevice' {id = pId_, type' = pType_}

-- | The ID for the GPU(s) on the container instance. The available GPU IDs can also be obtained on the container instance in the @/var/lib/ecs/gpu/nvidia_gpu_info.json@ file.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdId :: Lens.Lens' PlatformDevice Lude.Text
pdId = Lens.lens (id :: PlatformDevice -> Lude.Text) (\s a -> s {id = a} :: PlatformDevice)
{-# DEPRECATED pdId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of device that is available on the container instance. The only supported value is @GPU@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdType :: Lens.Lens' PlatformDevice PlatformDeviceType
pdType = Lens.lens (type' :: PlatformDevice -> PlatformDeviceType) (\s a -> s {type' = a} :: PlatformDevice)
{-# DEPRECATED pdType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.ToJSON PlatformDevice where
  toJSON PlatformDevice' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("id" Lude..= id), Lude.Just ("type" Lude..= type')]
      )
