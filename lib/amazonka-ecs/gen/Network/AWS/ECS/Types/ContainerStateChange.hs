{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ContainerStateChange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ContainerStateChange
  ( ContainerStateChange (..),

    -- * Smart constructor
    mkContainerStateChange,

    -- * Lenses
    cscContainerName,
    cscExitCode,
    cscImageDigest,
    cscNetworkBindings,
    cscReason,
    cscRuntimeId,
    cscStatus,
  )
where

import qualified Network.AWS.ECS.Types.NetworkBinding as Types
import qualified Network.AWS.ECS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing a change in state for a container.
--
-- /See:/ 'mkContainerStateChange' smart constructor.
data ContainerStateChange = ContainerStateChange'
  { -- | The name of the container.
    containerName :: Core.Maybe Types.String,
    -- | The exit code for the container, if the state change is a result of the container exiting.
    exitCode :: Core.Maybe Core.Int,
    -- | The container image SHA 256 digest.
    imageDigest :: Core.Maybe Types.String,
    -- | Any network bindings associated with the container.
    networkBindings :: Core.Maybe [Types.NetworkBinding],
    -- | The reason for the state change.
    reason :: Core.Maybe Types.String,
    -- | The ID of the Docker container.
    runtimeId :: Core.Maybe Types.String,
    -- | The status of the container.
    status :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContainerStateChange' value with any optional fields omitted.
mkContainerStateChange ::
  ContainerStateChange
mkContainerStateChange =
  ContainerStateChange'
    { containerName = Core.Nothing,
      exitCode = Core.Nothing,
      imageDigest = Core.Nothing,
      networkBindings = Core.Nothing,
      reason = Core.Nothing,
      runtimeId = Core.Nothing,
      status = Core.Nothing
    }

-- | The name of the container.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscContainerName :: Lens.Lens' ContainerStateChange (Core.Maybe Types.String)
cscContainerName = Lens.field @"containerName"
{-# DEPRECATED cscContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

-- | The exit code for the container, if the state change is a result of the container exiting.
--
-- /Note:/ Consider using 'exitCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscExitCode :: Lens.Lens' ContainerStateChange (Core.Maybe Core.Int)
cscExitCode = Lens.field @"exitCode"
{-# DEPRECATED cscExitCode "Use generic-lens or generic-optics with 'exitCode' instead." #-}

-- | The container image SHA 256 digest.
--
-- /Note:/ Consider using 'imageDigest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscImageDigest :: Lens.Lens' ContainerStateChange (Core.Maybe Types.String)
cscImageDigest = Lens.field @"imageDigest"
{-# DEPRECATED cscImageDigest "Use generic-lens or generic-optics with 'imageDigest' instead." #-}

-- | Any network bindings associated with the container.
--
-- /Note:/ Consider using 'networkBindings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscNetworkBindings :: Lens.Lens' ContainerStateChange (Core.Maybe [Types.NetworkBinding])
cscNetworkBindings = Lens.field @"networkBindings"
{-# DEPRECATED cscNetworkBindings "Use generic-lens or generic-optics with 'networkBindings' instead." #-}

-- | The reason for the state change.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscReason :: Lens.Lens' ContainerStateChange (Core.Maybe Types.String)
cscReason = Lens.field @"reason"
{-# DEPRECATED cscReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The ID of the Docker container.
--
-- /Note:/ Consider using 'runtimeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscRuntimeId :: Lens.Lens' ContainerStateChange (Core.Maybe Types.String)
cscRuntimeId = Lens.field @"runtimeId"
{-# DEPRECATED cscRuntimeId "Use generic-lens or generic-optics with 'runtimeId' instead." #-}

-- | The status of the container.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscStatus :: Lens.Lens' ContainerStateChange (Core.Maybe Types.String)
cscStatus = Lens.field @"status"
{-# DEPRECATED cscStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON ContainerStateChange where
  toJSON ContainerStateChange {..} =
    Core.object
      ( Core.catMaybes
          [ ("containerName" Core..=) Core.<$> containerName,
            ("exitCode" Core..=) Core.<$> exitCode,
            ("imageDigest" Core..=) Core.<$> imageDigest,
            ("networkBindings" Core..=) Core.<$> networkBindings,
            ("reason" Core..=) Core.<$> reason,
            ("runtimeId" Core..=) Core.<$> runtimeId,
            ("status" Core..=) Core.<$> status
          ]
      )
