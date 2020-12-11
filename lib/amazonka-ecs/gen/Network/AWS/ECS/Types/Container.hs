-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Container
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Container
  ( Container (..),

    -- * Smart constructor
    mkContainer,

    -- * Lenses
    cGpuIds,
    cNetworkBindings,
    cImage,
    cContainerARN,
    cNetworkInterfaces,
    cTaskARN,
    cLastStatus,
    cMemory,
    cReason,
    cName,
    cImageDigest,
    cExitCode,
    cHealthStatus,
    cCpu,
    cRuntimeId,
    cMemoryReservation,
  )
where

import Network.AWS.ECS.Types.HealthStatus
import Network.AWS.ECS.Types.NetworkBinding
import Network.AWS.ECS.Types.NetworkInterface
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A Docker container that is part of a task.
--
-- /See:/ 'mkContainer' smart constructor.
data Container = Container'
  { gpuIds :: Lude.Maybe [Lude.Text],
    networkBindings :: Lude.Maybe [NetworkBinding],
    image :: Lude.Maybe Lude.Text,
    containerARN :: Lude.Maybe Lude.Text,
    networkInterfaces :: Lude.Maybe [NetworkInterface],
    taskARN :: Lude.Maybe Lude.Text,
    lastStatus :: Lude.Maybe Lude.Text,
    memory :: Lude.Maybe Lude.Text,
    reason :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    imageDigest :: Lude.Maybe Lude.Text,
    exitCode :: Lude.Maybe Lude.Int,
    healthStatus :: Lude.Maybe HealthStatus,
    cpu :: Lude.Maybe Lude.Text,
    runtimeId :: Lude.Maybe Lude.Text,
    memoryReservation :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Container' with the minimum fields required to make a request.
--
-- * 'containerARN' - The Amazon Resource Name (ARN) of the container.
-- * 'cpu' - The number of CPU units set for the container. The value will be @0@ if no value was specified in the container definition when the task definition was registered.
-- * 'exitCode' - The exit code returned from the container.
-- * 'gpuIds' - The IDs of each GPU assigned to the container.
-- * 'healthStatus' - The health status of the container. If health checks are not configured for this container in its task definition, then it reports the health status as @UNKNOWN@ .
-- * 'image' - The image used for the container.
-- * 'imageDigest' - The container image manifest digest.
-- * 'lastStatus' - The last known status of the container.
-- * 'memory' - The hard limit (in MiB) of memory set for the container.
-- * 'memoryReservation' - The soft limit (in MiB) of memory set for the container.
-- * 'name' - The name of the container.
-- * 'networkBindings' - The network bindings associated with the container.
-- * 'networkInterfaces' - The network interfaces associated with the container.
-- * 'reason' - A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
-- * 'runtimeId' - The ID of the Docker container.
-- * 'taskARN' - The ARN of the task.
mkContainer ::
  Container
mkContainer =
  Container'
    { gpuIds = Lude.Nothing,
      networkBindings = Lude.Nothing,
      image = Lude.Nothing,
      containerARN = Lude.Nothing,
      networkInterfaces = Lude.Nothing,
      taskARN = Lude.Nothing,
      lastStatus = Lude.Nothing,
      memory = Lude.Nothing,
      reason = Lude.Nothing,
      name = Lude.Nothing,
      imageDigest = Lude.Nothing,
      exitCode = Lude.Nothing,
      healthStatus = Lude.Nothing,
      cpu = Lude.Nothing,
      runtimeId = Lude.Nothing,
      memoryReservation = Lude.Nothing
    }

-- | The IDs of each GPU assigned to the container.
--
-- /Note:/ Consider using 'gpuIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cGpuIds :: Lens.Lens' Container (Lude.Maybe [Lude.Text])
cGpuIds = Lens.lens (gpuIds :: Container -> Lude.Maybe [Lude.Text]) (\s a -> s {gpuIds = a} :: Container)
{-# DEPRECATED cGpuIds "Use generic-lens or generic-optics with 'gpuIds' instead." #-}

-- | The network bindings associated with the container.
--
-- /Note:/ Consider using 'networkBindings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNetworkBindings :: Lens.Lens' Container (Lude.Maybe [NetworkBinding])
cNetworkBindings = Lens.lens (networkBindings :: Container -> Lude.Maybe [NetworkBinding]) (\s a -> s {networkBindings = a} :: Container)
{-# DEPRECATED cNetworkBindings "Use generic-lens or generic-optics with 'networkBindings' instead." #-}

-- | The image used for the container.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cImage :: Lens.Lens' Container (Lude.Maybe Lude.Text)
cImage = Lens.lens (image :: Container -> Lude.Maybe Lude.Text) (\s a -> s {image = a} :: Container)
{-# DEPRECATED cImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | The Amazon Resource Name (ARN) of the container.
--
-- /Note:/ Consider using 'containerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cContainerARN :: Lens.Lens' Container (Lude.Maybe Lude.Text)
cContainerARN = Lens.lens (containerARN :: Container -> Lude.Maybe Lude.Text) (\s a -> s {containerARN = a} :: Container)
{-# DEPRECATED cContainerARN "Use generic-lens or generic-optics with 'containerARN' instead." #-}

-- | The network interfaces associated with the container.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cNetworkInterfaces :: Lens.Lens' Container (Lude.Maybe [NetworkInterface])
cNetworkInterfaces = Lens.lens (networkInterfaces :: Container -> Lude.Maybe [NetworkInterface]) (\s a -> s {networkInterfaces = a} :: Container)
{-# DEPRECATED cNetworkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead." #-}

-- | The ARN of the task.
--
-- /Note:/ Consider using 'taskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTaskARN :: Lens.Lens' Container (Lude.Maybe Lude.Text)
cTaskARN = Lens.lens (taskARN :: Container -> Lude.Maybe Lude.Text) (\s a -> s {taskARN = a} :: Container)
{-# DEPRECATED cTaskARN "Use generic-lens or generic-optics with 'taskARN' instead." #-}

-- | The last known status of the container.
--
-- /Note:/ Consider using 'lastStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLastStatus :: Lens.Lens' Container (Lude.Maybe Lude.Text)
cLastStatus = Lens.lens (lastStatus :: Container -> Lude.Maybe Lude.Text) (\s a -> s {lastStatus = a} :: Container)
{-# DEPRECATED cLastStatus "Use generic-lens or generic-optics with 'lastStatus' instead." #-}

-- | The hard limit (in MiB) of memory set for the container.
--
-- /Note:/ Consider using 'memory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMemory :: Lens.Lens' Container (Lude.Maybe Lude.Text)
cMemory = Lens.lens (memory :: Container -> Lude.Maybe Lude.Text) (\s a -> s {memory = a} :: Container)
{-# DEPRECATED cMemory "Use generic-lens or generic-optics with 'memory' instead." #-}

-- | A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cReason :: Lens.Lens' Container (Lude.Maybe Lude.Text)
cReason = Lens.lens (reason :: Container -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: Container)
{-# DEPRECATED cReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The name of the container.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' Container (Lude.Maybe Lude.Text)
cName = Lens.lens (name :: Container -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Container)
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The container image manifest digest.
--
-- /Note:/ Consider using 'imageDigest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cImageDigest :: Lens.Lens' Container (Lude.Maybe Lude.Text)
cImageDigest = Lens.lens (imageDigest :: Container -> Lude.Maybe Lude.Text) (\s a -> s {imageDigest = a} :: Container)
{-# DEPRECATED cImageDigest "Use generic-lens or generic-optics with 'imageDigest' instead." #-}

-- | The exit code returned from the container.
--
-- /Note:/ Consider using 'exitCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cExitCode :: Lens.Lens' Container (Lude.Maybe Lude.Int)
cExitCode = Lens.lens (exitCode :: Container -> Lude.Maybe Lude.Int) (\s a -> s {exitCode = a} :: Container)
{-# DEPRECATED cExitCode "Use generic-lens or generic-optics with 'exitCode' instead." #-}

-- | The health status of the container. If health checks are not configured for this container in its task definition, then it reports the health status as @UNKNOWN@ .
--
-- /Note:/ Consider using 'healthStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cHealthStatus :: Lens.Lens' Container (Lude.Maybe HealthStatus)
cHealthStatus = Lens.lens (healthStatus :: Container -> Lude.Maybe HealthStatus) (\s a -> s {healthStatus = a} :: Container)
{-# DEPRECATED cHealthStatus "Use generic-lens or generic-optics with 'healthStatus' instead." #-}

-- | The number of CPU units set for the container. The value will be @0@ if no value was specified in the container definition when the task definition was registered.
--
-- /Note:/ Consider using 'cpu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCpu :: Lens.Lens' Container (Lude.Maybe Lude.Text)
cCpu = Lens.lens (cpu :: Container -> Lude.Maybe Lude.Text) (\s a -> s {cpu = a} :: Container)
{-# DEPRECATED cCpu "Use generic-lens or generic-optics with 'cpu' instead." #-}

-- | The ID of the Docker container.
--
-- /Note:/ Consider using 'runtimeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRuntimeId :: Lens.Lens' Container (Lude.Maybe Lude.Text)
cRuntimeId = Lens.lens (runtimeId :: Container -> Lude.Maybe Lude.Text) (\s a -> s {runtimeId = a} :: Container)
{-# DEPRECATED cRuntimeId "Use generic-lens or generic-optics with 'runtimeId' instead." #-}

-- | The soft limit (in MiB) of memory set for the container.
--
-- /Note:/ Consider using 'memoryReservation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMemoryReservation :: Lens.Lens' Container (Lude.Maybe Lude.Text)
cMemoryReservation = Lens.lens (memoryReservation :: Container -> Lude.Maybe Lude.Text) (\s a -> s {memoryReservation = a} :: Container)
{-# DEPRECATED cMemoryReservation "Use generic-lens or generic-optics with 'memoryReservation' instead." #-}

instance Lude.FromJSON Container where
  parseJSON =
    Lude.withObject
      "Container"
      ( \x ->
          Container'
            Lude.<$> (x Lude..:? "gpuIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "networkBindings" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "image")
            Lude.<*> (x Lude..:? "containerArn")
            Lude.<*> (x Lude..:? "networkInterfaces" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "taskArn")
            Lude.<*> (x Lude..:? "lastStatus")
            Lude.<*> (x Lude..:? "memory")
            Lude.<*> (x Lude..:? "reason")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "imageDigest")
            Lude.<*> (x Lude..:? "exitCode")
            Lude.<*> (x Lude..:? "healthStatus")
            Lude.<*> (x Lude..:? "cpu")
            Lude.<*> (x Lude..:? "runtimeId")
            Lude.<*> (x Lude..:? "memoryReservation")
      )
