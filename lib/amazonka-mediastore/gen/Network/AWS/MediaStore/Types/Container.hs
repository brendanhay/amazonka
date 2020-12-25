{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.Types.Container
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaStore.Types.Container
  ( Container (..),

    -- * Smart constructor
    mkContainer,

    -- * Lenses
    cARN,
    cAccessLoggingEnabled,
    cCreationTime,
    cEndpoint,
    cName,
    cStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStore.Types.ContainerARN as Types
import qualified Network.AWS.MediaStore.Types.ContainerName as Types
import qualified Network.AWS.MediaStore.Types.ContainerStatus as Types
import qualified Network.AWS.MediaStore.Types.Endpoint as Types
import qualified Network.AWS.Prelude as Core

-- | This section describes operations that you can perform on an AWS Elemental MediaStore container.
--
-- /See:/ 'mkContainer' smart constructor.
data Container = Container'
  { -- | The Amazon Resource Name (ARN) of the container. The ARN has the following format:
    --
    -- arn:aws:<region>:<account that owns this container>:container/<name of container>
    -- For example: arn:aws:mediastore:us-west-2:111122223333:container/movies
    arn :: Core.Maybe Types.ContainerARN,
    -- | The state of access logging on the container. This value is @false@ by default, indicating that AWS Elemental MediaStore does not send access logs to Amazon CloudWatch Logs. When you enable access logging on the container, MediaStore changes this value to @true@ , indicating that the service delivers access logs for objects stored in that container to CloudWatch Logs.
    accessLoggingEnabled :: Core.Maybe Core.Bool,
    -- | Unix timestamp.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The DNS endpoint of the container. Use the endpoint to identify the specific container when sending requests to the data plane. The service assigns this value when the container is created. Once the value has been assigned, it does not change.
    endpoint :: Core.Maybe Types.Endpoint,
    -- | The name of the container.
    name :: Core.Maybe Types.ContainerName,
    -- | The status of container creation or deletion. The status is one of the following: @CREATING@ , @ACTIVE@ , or @DELETING@ . While the service is creating the container, the status is @CREATING@ . When the endpoint is available, the status changes to @ACTIVE@ .
    status :: Core.Maybe Types.ContainerStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Container' value with any optional fields omitted.
mkContainer ::
  Container
mkContainer =
  Container'
    { arn = Core.Nothing,
      accessLoggingEnabled = Core.Nothing,
      creationTime = Core.Nothing,
      endpoint = Core.Nothing,
      name = Core.Nothing,
      status = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the container. The ARN has the following format:
--
-- arn:aws:<region>:<account that owns this container>:container/<name of container>
-- For example: arn:aws:mediastore:us-west-2:111122223333:container/movies
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cARN :: Lens.Lens' Container (Core.Maybe Types.ContainerARN)
cARN = Lens.field @"arn"
{-# DEPRECATED cARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The state of access logging on the container. This value is @false@ by default, indicating that AWS Elemental MediaStore does not send access logs to Amazon CloudWatch Logs. When you enable access logging on the container, MediaStore changes this value to @true@ , indicating that the service delivers access logs for objects stored in that container to CloudWatch Logs.
--
-- /Note:/ Consider using 'accessLoggingEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAccessLoggingEnabled :: Lens.Lens' Container (Core.Maybe Core.Bool)
cAccessLoggingEnabled = Lens.field @"accessLoggingEnabled"
{-# DEPRECATED cAccessLoggingEnabled "Use generic-lens or generic-optics with 'accessLoggingEnabled' instead." #-}

-- | Unix timestamp.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCreationTime :: Lens.Lens' Container (Core.Maybe Core.NominalDiffTime)
cCreationTime = Lens.field @"creationTime"
{-# DEPRECATED cCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The DNS endpoint of the container. Use the endpoint to identify the specific container when sending requests to the data plane. The service assigns this value when the container is created. Once the value has been assigned, it does not change.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEndpoint :: Lens.Lens' Container (Core.Maybe Types.Endpoint)
cEndpoint = Lens.field @"endpoint"
{-# DEPRECATED cEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | The name of the container.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' Container (Core.Maybe Types.ContainerName)
cName = Lens.field @"name"
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The status of container creation or deletion. The status is one of the following: @CREATING@ , @ACTIVE@ , or @DELETING@ . While the service is creating the container, the status is @CREATING@ . When the endpoint is available, the status changes to @ACTIVE@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStatus :: Lens.Lens' Container (Core.Maybe Types.ContainerStatus)
cStatus = Lens.field @"status"
{-# DEPRECATED cStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON Container where
  parseJSON =
    Core.withObject "Container" Core.$
      \x ->
        Container'
          Core.<$> (x Core..:? "ARN")
          Core.<*> (x Core..:? "AccessLoggingEnabled")
          Core.<*> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "Endpoint")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Status")
