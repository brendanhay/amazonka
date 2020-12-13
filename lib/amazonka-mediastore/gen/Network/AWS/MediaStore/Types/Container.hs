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
    cCreationTime,
    cStatus,
    cAccessLoggingEnabled,
    cARN,
    cName,
    cEndpoint,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types.ContainerStatus
import qualified Network.AWS.Prelude as Lude

-- | This section describes operations that you can perform on an AWS Elemental MediaStore container.
--
-- /See:/ 'mkContainer' smart constructor.
data Container = Container'
  { -- | Unix timestamp.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | The status of container creation or deletion. The status is one of the following: @CREATING@ , @ACTIVE@ , or @DELETING@ . While the service is creating the container, the status is @CREATING@ . When the endpoint is available, the status changes to @ACTIVE@ .
    status :: Lude.Maybe ContainerStatus,
    -- | The state of access logging on the container. This value is @false@ by default, indicating that AWS Elemental MediaStore does not send access logs to Amazon CloudWatch Logs. When you enable access logging on the container, MediaStore changes this value to @true@ , indicating that the service delivers access logs for objects stored in that container to CloudWatch Logs.
    accessLoggingEnabled :: Lude.Maybe Lude.Bool,
    -- | The Amazon Resource Name (ARN) of the container. The ARN has the following format:
    --
    -- arn:aws:<region>:<account that owns this container>:container/<name of container>
    -- For example: arn:aws:mediastore:us-west-2:111122223333:container/movies
    arn :: Lude.Maybe Lude.Text,
    -- | The name of the container.
    name :: Lude.Maybe Lude.Text,
    -- | The DNS endpoint of the container. Use the endpoint to identify the specific container when sending requests to the data plane. The service assigns this value when the container is created. Once the value has been assigned, it does not change.
    endpoint :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Container' with the minimum fields required to make a request.
--
-- * 'creationTime' - Unix timestamp.
-- * 'status' - The status of container creation or deletion. The status is one of the following: @CREATING@ , @ACTIVE@ , or @DELETING@ . While the service is creating the container, the status is @CREATING@ . When the endpoint is available, the status changes to @ACTIVE@ .
-- * 'accessLoggingEnabled' - The state of access logging on the container. This value is @false@ by default, indicating that AWS Elemental MediaStore does not send access logs to Amazon CloudWatch Logs. When you enable access logging on the container, MediaStore changes this value to @true@ , indicating that the service delivers access logs for objects stored in that container to CloudWatch Logs.
-- * 'arn' - The Amazon Resource Name (ARN) of the container. The ARN has the following format:
--
-- arn:aws:<region>:<account that owns this container>:container/<name of container>
-- For example: arn:aws:mediastore:us-west-2:111122223333:container/movies
-- * 'name' - The name of the container.
-- * 'endpoint' - The DNS endpoint of the container. Use the endpoint to identify the specific container when sending requests to the data plane. The service assigns this value when the container is created. Once the value has been assigned, it does not change.
mkContainer ::
  Container
mkContainer =
  Container'
    { creationTime = Lude.Nothing,
      status = Lude.Nothing,
      accessLoggingEnabled = Lude.Nothing,
      arn = Lude.Nothing,
      name = Lude.Nothing,
      endpoint = Lude.Nothing
    }

-- | Unix timestamp.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCreationTime :: Lens.Lens' Container (Lude.Maybe Lude.Timestamp)
cCreationTime = Lens.lens (creationTime :: Container -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: Container)
{-# DEPRECATED cCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The status of container creation or deletion. The status is one of the following: @CREATING@ , @ACTIVE@ , or @DELETING@ . While the service is creating the container, the status is @CREATING@ . When the endpoint is available, the status changes to @ACTIVE@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStatus :: Lens.Lens' Container (Lude.Maybe ContainerStatus)
cStatus = Lens.lens (status :: Container -> Lude.Maybe ContainerStatus) (\s a -> s {status = a} :: Container)
{-# DEPRECATED cStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The state of access logging on the container. This value is @false@ by default, indicating that AWS Elemental MediaStore does not send access logs to Amazon CloudWatch Logs. When you enable access logging on the container, MediaStore changes this value to @true@ , indicating that the service delivers access logs for objects stored in that container to CloudWatch Logs.
--
-- /Note:/ Consider using 'accessLoggingEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAccessLoggingEnabled :: Lens.Lens' Container (Lude.Maybe Lude.Bool)
cAccessLoggingEnabled = Lens.lens (accessLoggingEnabled :: Container -> Lude.Maybe Lude.Bool) (\s a -> s {accessLoggingEnabled = a} :: Container)
{-# DEPRECATED cAccessLoggingEnabled "Use generic-lens or generic-optics with 'accessLoggingEnabled' instead." #-}

-- | The Amazon Resource Name (ARN) of the container. The ARN has the following format:
--
-- arn:aws:<region>:<account that owns this container>:container/<name of container>
-- For example: arn:aws:mediastore:us-west-2:111122223333:container/movies
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cARN :: Lens.Lens' Container (Lude.Maybe Lude.Text)
cARN = Lens.lens (arn :: Container -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Container)
{-# DEPRECATED cARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the container.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' Container (Lude.Maybe Lude.Text)
cName = Lens.lens (name :: Container -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Container)
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The DNS endpoint of the container. Use the endpoint to identify the specific container when sending requests to the data plane. The service assigns this value when the container is created. Once the value has been assigned, it does not change.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEndpoint :: Lens.Lens' Container (Lude.Maybe Lude.Text)
cEndpoint = Lens.lens (endpoint :: Container -> Lude.Maybe Lude.Text) (\s a -> s {endpoint = a} :: Container)
{-# DEPRECATED cEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

instance Lude.FromJSON Container where
  parseJSON =
    Lude.withObject
      "Container"
      ( \x ->
          Container'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "AccessLoggingEnabled")
            Lude.<*> (x Lude..:? "ARN")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Endpoint")
      )
