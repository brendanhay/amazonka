{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.Types.Container
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaStore.Types.Container where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types.ContainerStatus

-- | This section describes operations that you can perform on an AWS
-- Elemental MediaStore container.
--
-- /See:/ 'newContainer' smart constructor.
data Container = Container'
  { -- | The status of container creation or deletion. The status is one of the
    -- following: @CREATING@, @ACTIVE@, or @DELETING@. While the service is
    -- creating the container, the status is @CREATING@. When the endpoint is
    -- available, the status changes to @ACTIVE@.
    status :: Core.Maybe ContainerStatus,
    -- | Unix timestamp.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the container. The ARN has the
    -- following format:
    --
    -- arn:aws:\<region>:\<account that owns this container>:container\/\<name
    -- of container>
    --
    -- For example: arn:aws:mediastore:us-west-2:111122223333:container\/movies
    arn :: Core.Maybe Core.Text,
    -- | The state of access logging on the container. This value is @false@ by
    -- default, indicating that AWS Elemental MediaStore does not send access
    -- logs to Amazon CloudWatch Logs. When you enable access logging on the
    -- container, MediaStore changes this value to @true@, indicating that the
    -- service delivers access logs for objects stored in that container to
    -- CloudWatch Logs.
    accessLoggingEnabled :: Core.Maybe Core.Bool,
    -- | The name of the container.
    name :: Core.Maybe Core.Text,
    -- | The DNS endpoint of the container. Use the endpoint to identify the
    -- specific container when sending requests to the data plane. The service
    -- assigns this value when the container is created. Once the value has
    -- been assigned, it does not change.
    endpoint :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Container' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'container_status' - The status of container creation or deletion. The status is one of the
-- following: @CREATING@, @ACTIVE@, or @DELETING@. While the service is
-- creating the container, the status is @CREATING@. When the endpoint is
-- available, the status changes to @ACTIVE@.
--
-- 'creationTime', 'container_creationTime' - Unix timestamp.
--
-- 'arn', 'container_arn' - The Amazon Resource Name (ARN) of the container. The ARN has the
-- following format:
--
-- arn:aws:\<region>:\<account that owns this container>:container\/\<name
-- of container>
--
-- For example: arn:aws:mediastore:us-west-2:111122223333:container\/movies
--
-- 'accessLoggingEnabled', 'container_accessLoggingEnabled' - The state of access logging on the container. This value is @false@ by
-- default, indicating that AWS Elemental MediaStore does not send access
-- logs to Amazon CloudWatch Logs. When you enable access logging on the
-- container, MediaStore changes this value to @true@, indicating that the
-- service delivers access logs for objects stored in that container to
-- CloudWatch Logs.
--
-- 'name', 'container_name' - The name of the container.
--
-- 'endpoint', 'container_endpoint' - The DNS endpoint of the container. Use the endpoint to identify the
-- specific container when sending requests to the data plane. The service
-- assigns this value when the container is created. Once the value has
-- been assigned, it does not change.
newContainer ::
  Container
newContainer =
  Container'
    { status = Core.Nothing,
      creationTime = Core.Nothing,
      arn = Core.Nothing,
      accessLoggingEnabled = Core.Nothing,
      name = Core.Nothing,
      endpoint = Core.Nothing
    }

-- | The status of container creation or deletion. The status is one of the
-- following: @CREATING@, @ACTIVE@, or @DELETING@. While the service is
-- creating the container, the status is @CREATING@. When the endpoint is
-- available, the status changes to @ACTIVE@.
container_status :: Lens.Lens' Container (Core.Maybe ContainerStatus)
container_status = Lens.lens (\Container' {status} -> status) (\s@Container' {} a -> s {status = a} :: Container)

-- | Unix timestamp.
container_creationTime :: Lens.Lens' Container (Core.Maybe Core.UTCTime)
container_creationTime = Lens.lens (\Container' {creationTime} -> creationTime) (\s@Container' {} a -> s {creationTime = a} :: Container) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the container. The ARN has the
-- following format:
--
-- arn:aws:\<region>:\<account that owns this container>:container\/\<name
-- of container>
--
-- For example: arn:aws:mediastore:us-west-2:111122223333:container\/movies
container_arn :: Lens.Lens' Container (Core.Maybe Core.Text)
container_arn = Lens.lens (\Container' {arn} -> arn) (\s@Container' {} a -> s {arn = a} :: Container)

-- | The state of access logging on the container. This value is @false@ by
-- default, indicating that AWS Elemental MediaStore does not send access
-- logs to Amazon CloudWatch Logs. When you enable access logging on the
-- container, MediaStore changes this value to @true@, indicating that the
-- service delivers access logs for objects stored in that container to
-- CloudWatch Logs.
container_accessLoggingEnabled :: Lens.Lens' Container (Core.Maybe Core.Bool)
container_accessLoggingEnabled = Lens.lens (\Container' {accessLoggingEnabled} -> accessLoggingEnabled) (\s@Container' {} a -> s {accessLoggingEnabled = a} :: Container)

-- | The name of the container.
container_name :: Lens.Lens' Container (Core.Maybe Core.Text)
container_name = Lens.lens (\Container' {name} -> name) (\s@Container' {} a -> s {name = a} :: Container)

-- | The DNS endpoint of the container. Use the endpoint to identify the
-- specific container when sending requests to the data plane. The service
-- assigns this value when the container is created. Once the value has
-- been assigned, it does not change.
container_endpoint :: Lens.Lens' Container (Core.Maybe Core.Text)
container_endpoint = Lens.lens (\Container' {endpoint} -> endpoint) (\s@Container' {} a -> s {endpoint = a} :: Container)

instance Core.FromJSON Container where
  parseJSON =
    Core.withObject
      "Container"
      ( \x ->
          Container'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "ARN")
            Core.<*> (x Core..:? "AccessLoggingEnabled")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Endpoint")
      )

instance Core.Hashable Container

instance Core.NFData Container
