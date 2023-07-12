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
-- Module      : Amazonka.MediaStore.Types.Container
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaStore.Types.Container where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaStore.Types.ContainerStatus
import qualified Amazonka.Prelude as Prelude

-- | This section describes operations that you can perform on an AWS
-- Elemental MediaStore container.
--
-- /See:/ 'newContainer' smart constructor.
data Container = Container'
  { -- | The Amazon Resource Name (ARN) of the container. The ARN has the
    -- following format:
    --
    -- arn:aws:\<region>:\<account that owns this container>:container\/\<name
    -- of container>
    --
    -- For example: arn:aws:mediastore:us-west-2:111122223333:container\/movies
    arn :: Prelude.Maybe Prelude.Text,
    -- | The state of access logging on the container. This value is @false@ by
    -- default, indicating that AWS Elemental MediaStore does not send access
    -- logs to Amazon CloudWatch Logs. When you enable access logging on the
    -- container, MediaStore changes this value to @true@, indicating that the
    -- service delivers access logs for objects stored in that container to
    -- CloudWatch Logs.
    accessLoggingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Unix timestamp.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The DNS endpoint of the container. Use the endpoint to identify the
    -- specific container when sending requests to the data plane. The service
    -- assigns this value when the container is created. Once the value has
    -- been assigned, it does not change.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | The name of the container.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status of container creation or deletion. The status is one of the
    -- following: @CREATING@, @ACTIVE@, or @DELETING@. While the service is
    -- creating the container, the status is @CREATING@. When the endpoint is
    -- available, the status changes to @ACTIVE@.
    status :: Prelude.Maybe ContainerStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Container' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'creationTime', 'container_creationTime' - Unix timestamp.
--
-- 'endpoint', 'container_endpoint' - The DNS endpoint of the container. Use the endpoint to identify the
-- specific container when sending requests to the data plane. The service
-- assigns this value when the container is created. Once the value has
-- been assigned, it does not change.
--
-- 'name', 'container_name' - The name of the container.
--
-- 'status', 'container_status' - The status of container creation or deletion. The status is one of the
-- following: @CREATING@, @ACTIVE@, or @DELETING@. While the service is
-- creating the container, the status is @CREATING@. When the endpoint is
-- available, the status changes to @ACTIVE@.
newContainer ::
  Container
newContainer =
  Container'
    { arn = Prelude.Nothing,
      accessLoggingEnabled = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the container. The ARN has the
-- following format:
--
-- arn:aws:\<region>:\<account that owns this container>:container\/\<name
-- of container>
--
-- For example: arn:aws:mediastore:us-west-2:111122223333:container\/movies
container_arn :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_arn = Lens.lens (\Container' {arn} -> arn) (\s@Container' {} a -> s {arn = a} :: Container)

-- | The state of access logging on the container. This value is @false@ by
-- default, indicating that AWS Elemental MediaStore does not send access
-- logs to Amazon CloudWatch Logs. When you enable access logging on the
-- container, MediaStore changes this value to @true@, indicating that the
-- service delivers access logs for objects stored in that container to
-- CloudWatch Logs.
container_accessLoggingEnabled :: Lens.Lens' Container (Prelude.Maybe Prelude.Bool)
container_accessLoggingEnabled = Lens.lens (\Container' {accessLoggingEnabled} -> accessLoggingEnabled) (\s@Container' {} a -> s {accessLoggingEnabled = a} :: Container)

-- | Unix timestamp.
container_creationTime :: Lens.Lens' Container (Prelude.Maybe Prelude.UTCTime)
container_creationTime = Lens.lens (\Container' {creationTime} -> creationTime) (\s@Container' {} a -> s {creationTime = a} :: Container) Prelude.. Lens.mapping Data._Time

-- | The DNS endpoint of the container. Use the endpoint to identify the
-- specific container when sending requests to the data plane. The service
-- assigns this value when the container is created. Once the value has
-- been assigned, it does not change.
container_endpoint :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_endpoint = Lens.lens (\Container' {endpoint} -> endpoint) (\s@Container' {} a -> s {endpoint = a} :: Container)

-- | The name of the container.
container_name :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_name = Lens.lens (\Container' {name} -> name) (\s@Container' {} a -> s {name = a} :: Container)

-- | The status of container creation or deletion. The status is one of the
-- following: @CREATING@, @ACTIVE@, or @DELETING@. While the service is
-- creating the container, the status is @CREATING@. When the endpoint is
-- available, the status changes to @ACTIVE@.
container_status :: Lens.Lens' Container (Prelude.Maybe ContainerStatus)
container_status = Lens.lens (\Container' {status} -> status) (\s@Container' {} a -> s {status = a} :: Container)

instance Data.FromJSON Container where
  parseJSON =
    Data.withObject
      "Container"
      ( \x ->
          Container'
            Prelude.<$> (x Data..:? "ARN")
            Prelude.<*> (x Data..:? "AccessLoggingEnabled")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "Endpoint")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable Container where
  hashWithSalt _salt Container' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` accessLoggingEnabled
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData Container where
  rnf Container' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf accessLoggingEnabled
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
