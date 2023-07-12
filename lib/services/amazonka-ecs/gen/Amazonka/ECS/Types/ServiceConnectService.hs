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
-- Module      : Amazonka.ECS.Types.ServiceConnectService
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.ServiceConnectService where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.ServiceConnectClientAlias
import qualified Amazonka.Prelude as Prelude

-- | The Service Connect service object configuration. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- /See:/ 'newServiceConnectService' smart constructor.
data ServiceConnectService = ServiceConnectService'
  { -- | The list of client aliases for this Service Connect service. You use
    -- these to assign names that can be used by client applications. The
    -- maximum number of client aliases that you can have in this list is 1.
    --
    -- Each alias (\"endpoint\") is a fully-qualified name and port number that
    -- other Amazon ECS tasks (\"clients\") can use to connect to this service.
    --
    -- Each name and port mapping must be unique within the namespace.
    --
    -- For each @ServiceConnectService@, you must provide at least one
    -- @clientAlias@ with one @port@.
    clientAliases :: Prelude.Maybe [ServiceConnectClientAlias],
    -- | The @discoveryName@ is the name of the new Cloud Map service that Amazon
    -- ECS creates for this Amazon ECS service. This must be unique within the
    -- Cloud Map namespace. The name can contain up to 64 characters. The name
    -- can include lowercase letters, numbers, underscores (_), and hyphens
    -- (-). The name can\'t start with a hyphen.
    --
    -- If this parameter isn\'t specified, the default value of
    -- @discoveryName.namespace@ is used. If the @discoveryName@ isn\'t
    -- specified, the port mapping name from the task definition is used in
    -- @portName.namespace@.
    discoveryName :: Prelude.Maybe Prelude.Text,
    -- | The port number for the Service Connect proxy to listen on.
    --
    -- Use the value of this field to bypass the proxy for traffic on the port
    -- number specified in the named @portMapping@ in the task definition of
    -- this application, and then use it in your VPC security groups to allow
    -- traffic into the proxy for this Amazon ECS service.
    --
    -- In @awsvpc@ mode and Fargate, the default value is the container port
    -- number. The container port number is in the @portMapping@ in the task
    -- definition. In bridge mode, the default value is the ephemeral port of
    -- the Service Connect proxy.
    ingressPortOverride :: Prelude.Maybe Prelude.Natural,
    -- | The @portName@ must match the name of one of the @portMappings@ from all
    -- the containers in the task definition of this Amazon ECS service.
    portName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceConnectService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientAliases', 'serviceConnectService_clientAliases' - The list of client aliases for this Service Connect service. You use
-- these to assign names that can be used by client applications. The
-- maximum number of client aliases that you can have in this list is 1.
--
-- Each alias (\"endpoint\") is a fully-qualified name and port number that
-- other Amazon ECS tasks (\"clients\") can use to connect to this service.
--
-- Each name and port mapping must be unique within the namespace.
--
-- For each @ServiceConnectService@, you must provide at least one
-- @clientAlias@ with one @port@.
--
-- 'discoveryName', 'serviceConnectService_discoveryName' - The @discoveryName@ is the name of the new Cloud Map service that Amazon
-- ECS creates for this Amazon ECS service. This must be unique within the
-- Cloud Map namespace. The name can contain up to 64 characters. The name
-- can include lowercase letters, numbers, underscores (_), and hyphens
-- (-). The name can\'t start with a hyphen.
--
-- If this parameter isn\'t specified, the default value of
-- @discoveryName.namespace@ is used. If the @discoveryName@ isn\'t
-- specified, the port mapping name from the task definition is used in
-- @portName.namespace@.
--
-- 'ingressPortOverride', 'serviceConnectService_ingressPortOverride' - The port number for the Service Connect proxy to listen on.
--
-- Use the value of this field to bypass the proxy for traffic on the port
-- number specified in the named @portMapping@ in the task definition of
-- this application, and then use it in your VPC security groups to allow
-- traffic into the proxy for this Amazon ECS service.
--
-- In @awsvpc@ mode and Fargate, the default value is the container port
-- number. The container port number is in the @portMapping@ in the task
-- definition. In bridge mode, the default value is the ephemeral port of
-- the Service Connect proxy.
--
-- 'portName', 'serviceConnectService_portName' - The @portName@ must match the name of one of the @portMappings@ from all
-- the containers in the task definition of this Amazon ECS service.
newServiceConnectService ::
  -- | 'portName'
  Prelude.Text ->
  ServiceConnectService
newServiceConnectService pPortName_ =
  ServiceConnectService'
    { clientAliases =
        Prelude.Nothing,
      discoveryName = Prelude.Nothing,
      ingressPortOverride = Prelude.Nothing,
      portName = pPortName_
    }

-- | The list of client aliases for this Service Connect service. You use
-- these to assign names that can be used by client applications. The
-- maximum number of client aliases that you can have in this list is 1.
--
-- Each alias (\"endpoint\") is a fully-qualified name and port number that
-- other Amazon ECS tasks (\"clients\") can use to connect to this service.
--
-- Each name and port mapping must be unique within the namespace.
--
-- For each @ServiceConnectService@, you must provide at least one
-- @clientAlias@ with one @port@.
serviceConnectService_clientAliases :: Lens.Lens' ServiceConnectService (Prelude.Maybe [ServiceConnectClientAlias])
serviceConnectService_clientAliases = Lens.lens (\ServiceConnectService' {clientAliases} -> clientAliases) (\s@ServiceConnectService' {} a -> s {clientAliases = a} :: ServiceConnectService) Prelude.. Lens.mapping Lens.coerced

-- | The @discoveryName@ is the name of the new Cloud Map service that Amazon
-- ECS creates for this Amazon ECS service. This must be unique within the
-- Cloud Map namespace. The name can contain up to 64 characters. The name
-- can include lowercase letters, numbers, underscores (_), and hyphens
-- (-). The name can\'t start with a hyphen.
--
-- If this parameter isn\'t specified, the default value of
-- @discoveryName.namespace@ is used. If the @discoveryName@ isn\'t
-- specified, the port mapping name from the task definition is used in
-- @portName.namespace@.
serviceConnectService_discoveryName :: Lens.Lens' ServiceConnectService (Prelude.Maybe Prelude.Text)
serviceConnectService_discoveryName = Lens.lens (\ServiceConnectService' {discoveryName} -> discoveryName) (\s@ServiceConnectService' {} a -> s {discoveryName = a} :: ServiceConnectService)

-- | The port number for the Service Connect proxy to listen on.
--
-- Use the value of this field to bypass the proxy for traffic on the port
-- number specified in the named @portMapping@ in the task definition of
-- this application, and then use it in your VPC security groups to allow
-- traffic into the proxy for this Amazon ECS service.
--
-- In @awsvpc@ mode and Fargate, the default value is the container port
-- number. The container port number is in the @portMapping@ in the task
-- definition. In bridge mode, the default value is the ephemeral port of
-- the Service Connect proxy.
serviceConnectService_ingressPortOverride :: Lens.Lens' ServiceConnectService (Prelude.Maybe Prelude.Natural)
serviceConnectService_ingressPortOverride = Lens.lens (\ServiceConnectService' {ingressPortOverride} -> ingressPortOverride) (\s@ServiceConnectService' {} a -> s {ingressPortOverride = a} :: ServiceConnectService)

-- | The @portName@ must match the name of one of the @portMappings@ from all
-- the containers in the task definition of this Amazon ECS service.
serviceConnectService_portName :: Lens.Lens' ServiceConnectService Prelude.Text
serviceConnectService_portName = Lens.lens (\ServiceConnectService' {portName} -> portName) (\s@ServiceConnectService' {} a -> s {portName = a} :: ServiceConnectService)

instance Data.FromJSON ServiceConnectService where
  parseJSON =
    Data.withObject
      "ServiceConnectService"
      ( \x ->
          ServiceConnectService'
            Prelude.<$> (x Data..:? "clientAliases" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "discoveryName")
            Prelude.<*> (x Data..:? "ingressPortOverride")
            Prelude.<*> (x Data..: "portName")
      )

instance Prelude.Hashable ServiceConnectService where
  hashWithSalt _salt ServiceConnectService' {..} =
    _salt
      `Prelude.hashWithSalt` clientAliases
      `Prelude.hashWithSalt` discoveryName
      `Prelude.hashWithSalt` ingressPortOverride
      `Prelude.hashWithSalt` portName

instance Prelude.NFData ServiceConnectService where
  rnf ServiceConnectService' {..} =
    Prelude.rnf clientAliases
      `Prelude.seq` Prelude.rnf discoveryName
      `Prelude.seq` Prelude.rnf ingressPortOverride
      `Prelude.seq` Prelude.rnf portName

instance Data.ToJSON ServiceConnectService where
  toJSON ServiceConnectService' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientAliases" Data..=) Prelude.<$> clientAliases,
            ("discoveryName" Data..=) Prelude.<$> discoveryName,
            ("ingressPortOverride" Data..=)
              Prelude.<$> ingressPortOverride,
            Prelude.Just ("portName" Data..= portName)
          ]
      )
