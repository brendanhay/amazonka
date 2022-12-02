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
-- Module      : Amazonka.ECS.Types.ServiceRegistry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.ServiceRegistry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details for the service registry.
--
-- Each service may be associated with one service registry. Multiple
-- service registries for each service are not supported.
--
-- When you add, update, or remove the service registries configuration,
-- Amazon ECS starts a new deployment. New tasks are registered and
-- deregistered to the updated service registry configuration.
--
-- /See:/ 'newServiceRegistry' smart constructor.
data ServiceRegistry = ServiceRegistry'
  { -- | The port value used if your service discovery service specified an SRV
    -- record. This field might be used if both the @awsvpc@ network mode and
    -- SRV records are used.
    port :: Prelude.Maybe Prelude.Int,
    -- | The port value to be used for your service discovery service. It\'s
    -- already specified in the task definition. If the task definition your
    -- service task specifies uses the @bridge@ or @host@ network mode, you
    -- must specify a @containerName@ and @containerPort@ combination from the
    -- task definition. If the task definition your service task specifies uses
    -- the @awsvpc@ network mode and a type SRV DNS record is used, you must
    -- specify either a @containerName@ and @containerPort@ combination or a
    -- @port@ value. However, you can\'t specify both.
    containerPort :: Prelude.Maybe Prelude.Int,
    -- | The container name value to be used for your service discovery service.
    -- It\'s already specified in the task definition. If the task definition
    -- that your service task specifies uses the @bridge@ or @host@ network
    -- mode, you must specify a @containerName@ and @containerPort@ combination
    -- from the task definition. If the task definition that your service task
    -- specifies uses the @awsvpc@ network mode and a type SRV DNS record is
    -- used, you must specify either a @containerName@ and @containerPort@
    -- combination or a @port@ value. However, you can\'t specify both.
    containerName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the service registry. The currently
    -- supported service registry is Cloud Map. For more information, see
    -- <https://docs.aws.amazon.com/cloud-map/latest/api/API_CreateService.html CreateService>.
    registryArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceRegistry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'port', 'serviceRegistry_port' - The port value used if your service discovery service specified an SRV
-- record. This field might be used if both the @awsvpc@ network mode and
-- SRV records are used.
--
-- 'containerPort', 'serviceRegistry_containerPort' - The port value to be used for your service discovery service. It\'s
-- already specified in the task definition. If the task definition your
-- service task specifies uses the @bridge@ or @host@ network mode, you
-- must specify a @containerName@ and @containerPort@ combination from the
-- task definition. If the task definition your service task specifies uses
-- the @awsvpc@ network mode and a type SRV DNS record is used, you must
-- specify either a @containerName@ and @containerPort@ combination or a
-- @port@ value. However, you can\'t specify both.
--
-- 'containerName', 'serviceRegistry_containerName' - The container name value to be used for your service discovery service.
-- It\'s already specified in the task definition. If the task definition
-- that your service task specifies uses the @bridge@ or @host@ network
-- mode, you must specify a @containerName@ and @containerPort@ combination
-- from the task definition. If the task definition that your service task
-- specifies uses the @awsvpc@ network mode and a type SRV DNS record is
-- used, you must specify either a @containerName@ and @containerPort@
-- combination or a @port@ value. However, you can\'t specify both.
--
-- 'registryArn', 'serviceRegistry_registryArn' - The Amazon Resource Name (ARN) of the service registry. The currently
-- supported service registry is Cloud Map. For more information, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_CreateService.html CreateService>.
newServiceRegistry ::
  ServiceRegistry
newServiceRegistry =
  ServiceRegistry'
    { port = Prelude.Nothing,
      containerPort = Prelude.Nothing,
      containerName = Prelude.Nothing,
      registryArn = Prelude.Nothing
    }

-- | The port value used if your service discovery service specified an SRV
-- record. This field might be used if both the @awsvpc@ network mode and
-- SRV records are used.
serviceRegistry_port :: Lens.Lens' ServiceRegistry (Prelude.Maybe Prelude.Int)
serviceRegistry_port = Lens.lens (\ServiceRegistry' {port} -> port) (\s@ServiceRegistry' {} a -> s {port = a} :: ServiceRegistry)

-- | The port value to be used for your service discovery service. It\'s
-- already specified in the task definition. If the task definition your
-- service task specifies uses the @bridge@ or @host@ network mode, you
-- must specify a @containerName@ and @containerPort@ combination from the
-- task definition. If the task definition your service task specifies uses
-- the @awsvpc@ network mode and a type SRV DNS record is used, you must
-- specify either a @containerName@ and @containerPort@ combination or a
-- @port@ value. However, you can\'t specify both.
serviceRegistry_containerPort :: Lens.Lens' ServiceRegistry (Prelude.Maybe Prelude.Int)
serviceRegistry_containerPort = Lens.lens (\ServiceRegistry' {containerPort} -> containerPort) (\s@ServiceRegistry' {} a -> s {containerPort = a} :: ServiceRegistry)

-- | The container name value to be used for your service discovery service.
-- It\'s already specified in the task definition. If the task definition
-- that your service task specifies uses the @bridge@ or @host@ network
-- mode, you must specify a @containerName@ and @containerPort@ combination
-- from the task definition. If the task definition that your service task
-- specifies uses the @awsvpc@ network mode and a type SRV DNS record is
-- used, you must specify either a @containerName@ and @containerPort@
-- combination or a @port@ value. However, you can\'t specify both.
serviceRegistry_containerName :: Lens.Lens' ServiceRegistry (Prelude.Maybe Prelude.Text)
serviceRegistry_containerName = Lens.lens (\ServiceRegistry' {containerName} -> containerName) (\s@ServiceRegistry' {} a -> s {containerName = a} :: ServiceRegistry)

-- | The Amazon Resource Name (ARN) of the service registry. The currently
-- supported service registry is Cloud Map. For more information, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_CreateService.html CreateService>.
serviceRegistry_registryArn :: Lens.Lens' ServiceRegistry (Prelude.Maybe Prelude.Text)
serviceRegistry_registryArn = Lens.lens (\ServiceRegistry' {registryArn} -> registryArn) (\s@ServiceRegistry' {} a -> s {registryArn = a} :: ServiceRegistry)

instance Data.FromJSON ServiceRegistry where
  parseJSON =
    Data.withObject
      "ServiceRegistry"
      ( \x ->
          ServiceRegistry'
            Prelude.<$> (x Data..:? "port")
            Prelude.<*> (x Data..:? "containerPort")
            Prelude.<*> (x Data..:? "containerName")
            Prelude.<*> (x Data..:? "registryArn")
      )

instance Prelude.Hashable ServiceRegistry where
  hashWithSalt _salt ServiceRegistry' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` containerPort
      `Prelude.hashWithSalt` containerName
      `Prelude.hashWithSalt` registryArn

instance Prelude.NFData ServiceRegistry where
  rnf ServiceRegistry' {..} =
    Prelude.rnf port
      `Prelude.seq` Prelude.rnf containerPort
      `Prelude.seq` Prelude.rnf containerName
      `Prelude.seq` Prelude.rnf registryArn

instance Data.ToJSON ServiceRegistry where
  toJSON ServiceRegistry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("port" Data..=) Prelude.<$> port,
            ("containerPort" Data..=) Prelude.<$> containerPort,
            ("containerName" Data..=) Prelude.<$> containerName,
            ("registryArn" Data..=) Prelude.<$> registryArn
          ]
      )
