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
-- Module      : Network.AWS.ECS.Types.ServiceRegistry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ServiceRegistry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Details of the service registry.
--
-- /See:/ 'newServiceRegistry' smart constructor.
data ServiceRegistry = ServiceRegistry'
  { -- | The port value used if your service discovery service specified an SRV
    -- record. This field may be used if both the @awsvpc@ network mode and SRV
    -- records are used.
    port :: Core.Maybe Core.Int,
    -- | The port value, already specified in the task definition, to be used for
    -- your service discovery service. If the task definition your service task
    -- specifies uses the @bridge@ or @host@ network mode, you must specify a
    -- @containerName@ and @containerPort@ combination from the task
    -- definition. If the task definition your service task specifies uses the
    -- @awsvpc@ network mode and a type SRV DNS record is used, you must
    -- specify either a @containerName@ and @containerPort@ combination or a
    -- @port@ value, but not both.
    containerPort :: Core.Maybe Core.Int,
    -- | The container name value, already specified in the task definition, to
    -- be used for your service discovery service. If the task definition that
    -- your service task specifies uses the @bridge@ or @host@ network mode,
    -- you must specify a @containerName@ and @containerPort@ combination from
    -- the task definition. If the task definition that your service task
    -- specifies uses the @awsvpc@ network mode and a type SRV DNS record is
    -- used, you must specify either a @containerName@ and @containerPort@
    -- combination or a @port@ value, but not both.
    containerName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the service registry. The currently
    -- supported service registry is AWS Cloud Map. For more information, see
    -- <https://docs.aws.amazon.com/cloud-map/latest/api/API_CreateService.html CreateService>.
    registryArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ServiceRegistry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'port', 'serviceRegistry_port' - The port value used if your service discovery service specified an SRV
-- record. This field may be used if both the @awsvpc@ network mode and SRV
-- records are used.
--
-- 'containerPort', 'serviceRegistry_containerPort' - The port value, already specified in the task definition, to be used for
-- your service discovery service. If the task definition your service task
-- specifies uses the @bridge@ or @host@ network mode, you must specify a
-- @containerName@ and @containerPort@ combination from the task
-- definition. If the task definition your service task specifies uses the
-- @awsvpc@ network mode and a type SRV DNS record is used, you must
-- specify either a @containerName@ and @containerPort@ combination or a
-- @port@ value, but not both.
--
-- 'containerName', 'serviceRegistry_containerName' - The container name value, already specified in the task definition, to
-- be used for your service discovery service. If the task definition that
-- your service task specifies uses the @bridge@ or @host@ network mode,
-- you must specify a @containerName@ and @containerPort@ combination from
-- the task definition. If the task definition that your service task
-- specifies uses the @awsvpc@ network mode and a type SRV DNS record is
-- used, you must specify either a @containerName@ and @containerPort@
-- combination or a @port@ value, but not both.
--
-- 'registryArn', 'serviceRegistry_registryArn' - The Amazon Resource Name (ARN) of the service registry. The currently
-- supported service registry is AWS Cloud Map. For more information, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_CreateService.html CreateService>.
newServiceRegistry ::
  ServiceRegistry
newServiceRegistry =
  ServiceRegistry'
    { port = Core.Nothing,
      containerPort = Core.Nothing,
      containerName = Core.Nothing,
      registryArn = Core.Nothing
    }

-- | The port value used if your service discovery service specified an SRV
-- record. This field may be used if both the @awsvpc@ network mode and SRV
-- records are used.
serviceRegistry_port :: Lens.Lens' ServiceRegistry (Core.Maybe Core.Int)
serviceRegistry_port = Lens.lens (\ServiceRegistry' {port} -> port) (\s@ServiceRegistry' {} a -> s {port = a} :: ServiceRegistry)

-- | The port value, already specified in the task definition, to be used for
-- your service discovery service. If the task definition your service task
-- specifies uses the @bridge@ or @host@ network mode, you must specify a
-- @containerName@ and @containerPort@ combination from the task
-- definition. If the task definition your service task specifies uses the
-- @awsvpc@ network mode and a type SRV DNS record is used, you must
-- specify either a @containerName@ and @containerPort@ combination or a
-- @port@ value, but not both.
serviceRegistry_containerPort :: Lens.Lens' ServiceRegistry (Core.Maybe Core.Int)
serviceRegistry_containerPort = Lens.lens (\ServiceRegistry' {containerPort} -> containerPort) (\s@ServiceRegistry' {} a -> s {containerPort = a} :: ServiceRegistry)

-- | The container name value, already specified in the task definition, to
-- be used for your service discovery service. If the task definition that
-- your service task specifies uses the @bridge@ or @host@ network mode,
-- you must specify a @containerName@ and @containerPort@ combination from
-- the task definition. If the task definition that your service task
-- specifies uses the @awsvpc@ network mode and a type SRV DNS record is
-- used, you must specify either a @containerName@ and @containerPort@
-- combination or a @port@ value, but not both.
serviceRegistry_containerName :: Lens.Lens' ServiceRegistry (Core.Maybe Core.Text)
serviceRegistry_containerName = Lens.lens (\ServiceRegistry' {containerName} -> containerName) (\s@ServiceRegistry' {} a -> s {containerName = a} :: ServiceRegistry)

-- | The Amazon Resource Name (ARN) of the service registry. The currently
-- supported service registry is AWS Cloud Map. For more information, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_CreateService.html CreateService>.
serviceRegistry_registryArn :: Lens.Lens' ServiceRegistry (Core.Maybe Core.Text)
serviceRegistry_registryArn = Lens.lens (\ServiceRegistry' {registryArn} -> registryArn) (\s@ServiceRegistry' {} a -> s {registryArn = a} :: ServiceRegistry)

instance Core.FromJSON ServiceRegistry where
  parseJSON =
    Core.withObject
      "ServiceRegistry"
      ( \x ->
          ServiceRegistry'
            Core.<$> (x Core..:? "port")
            Core.<*> (x Core..:? "containerPort")
            Core.<*> (x Core..:? "containerName")
            Core.<*> (x Core..:? "registryArn")
      )

instance Core.Hashable ServiceRegistry

instance Core.NFData ServiceRegistry

instance Core.ToJSON ServiceRegistry where
  toJSON ServiceRegistry' {..} =
    Core.object
      ( Core.catMaybes
          [ ("port" Core..=) Core.<$> port,
            ("containerPort" Core..=) Core.<$> containerPort,
            ("containerName" Core..=) Core.<$> containerName,
            ("registryArn" Core..=) Core.<$> registryArn
          ]
      )
