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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.ServiceRegistry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details of the service registry.
--
-- /See:/ 'newServiceRegistry' smart constructor.
data ServiceRegistry = ServiceRegistry'
  { -- | The Amazon Resource Name (ARN) of the service registry. The currently
    -- supported service registry is Cloud Map. For more information, see
    -- <https://docs.aws.amazon.com/cloud-map/latest/api/API_CreateService.html CreateService>.
    registryArn :: Prelude.Maybe Prelude.Text,
    -- | The container name value, already specified in the task definition, to
    -- be used for your service discovery service. If the task definition that
    -- your service task specifies uses the @bridge@ or @host@ network mode,
    -- you must specify a @containerName@ and @containerPort@ combination from
    -- the task definition. If the task definition that your service task
    -- specifies uses the @awsvpc@ network mode and a type SRV DNS record is
    -- used, you must specify either a @containerName@ and @containerPort@
    -- combination or a @port@ value, but not both.
    containerName :: Prelude.Maybe Prelude.Text,
    -- | The port value, already specified in the task definition, to be used for
    -- your service discovery service. If the task definition your service task
    -- specifies uses the @bridge@ or @host@ network mode, you must specify a
    -- @containerName@ and @containerPort@ combination from the task
    -- definition. If the task definition your service task specifies uses the
    -- @awsvpc@ network mode and a type SRV DNS record is used, you must
    -- specify either a @containerName@ and @containerPort@ combination or a
    -- @port@ value, but not both.
    containerPort :: Prelude.Maybe Prelude.Int,
    -- | The port value used if your service discovery service specified an SRV
    -- record. This field may be used if both the @awsvpc@ network mode and SRV
    -- records are used.
    port :: Prelude.Maybe Prelude.Int
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
-- 'registryArn', 'serviceRegistry_registryArn' - The Amazon Resource Name (ARN) of the service registry. The currently
-- supported service registry is Cloud Map. For more information, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_CreateService.html CreateService>.
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
-- 'containerPort', 'serviceRegistry_containerPort' - The port value, already specified in the task definition, to be used for
-- your service discovery service. If the task definition your service task
-- specifies uses the @bridge@ or @host@ network mode, you must specify a
-- @containerName@ and @containerPort@ combination from the task
-- definition. If the task definition your service task specifies uses the
-- @awsvpc@ network mode and a type SRV DNS record is used, you must
-- specify either a @containerName@ and @containerPort@ combination or a
-- @port@ value, but not both.
--
-- 'port', 'serviceRegistry_port' - The port value used if your service discovery service specified an SRV
-- record. This field may be used if both the @awsvpc@ network mode and SRV
-- records are used.
newServiceRegistry ::
  ServiceRegistry
newServiceRegistry =
  ServiceRegistry'
    { registryArn = Prelude.Nothing,
      containerName = Prelude.Nothing,
      containerPort = Prelude.Nothing,
      port = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the service registry. The currently
-- supported service registry is Cloud Map. For more information, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_CreateService.html CreateService>.
serviceRegistry_registryArn :: Lens.Lens' ServiceRegistry (Prelude.Maybe Prelude.Text)
serviceRegistry_registryArn = Lens.lens (\ServiceRegistry' {registryArn} -> registryArn) (\s@ServiceRegistry' {} a -> s {registryArn = a} :: ServiceRegistry)

-- | The container name value, already specified in the task definition, to
-- be used for your service discovery service. If the task definition that
-- your service task specifies uses the @bridge@ or @host@ network mode,
-- you must specify a @containerName@ and @containerPort@ combination from
-- the task definition. If the task definition that your service task
-- specifies uses the @awsvpc@ network mode and a type SRV DNS record is
-- used, you must specify either a @containerName@ and @containerPort@
-- combination or a @port@ value, but not both.
serviceRegistry_containerName :: Lens.Lens' ServiceRegistry (Prelude.Maybe Prelude.Text)
serviceRegistry_containerName = Lens.lens (\ServiceRegistry' {containerName} -> containerName) (\s@ServiceRegistry' {} a -> s {containerName = a} :: ServiceRegistry)

-- | The port value, already specified in the task definition, to be used for
-- your service discovery service. If the task definition your service task
-- specifies uses the @bridge@ or @host@ network mode, you must specify a
-- @containerName@ and @containerPort@ combination from the task
-- definition. If the task definition your service task specifies uses the
-- @awsvpc@ network mode and a type SRV DNS record is used, you must
-- specify either a @containerName@ and @containerPort@ combination or a
-- @port@ value, but not both.
serviceRegistry_containerPort :: Lens.Lens' ServiceRegistry (Prelude.Maybe Prelude.Int)
serviceRegistry_containerPort = Lens.lens (\ServiceRegistry' {containerPort} -> containerPort) (\s@ServiceRegistry' {} a -> s {containerPort = a} :: ServiceRegistry)

-- | The port value used if your service discovery service specified an SRV
-- record. This field may be used if both the @awsvpc@ network mode and SRV
-- records are used.
serviceRegistry_port :: Lens.Lens' ServiceRegistry (Prelude.Maybe Prelude.Int)
serviceRegistry_port = Lens.lens (\ServiceRegistry' {port} -> port) (\s@ServiceRegistry' {} a -> s {port = a} :: ServiceRegistry)

instance Core.FromJSON ServiceRegistry where
  parseJSON =
    Core.withObject
      "ServiceRegistry"
      ( \x ->
          ServiceRegistry'
            Prelude.<$> (x Core..:? "registryArn")
            Prelude.<*> (x Core..:? "containerName")
            Prelude.<*> (x Core..:? "containerPort")
            Prelude.<*> (x Core..:? "port")
      )

instance Prelude.Hashable ServiceRegistry where
  hashWithSalt _salt ServiceRegistry' {..} =
    _salt `Prelude.hashWithSalt` registryArn
      `Prelude.hashWithSalt` containerName
      `Prelude.hashWithSalt` containerPort
      `Prelude.hashWithSalt` port

instance Prelude.NFData ServiceRegistry where
  rnf ServiceRegistry' {..} =
    Prelude.rnf registryArn
      `Prelude.seq` Prelude.rnf containerName
      `Prelude.seq` Prelude.rnf containerPort
      `Prelude.seq` Prelude.rnf port

instance Core.ToJSON ServiceRegistry where
  toJSON ServiceRegistry' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("registryArn" Core..=) Prelude.<$> registryArn,
            ("containerName" Core..=) Prelude.<$> containerName,
            ("containerPort" Core..=) Prelude.<$> containerPort,
            ("port" Core..=) Prelude.<$> port
          ]
      )
