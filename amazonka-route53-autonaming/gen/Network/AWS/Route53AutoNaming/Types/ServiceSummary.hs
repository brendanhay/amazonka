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
-- Module      : Network.AWS.Route53AutoNaming.Types.ServiceSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.ServiceSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Route53AutoNaming.Types.DnsConfig
import Network.AWS.Route53AutoNaming.Types.HealthCheckConfig
import Network.AWS.Route53AutoNaming.Types.HealthCheckCustomConfig
import Network.AWS.Route53AutoNaming.Types.ServiceType

-- | A complex type that contains information about a specified service.
--
-- /See:/ 'newServiceSummary' smart constructor.
data ServiceSummary = ServiceSummary'
  { dnsConfig :: Core.Maybe DnsConfig,
    -- | The date and time that the service was created.
    createDate :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the service
    -- when you create it.
    arn :: Core.Maybe Core.Text,
    -- | The ID that AWS Cloud Map assigned to the service when you created it.
    id :: Core.Maybe Core.Text,
    -- | The name of the service.
    name :: Core.Maybe Core.Text,
    -- | The description that you specify when you create the service.
    description :: Core.Maybe Core.Text,
    healthCheckCustomConfig :: Core.Maybe HealthCheckCustomConfig,
    -- | Describes the systems that can be used to discover the service
    -- instances.
    --
    -- [DNS_HTTP]
    --     The service instances can be discovered using either DNS queries or
    --     the @DiscoverInstances@ API operation.
    --
    -- [HTTP]
    --     The service instances can only be discovered using the
    --     @DiscoverInstances@ API operation.
    --
    -- [DNS]
    --     Reserved.
    type' :: Core.Maybe ServiceType,
    healthCheckConfig :: Core.Maybe HealthCheckConfig,
    -- | The number of instances that are currently associated with the service.
    -- Instances that were previously associated with the service but that have
    -- been deleted are not included in the count. The count might not reflect
    -- pending registrations and deregistrations.
    instanceCount :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ServiceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dnsConfig', 'serviceSummary_dnsConfig' - Undocumented member.
--
-- 'createDate', 'serviceSummary_createDate' - The date and time that the service was created.
--
-- 'arn', 'serviceSummary_arn' - The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the service
-- when you create it.
--
-- 'id', 'serviceSummary_id' - The ID that AWS Cloud Map assigned to the service when you created it.
--
-- 'name', 'serviceSummary_name' - The name of the service.
--
-- 'description', 'serviceSummary_description' - The description that you specify when you create the service.
--
-- 'healthCheckCustomConfig', 'serviceSummary_healthCheckCustomConfig' - Undocumented member.
--
-- 'type'', 'serviceSummary_type' - Describes the systems that can be used to discover the service
-- instances.
--
-- [DNS_HTTP]
--     The service instances can be discovered using either DNS queries or
--     the @DiscoverInstances@ API operation.
--
-- [HTTP]
--     The service instances can only be discovered using the
--     @DiscoverInstances@ API operation.
--
-- [DNS]
--     Reserved.
--
-- 'healthCheckConfig', 'serviceSummary_healthCheckConfig' - Undocumented member.
--
-- 'instanceCount', 'serviceSummary_instanceCount' - The number of instances that are currently associated with the service.
-- Instances that were previously associated with the service but that have
-- been deleted are not included in the count. The count might not reflect
-- pending registrations and deregistrations.
newServiceSummary ::
  ServiceSummary
newServiceSummary =
  ServiceSummary'
    { dnsConfig = Core.Nothing,
      createDate = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing,
      description = Core.Nothing,
      healthCheckCustomConfig = Core.Nothing,
      type' = Core.Nothing,
      healthCheckConfig = Core.Nothing,
      instanceCount = Core.Nothing
    }

-- | Undocumented member.
serviceSummary_dnsConfig :: Lens.Lens' ServiceSummary (Core.Maybe DnsConfig)
serviceSummary_dnsConfig = Lens.lens (\ServiceSummary' {dnsConfig} -> dnsConfig) (\s@ServiceSummary' {} a -> s {dnsConfig = a} :: ServiceSummary)

-- | The date and time that the service was created.
serviceSummary_createDate :: Lens.Lens' ServiceSummary (Core.Maybe Core.UTCTime)
serviceSummary_createDate = Lens.lens (\ServiceSummary' {createDate} -> createDate) (\s@ServiceSummary' {} a -> s {createDate = a} :: ServiceSummary) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the service
-- when you create it.
serviceSummary_arn :: Lens.Lens' ServiceSummary (Core.Maybe Core.Text)
serviceSummary_arn = Lens.lens (\ServiceSummary' {arn} -> arn) (\s@ServiceSummary' {} a -> s {arn = a} :: ServiceSummary)

-- | The ID that AWS Cloud Map assigned to the service when you created it.
serviceSummary_id :: Lens.Lens' ServiceSummary (Core.Maybe Core.Text)
serviceSummary_id = Lens.lens (\ServiceSummary' {id} -> id) (\s@ServiceSummary' {} a -> s {id = a} :: ServiceSummary)

-- | The name of the service.
serviceSummary_name :: Lens.Lens' ServiceSummary (Core.Maybe Core.Text)
serviceSummary_name = Lens.lens (\ServiceSummary' {name} -> name) (\s@ServiceSummary' {} a -> s {name = a} :: ServiceSummary)

-- | The description that you specify when you create the service.
serviceSummary_description :: Lens.Lens' ServiceSummary (Core.Maybe Core.Text)
serviceSummary_description = Lens.lens (\ServiceSummary' {description} -> description) (\s@ServiceSummary' {} a -> s {description = a} :: ServiceSummary)

-- | Undocumented member.
serviceSummary_healthCheckCustomConfig :: Lens.Lens' ServiceSummary (Core.Maybe HealthCheckCustomConfig)
serviceSummary_healthCheckCustomConfig = Lens.lens (\ServiceSummary' {healthCheckCustomConfig} -> healthCheckCustomConfig) (\s@ServiceSummary' {} a -> s {healthCheckCustomConfig = a} :: ServiceSummary)

-- | Describes the systems that can be used to discover the service
-- instances.
--
-- [DNS_HTTP]
--     The service instances can be discovered using either DNS queries or
--     the @DiscoverInstances@ API operation.
--
-- [HTTP]
--     The service instances can only be discovered using the
--     @DiscoverInstances@ API operation.
--
-- [DNS]
--     Reserved.
serviceSummary_type :: Lens.Lens' ServiceSummary (Core.Maybe ServiceType)
serviceSummary_type = Lens.lens (\ServiceSummary' {type'} -> type') (\s@ServiceSummary' {} a -> s {type' = a} :: ServiceSummary)

-- | Undocumented member.
serviceSummary_healthCheckConfig :: Lens.Lens' ServiceSummary (Core.Maybe HealthCheckConfig)
serviceSummary_healthCheckConfig = Lens.lens (\ServiceSummary' {healthCheckConfig} -> healthCheckConfig) (\s@ServiceSummary' {} a -> s {healthCheckConfig = a} :: ServiceSummary)

-- | The number of instances that are currently associated with the service.
-- Instances that were previously associated with the service but that have
-- been deleted are not included in the count. The count might not reflect
-- pending registrations and deregistrations.
serviceSummary_instanceCount :: Lens.Lens' ServiceSummary (Core.Maybe Core.Int)
serviceSummary_instanceCount = Lens.lens (\ServiceSummary' {instanceCount} -> instanceCount) (\s@ServiceSummary' {} a -> s {instanceCount = a} :: ServiceSummary)

instance Core.FromJSON ServiceSummary where
  parseJSON =
    Core.withObject
      "ServiceSummary"
      ( \x ->
          ServiceSummary'
            Core.<$> (x Core..:? "DnsConfig")
            Core.<*> (x Core..:? "CreateDate")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "HealthCheckCustomConfig")
            Core.<*> (x Core..:? "Type")
            Core.<*> (x Core..:? "HealthCheckConfig")
            Core.<*> (x Core..:? "InstanceCount")
      )

instance Core.Hashable ServiceSummary

instance Core.NFData ServiceSummary
