{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53AutoNaming.Types.DnsConfig
import Network.AWS.Route53AutoNaming.Types.HealthCheckConfig
import Network.AWS.Route53AutoNaming.Types.HealthCheckCustomConfig
import Network.AWS.Route53AutoNaming.Types.ServiceType

-- | A complex type that contains information about a specified service.
--
-- /See:/ 'newServiceSummary' smart constructor.
data ServiceSummary = ServiceSummary'
  { dnsConfig :: Prelude.Maybe DnsConfig,
    -- | The date and time that the service was created.
    createDate :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the service
    -- when you create it.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID that AWS Cloud Map assigned to the service when you created it.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the service.
    name :: Prelude.Maybe Prelude.Text,
    -- | The description that you specify when you create the service.
    description :: Prelude.Maybe Prelude.Text,
    healthCheckCustomConfig :: Prelude.Maybe HealthCheckCustomConfig,
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
    type' :: Prelude.Maybe ServiceType,
    healthCheckConfig :: Prelude.Maybe HealthCheckConfig,
    -- | The number of instances that are currently associated with the service.
    -- Instances that were previously associated with the service but that have
    -- been deleted are not included in the count. The count might not reflect
    -- pending registrations and deregistrations.
    instanceCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { dnsConfig = Prelude.Nothing,
      createDate = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      healthCheckCustomConfig = Prelude.Nothing,
      type' = Prelude.Nothing,
      healthCheckConfig = Prelude.Nothing,
      instanceCount = Prelude.Nothing
    }

-- | Undocumented member.
serviceSummary_dnsConfig :: Lens.Lens' ServiceSummary (Prelude.Maybe DnsConfig)
serviceSummary_dnsConfig = Lens.lens (\ServiceSummary' {dnsConfig} -> dnsConfig) (\s@ServiceSummary' {} a -> s {dnsConfig = a} :: ServiceSummary)

-- | The date and time that the service was created.
serviceSummary_createDate :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.UTCTime)
serviceSummary_createDate = Lens.lens (\ServiceSummary' {createDate} -> createDate) (\s@ServiceSummary' {} a -> s {createDate = a} :: ServiceSummary) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) that AWS Cloud Map assigns to the service
-- when you create it.
serviceSummary_arn :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_arn = Lens.lens (\ServiceSummary' {arn} -> arn) (\s@ServiceSummary' {} a -> s {arn = a} :: ServiceSummary)

-- | The ID that AWS Cloud Map assigned to the service when you created it.
serviceSummary_id :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_id = Lens.lens (\ServiceSummary' {id} -> id) (\s@ServiceSummary' {} a -> s {id = a} :: ServiceSummary)

-- | The name of the service.
serviceSummary_name :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_name = Lens.lens (\ServiceSummary' {name} -> name) (\s@ServiceSummary' {} a -> s {name = a} :: ServiceSummary)

-- | The description that you specify when you create the service.
serviceSummary_description :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_description = Lens.lens (\ServiceSummary' {description} -> description) (\s@ServiceSummary' {} a -> s {description = a} :: ServiceSummary)

-- | Undocumented member.
serviceSummary_healthCheckCustomConfig :: Lens.Lens' ServiceSummary (Prelude.Maybe HealthCheckCustomConfig)
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
serviceSummary_type :: Lens.Lens' ServiceSummary (Prelude.Maybe ServiceType)
serviceSummary_type = Lens.lens (\ServiceSummary' {type'} -> type') (\s@ServiceSummary' {} a -> s {type' = a} :: ServiceSummary)

-- | Undocumented member.
serviceSummary_healthCheckConfig :: Lens.Lens' ServiceSummary (Prelude.Maybe HealthCheckConfig)
serviceSummary_healthCheckConfig = Lens.lens (\ServiceSummary' {healthCheckConfig} -> healthCheckConfig) (\s@ServiceSummary' {} a -> s {healthCheckConfig = a} :: ServiceSummary)

-- | The number of instances that are currently associated with the service.
-- Instances that were previously associated with the service but that have
-- been deleted are not included in the count. The count might not reflect
-- pending registrations and deregistrations.
serviceSummary_instanceCount :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Int)
serviceSummary_instanceCount = Lens.lens (\ServiceSummary' {instanceCount} -> instanceCount) (\s@ServiceSummary' {} a -> s {instanceCount = a} :: ServiceSummary)

instance Prelude.FromJSON ServiceSummary where
  parseJSON =
    Prelude.withObject
      "ServiceSummary"
      ( \x ->
          ServiceSummary'
            Prelude.<$> (x Prelude..:? "DnsConfig")
            Prelude.<*> (x Prelude..:? "CreateDate")
            Prelude.<*> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "HealthCheckCustomConfig")
            Prelude.<*> (x Prelude..:? "Type")
            Prelude.<*> (x Prelude..:? "HealthCheckConfig")
            Prelude.<*> (x Prelude..:? "InstanceCount")
      )

instance Prelude.Hashable ServiceSummary

instance Prelude.NFData ServiceSummary
