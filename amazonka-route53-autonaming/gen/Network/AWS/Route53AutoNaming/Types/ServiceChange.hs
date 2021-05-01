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
-- Module      : Network.AWS.Route53AutoNaming.Types.ServiceChange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.ServiceChange where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53AutoNaming.Types.DnsConfigChange
import Network.AWS.Route53AutoNaming.Types.HealthCheckConfig

-- | A complex type that contains changes to an existing service.
--
-- /See:/ 'newServiceChange' smart constructor.
data ServiceChange = ServiceChange'
  { -- | A complex type that contains information about the Route 53 DNS records
    -- that you want AWS Cloud Map to create when you register an instance.
    dnsConfig :: Prelude.Maybe DnsConfigChange,
    -- | A description for the service.
    description :: Prelude.Maybe Prelude.Text,
    healthCheckConfig :: Prelude.Maybe HealthCheckConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ServiceChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dnsConfig', 'serviceChange_dnsConfig' - A complex type that contains information about the Route 53 DNS records
-- that you want AWS Cloud Map to create when you register an instance.
--
-- 'description', 'serviceChange_description' - A description for the service.
--
-- 'healthCheckConfig', 'serviceChange_healthCheckConfig' - Undocumented member.
newServiceChange ::
  ServiceChange
newServiceChange =
  ServiceChange'
    { dnsConfig = Prelude.Nothing,
      description = Prelude.Nothing,
      healthCheckConfig = Prelude.Nothing
    }

-- | A complex type that contains information about the Route 53 DNS records
-- that you want AWS Cloud Map to create when you register an instance.
serviceChange_dnsConfig :: Lens.Lens' ServiceChange (Prelude.Maybe DnsConfigChange)
serviceChange_dnsConfig = Lens.lens (\ServiceChange' {dnsConfig} -> dnsConfig) (\s@ServiceChange' {} a -> s {dnsConfig = a} :: ServiceChange)

-- | A description for the service.
serviceChange_description :: Lens.Lens' ServiceChange (Prelude.Maybe Prelude.Text)
serviceChange_description = Lens.lens (\ServiceChange' {description} -> description) (\s@ServiceChange' {} a -> s {description = a} :: ServiceChange)

-- | Undocumented member.
serviceChange_healthCheckConfig :: Lens.Lens' ServiceChange (Prelude.Maybe HealthCheckConfig)
serviceChange_healthCheckConfig = Lens.lens (\ServiceChange' {healthCheckConfig} -> healthCheckConfig) (\s@ServiceChange' {} a -> s {healthCheckConfig = a} :: ServiceChange)

instance Prelude.Hashable ServiceChange

instance Prelude.NFData ServiceChange

instance Prelude.ToJSON ServiceChange where
  toJSON ServiceChange' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DnsConfig" Prelude..=) Prelude.<$> dnsConfig,
            ("Description" Prelude..=) Prelude.<$> description,
            ("HealthCheckConfig" Prelude..=)
              Prelude.<$> healthCheckConfig
          ]
      )
