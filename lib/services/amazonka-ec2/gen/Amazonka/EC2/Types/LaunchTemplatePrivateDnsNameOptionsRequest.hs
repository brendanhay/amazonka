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
-- Module      : Amazonka.EC2.Types.LaunchTemplatePrivateDnsNameOptionsRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplatePrivateDnsNameOptionsRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.HostnameType
import qualified Amazonka.Prelude as Prelude

-- | Describes the options for instance hostnames.
--
-- /See:/ 'newLaunchTemplatePrivateDnsNameOptionsRequest' smart constructor.
data LaunchTemplatePrivateDnsNameOptionsRequest = LaunchTemplatePrivateDnsNameOptionsRequest'
  { -- | Indicates whether to respond to DNS queries for instance hostnames with
    -- DNS AAAA records.
    enableResourceNameDnsAAAARecord :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether to respond to DNS queries for instance hostnames with
    -- DNS A records.
    enableResourceNameDnsARecord :: Prelude.Maybe Prelude.Bool,
    -- | The type of hostname for Amazon EC2 instances. For IPv4 only subnets, an
    -- instance DNS name must be based on the instance IPv4 address. For IPv6
    -- native subnets, an instance DNS name must be based on the instance ID.
    -- For dual-stack subnets, you can specify whether DNS names use the
    -- instance IPv4 address or the instance ID.
    hostnameType :: Prelude.Maybe HostnameType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplatePrivateDnsNameOptionsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableResourceNameDnsAAAARecord', 'launchTemplatePrivateDnsNameOptionsRequest_enableResourceNameDnsAAAARecord' - Indicates whether to respond to DNS queries for instance hostnames with
-- DNS AAAA records.
--
-- 'enableResourceNameDnsARecord', 'launchTemplatePrivateDnsNameOptionsRequest_enableResourceNameDnsARecord' - Indicates whether to respond to DNS queries for instance hostnames with
-- DNS A records.
--
-- 'hostnameType', 'launchTemplatePrivateDnsNameOptionsRequest_hostnameType' - The type of hostname for Amazon EC2 instances. For IPv4 only subnets, an
-- instance DNS name must be based on the instance IPv4 address. For IPv6
-- native subnets, an instance DNS name must be based on the instance ID.
-- For dual-stack subnets, you can specify whether DNS names use the
-- instance IPv4 address or the instance ID.
newLaunchTemplatePrivateDnsNameOptionsRequest ::
  LaunchTemplatePrivateDnsNameOptionsRequest
newLaunchTemplatePrivateDnsNameOptionsRequest =
  LaunchTemplatePrivateDnsNameOptionsRequest'
    { enableResourceNameDnsAAAARecord =
        Prelude.Nothing,
      enableResourceNameDnsARecord =
        Prelude.Nothing,
      hostnameType = Prelude.Nothing
    }

-- | Indicates whether to respond to DNS queries for instance hostnames with
-- DNS AAAA records.
launchTemplatePrivateDnsNameOptionsRequest_enableResourceNameDnsAAAARecord :: Lens.Lens' LaunchTemplatePrivateDnsNameOptionsRequest (Prelude.Maybe Prelude.Bool)
launchTemplatePrivateDnsNameOptionsRequest_enableResourceNameDnsAAAARecord = Lens.lens (\LaunchTemplatePrivateDnsNameOptionsRequest' {enableResourceNameDnsAAAARecord} -> enableResourceNameDnsAAAARecord) (\s@LaunchTemplatePrivateDnsNameOptionsRequest' {} a -> s {enableResourceNameDnsAAAARecord = a} :: LaunchTemplatePrivateDnsNameOptionsRequest)

-- | Indicates whether to respond to DNS queries for instance hostnames with
-- DNS A records.
launchTemplatePrivateDnsNameOptionsRequest_enableResourceNameDnsARecord :: Lens.Lens' LaunchTemplatePrivateDnsNameOptionsRequest (Prelude.Maybe Prelude.Bool)
launchTemplatePrivateDnsNameOptionsRequest_enableResourceNameDnsARecord = Lens.lens (\LaunchTemplatePrivateDnsNameOptionsRequest' {enableResourceNameDnsARecord} -> enableResourceNameDnsARecord) (\s@LaunchTemplatePrivateDnsNameOptionsRequest' {} a -> s {enableResourceNameDnsARecord = a} :: LaunchTemplatePrivateDnsNameOptionsRequest)

-- | The type of hostname for Amazon EC2 instances. For IPv4 only subnets, an
-- instance DNS name must be based on the instance IPv4 address. For IPv6
-- native subnets, an instance DNS name must be based on the instance ID.
-- For dual-stack subnets, you can specify whether DNS names use the
-- instance IPv4 address or the instance ID.
launchTemplatePrivateDnsNameOptionsRequest_hostnameType :: Lens.Lens' LaunchTemplatePrivateDnsNameOptionsRequest (Prelude.Maybe HostnameType)
launchTemplatePrivateDnsNameOptionsRequest_hostnameType = Lens.lens (\LaunchTemplatePrivateDnsNameOptionsRequest' {hostnameType} -> hostnameType) (\s@LaunchTemplatePrivateDnsNameOptionsRequest' {} a -> s {hostnameType = a} :: LaunchTemplatePrivateDnsNameOptionsRequest)

instance
  Prelude.Hashable
    LaunchTemplatePrivateDnsNameOptionsRequest
  where
  hashWithSalt
    _salt
    LaunchTemplatePrivateDnsNameOptionsRequest' {..} =
      _salt
        `Prelude.hashWithSalt` enableResourceNameDnsAAAARecord
        `Prelude.hashWithSalt` enableResourceNameDnsARecord
        `Prelude.hashWithSalt` hostnameType

instance
  Prelude.NFData
    LaunchTemplatePrivateDnsNameOptionsRequest
  where
  rnf LaunchTemplatePrivateDnsNameOptionsRequest' {..} =
    Prelude.rnf enableResourceNameDnsAAAARecord
      `Prelude.seq` Prelude.rnf enableResourceNameDnsARecord
      `Prelude.seq` Prelude.rnf hostnameType

instance
  Data.ToQuery
    LaunchTemplatePrivateDnsNameOptionsRequest
  where
  toQuery
    LaunchTemplatePrivateDnsNameOptionsRequest' {..} =
      Prelude.mconcat
        [ "EnableResourceNameDnsAAAARecord"
            Data.=: enableResourceNameDnsAAAARecord,
          "EnableResourceNameDnsARecord"
            Data.=: enableResourceNameDnsARecord,
          "HostnameType" Data.=: hostnameType
        ]
