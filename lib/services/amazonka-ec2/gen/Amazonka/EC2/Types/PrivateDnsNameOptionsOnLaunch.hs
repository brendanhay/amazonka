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
-- Module      : Amazonka.EC2.Types.PrivateDnsNameOptionsOnLaunch
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PrivateDnsNameOptionsOnLaunch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.HostnameType
import qualified Amazonka.Prelude as Prelude

-- | Describes the options for instance hostnames.
--
-- /See:/ 'newPrivateDnsNameOptionsOnLaunch' smart constructor.
data PrivateDnsNameOptionsOnLaunch = PrivateDnsNameOptionsOnLaunch'
  { -- | Indicates whether to respond to DNS queries for instance hostnames with
    -- DNS A records.
    enableResourceNameDnsARecord :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether to respond to DNS queries for instance hostname with
    -- DNS AAAA records.
    enableResourceNameDnsAAAARecord :: Prelude.Maybe Prelude.Bool,
    -- | The type of hostname for EC2 instances. For IPv4 only subnets, an
    -- instance DNS name must be based on the instance IPv4 address. For IPv6
    -- only subnets, an instance DNS name must be based on the instance ID. For
    -- dual-stack subnets, you can specify whether DNS names use the instance
    -- IPv4 address or the instance ID.
    hostnameType :: Prelude.Maybe HostnameType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrivateDnsNameOptionsOnLaunch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableResourceNameDnsARecord', 'privateDnsNameOptionsOnLaunch_enableResourceNameDnsARecord' - Indicates whether to respond to DNS queries for instance hostnames with
-- DNS A records.
--
-- 'enableResourceNameDnsAAAARecord', 'privateDnsNameOptionsOnLaunch_enableResourceNameDnsAAAARecord' - Indicates whether to respond to DNS queries for instance hostname with
-- DNS AAAA records.
--
-- 'hostnameType', 'privateDnsNameOptionsOnLaunch_hostnameType' - The type of hostname for EC2 instances. For IPv4 only subnets, an
-- instance DNS name must be based on the instance IPv4 address. For IPv6
-- only subnets, an instance DNS name must be based on the instance ID. For
-- dual-stack subnets, you can specify whether DNS names use the instance
-- IPv4 address or the instance ID.
newPrivateDnsNameOptionsOnLaunch ::
  PrivateDnsNameOptionsOnLaunch
newPrivateDnsNameOptionsOnLaunch =
  PrivateDnsNameOptionsOnLaunch'
    { enableResourceNameDnsARecord =
        Prelude.Nothing,
      enableResourceNameDnsAAAARecord =
        Prelude.Nothing,
      hostnameType = Prelude.Nothing
    }

-- | Indicates whether to respond to DNS queries for instance hostnames with
-- DNS A records.
privateDnsNameOptionsOnLaunch_enableResourceNameDnsARecord :: Lens.Lens' PrivateDnsNameOptionsOnLaunch (Prelude.Maybe Prelude.Bool)
privateDnsNameOptionsOnLaunch_enableResourceNameDnsARecord = Lens.lens (\PrivateDnsNameOptionsOnLaunch' {enableResourceNameDnsARecord} -> enableResourceNameDnsARecord) (\s@PrivateDnsNameOptionsOnLaunch' {} a -> s {enableResourceNameDnsARecord = a} :: PrivateDnsNameOptionsOnLaunch)

-- | Indicates whether to respond to DNS queries for instance hostname with
-- DNS AAAA records.
privateDnsNameOptionsOnLaunch_enableResourceNameDnsAAAARecord :: Lens.Lens' PrivateDnsNameOptionsOnLaunch (Prelude.Maybe Prelude.Bool)
privateDnsNameOptionsOnLaunch_enableResourceNameDnsAAAARecord = Lens.lens (\PrivateDnsNameOptionsOnLaunch' {enableResourceNameDnsAAAARecord} -> enableResourceNameDnsAAAARecord) (\s@PrivateDnsNameOptionsOnLaunch' {} a -> s {enableResourceNameDnsAAAARecord = a} :: PrivateDnsNameOptionsOnLaunch)

-- | The type of hostname for EC2 instances. For IPv4 only subnets, an
-- instance DNS name must be based on the instance IPv4 address. For IPv6
-- only subnets, an instance DNS name must be based on the instance ID. For
-- dual-stack subnets, you can specify whether DNS names use the instance
-- IPv4 address or the instance ID.
privateDnsNameOptionsOnLaunch_hostnameType :: Lens.Lens' PrivateDnsNameOptionsOnLaunch (Prelude.Maybe HostnameType)
privateDnsNameOptionsOnLaunch_hostnameType = Lens.lens (\PrivateDnsNameOptionsOnLaunch' {hostnameType} -> hostnameType) (\s@PrivateDnsNameOptionsOnLaunch' {} a -> s {hostnameType = a} :: PrivateDnsNameOptionsOnLaunch)

instance Core.FromXML PrivateDnsNameOptionsOnLaunch where
  parseXML x =
    PrivateDnsNameOptionsOnLaunch'
      Prelude.<$> (x Core..@? "enableResourceNameDnsARecord")
      Prelude.<*> (x Core..@? "enableResourceNameDnsAAAARecord")
      Prelude.<*> (x Core..@? "hostnameType")

instance
  Prelude.Hashable
    PrivateDnsNameOptionsOnLaunch
  where
  hashWithSalt _salt PrivateDnsNameOptionsOnLaunch' {..} =
    _salt
      `Prelude.hashWithSalt` enableResourceNameDnsARecord
      `Prelude.hashWithSalt` enableResourceNameDnsAAAARecord
      `Prelude.hashWithSalt` hostnameType

instance Prelude.NFData PrivateDnsNameOptionsOnLaunch where
  rnf PrivateDnsNameOptionsOnLaunch' {..} =
    Prelude.rnf enableResourceNameDnsARecord
      `Prelude.seq` Prelude.rnf enableResourceNameDnsAAAARecord
      `Prelude.seq` Prelude.rnf hostnameType
