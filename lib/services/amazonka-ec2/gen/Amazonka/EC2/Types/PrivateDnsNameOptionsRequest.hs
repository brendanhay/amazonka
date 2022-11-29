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
-- Module      : Amazonka.EC2.Types.PrivateDnsNameOptionsRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PrivateDnsNameOptionsRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.HostnameType
import qualified Amazonka.Prelude as Prelude

-- | Describes the options for instance hostnames.
--
-- /See:/ 'newPrivateDnsNameOptionsRequest' smart constructor.
data PrivateDnsNameOptionsRequest = PrivateDnsNameOptionsRequest'
  { -- | Indicates whether to respond to DNS queries for instance hostnames with
    -- DNS A records.
    enableResourceNameDnsARecord :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether to respond to DNS queries for instance hostnames with
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
-- Create a value of 'PrivateDnsNameOptionsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableResourceNameDnsARecord', 'privateDnsNameOptionsRequest_enableResourceNameDnsARecord' - Indicates whether to respond to DNS queries for instance hostnames with
-- DNS A records.
--
-- 'enableResourceNameDnsAAAARecord', 'privateDnsNameOptionsRequest_enableResourceNameDnsAAAARecord' - Indicates whether to respond to DNS queries for instance hostnames with
-- DNS AAAA records.
--
-- 'hostnameType', 'privateDnsNameOptionsRequest_hostnameType' - The type of hostname for EC2 instances. For IPv4 only subnets, an
-- instance DNS name must be based on the instance IPv4 address. For IPv6
-- only subnets, an instance DNS name must be based on the instance ID. For
-- dual-stack subnets, you can specify whether DNS names use the instance
-- IPv4 address or the instance ID.
newPrivateDnsNameOptionsRequest ::
  PrivateDnsNameOptionsRequest
newPrivateDnsNameOptionsRequest =
  PrivateDnsNameOptionsRequest'
    { enableResourceNameDnsARecord =
        Prelude.Nothing,
      enableResourceNameDnsAAAARecord =
        Prelude.Nothing,
      hostnameType = Prelude.Nothing
    }

-- | Indicates whether to respond to DNS queries for instance hostnames with
-- DNS A records.
privateDnsNameOptionsRequest_enableResourceNameDnsARecord :: Lens.Lens' PrivateDnsNameOptionsRequest (Prelude.Maybe Prelude.Bool)
privateDnsNameOptionsRequest_enableResourceNameDnsARecord = Lens.lens (\PrivateDnsNameOptionsRequest' {enableResourceNameDnsARecord} -> enableResourceNameDnsARecord) (\s@PrivateDnsNameOptionsRequest' {} a -> s {enableResourceNameDnsARecord = a} :: PrivateDnsNameOptionsRequest)

-- | Indicates whether to respond to DNS queries for instance hostnames with
-- DNS AAAA records.
privateDnsNameOptionsRequest_enableResourceNameDnsAAAARecord :: Lens.Lens' PrivateDnsNameOptionsRequest (Prelude.Maybe Prelude.Bool)
privateDnsNameOptionsRequest_enableResourceNameDnsAAAARecord = Lens.lens (\PrivateDnsNameOptionsRequest' {enableResourceNameDnsAAAARecord} -> enableResourceNameDnsAAAARecord) (\s@PrivateDnsNameOptionsRequest' {} a -> s {enableResourceNameDnsAAAARecord = a} :: PrivateDnsNameOptionsRequest)

-- | The type of hostname for EC2 instances. For IPv4 only subnets, an
-- instance DNS name must be based on the instance IPv4 address. For IPv6
-- only subnets, an instance DNS name must be based on the instance ID. For
-- dual-stack subnets, you can specify whether DNS names use the instance
-- IPv4 address or the instance ID.
privateDnsNameOptionsRequest_hostnameType :: Lens.Lens' PrivateDnsNameOptionsRequest (Prelude.Maybe HostnameType)
privateDnsNameOptionsRequest_hostnameType = Lens.lens (\PrivateDnsNameOptionsRequest' {hostnameType} -> hostnameType) (\s@PrivateDnsNameOptionsRequest' {} a -> s {hostnameType = a} :: PrivateDnsNameOptionsRequest)

instance
  Prelude.Hashable
    PrivateDnsNameOptionsRequest
  where
  hashWithSalt _salt PrivateDnsNameOptionsRequest' {..} =
    _salt
      `Prelude.hashWithSalt` enableResourceNameDnsARecord
      `Prelude.hashWithSalt` enableResourceNameDnsAAAARecord
      `Prelude.hashWithSalt` hostnameType

instance Prelude.NFData PrivateDnsNameOptionsRequest where
  rnf PrivateDnsNameOptionsRequest' {..} =
    Prelude.rnf enableResourceNameDnsARecord
      `Prelude.seq` Prelude.rnf enableResourceNameDnsAAAARecord
      `Prelude.seq` Prelude.rnf hostnameType

instance Core.ToQuery PrivateDnsNameOptionsRequest where
  toQuery PrivateDnsNameOptionsRequest' {..} =
    Prelude.mconcat
      [ "EnableResourceNameDnsARecord"
          Core.=: enableResourceNameDnsARecord,
        "EnableResourceNameDnsAAAARecord"
          Core.=: enableResourceNameDnsAAAARecord,
        "HostnameType" Core.=: hostnameType
      ]
