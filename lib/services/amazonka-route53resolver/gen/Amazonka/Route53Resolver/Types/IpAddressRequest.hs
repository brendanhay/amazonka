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
-- Module      : Amazonka.Route53Resolver.Types.IpAddressRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.IpAddressRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | In a
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_CreateResolverEndpoint.html CreateResolverEndpoint>
-- request, the IP address that DNS queries originate from (for outbound
-- endpoints) or that you forward DNS queries to (for inbound endpoints).
-- @IpAddressRequest@ also includes the ID of the subnet that contains the
-- IP address.
--
-- /See:/ 'newIpAddressRequest' smart constructor.
data IpAddressRequest = IpAddressRequest'
  { -- | The IP address that you want to use for DNS queries.
    ip :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet that contains the IP address.
    subnetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpAddressRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ip', 'ipAddressRequest_ip' - The IP address that you want to use for DNS queries.
--
-- 'subnetId', 'ipAddressRequest_subnetId' - The ID of the subnet that contains the IP address.
newIpAddressRequest ::
  -- | 'subnetId'
  Prelude.Text ->
  IpAddressRequest
newIpAddressRequest pSubnetId_ =
  IpAddressRequest'
    { ip = Prelude.Nothing,
      subnetId = pSubnetId_
    }

-- | The IP address that you want to use for DNS queries.
ipAddressRequest_ip :: Lens.Lens' IpAddressRequest (Prelude.Maybe Prelude.Text)
ipAddressRequest_ip = Lens.lens (\IpAddressRequest' {ip} -> ip) (\s@IpAddressRequest' {} a -> s {ip = a} :: IpAddressRequest)

-- | The ID of the subnet that contains the IP address.
ipAddressRequest_subnetId :: Lens.Lens' IpAddressRequest Prelude.Text
ipAddressRequest_subnetId = Lens.lens (\IpAddressRequest' {subnetId} -> subnetId) (\s@IpAddressRequest' {} a -> s {subnetId = a} :: IpAddressRequest)

instance Prelude.Hashable IpAddressRequest where
  hashWithSalt _salt IpAddressRequest' {..} =
    _salt `Prelude.hashWithSalt` ip
      `Prelude.hashWithSalt` subnetId

instance Prelude.NFData IpAddressRequest where
  rnf IpAddressRequest' {..} =
    Prelude.rnf ip `Prelude.seq` Prelude.rnf subnetId

instance Data.ToJSON IpAddressRequest where
  toJSON IpAddressRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Ip" Data..=) Prelude.<$> ip,
            Prelude.Just ("SubnetId" Data..= subnetId)
          ]
      )
