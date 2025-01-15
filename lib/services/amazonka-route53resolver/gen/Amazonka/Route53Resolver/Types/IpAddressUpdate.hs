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
-- Module      : Amazonka.Route53Resolver.Types.IpAddressUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.IpAddressUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | In an
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_UpdateResolverEndpoint.html UpdateResolverEndpoint>
-- request, information about an IP address to update.
--
-- /See:/ 'newIpAddressUpdate' smart constructor.
data IpAddressUpdate = IpAddressUpdate'
  { -- | The new IP address.
    ip :: Prelude.Maybe Prelude.Text,
    -- | /Only when removing an IP address from a Resolver endpoint/: The ID of
    -- the IP address that you want to remove. To get this ID, use
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_GetResolverEndpoint.html GetResolverEndpoint>.
    ipId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet that includes the IP address that you want to
    -- update. To get this ID, use
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_GetResolverEndpoint.html GetResolverEndpoint>.
    subnetId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpAddressUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ip', 'ipAddressUpdate_ip' - The new IP address.
--
-- 'ipId', 'ipAddressUpdate_ipId' - /Only when removing an IP address from a Resolver endpoint/: The ID of
-- the IP address that you want to remove. To get this ID, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_GetResolverEndpoint.html GetResolverEndpoint>.
--
-- 'subnetId', 'ipAddressUpdate_subnetId' - The ID of the subnet that includes the IP address that you want to
-- update. To get this ID, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_GetResolverEndpoint.html GetResolverEndpoint>.
newIpAddressUpdate ::
  IpAddressUpdate
newIpAddressUpdate =
  IpAddressUpdate'
    { ip = Prelude.Nothing,
      ipId = Prelude.Nothing,
      subnetId = Prelude.Nothing
    }

-- | The new IP address.
ipAddressUpdate_ip :: Lens.Lens' IpAddressUpdate (Prelude.Maybe Prelude.Text)
ipAddressUpdate_ip = Lens.lens (\IpAddressUpdate' {ip} -> ip) (\s@IpAddressUpdate' {} a -> s {ip = a} :: IpAddressUpdate)

-- | /Only when removing an IP address from a Resolver endpoint/: The ID of
-- the IP address that you want to remove. To get this ID, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_GetResolverEndpoint.html GetResolverEndpoint>.
ipAddressUpdate_ipId :: Lens.Lens' IpAddressUpdate (Prelude.Maybe Prelude.Text)
ipAddressUpdate_ipId = Lens.lens (\IpAddressUpdate' {ipId} -> ipId) (\s@IpAddressUpdate' {} a -> s {ipId = a} :: IpAddressUpdate)

-- | The ID of the subnet that includes the IP address that you want to
-- update. To get this ID, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_GetResolverEndpoint.html GetResolverEndpoint>.
ipAddressUpdate_subnetId :: Lens.Lens' IpAddressUpdate (Prelude.Maybe Prelude.Text)
ipAddressUpdate_subnetId = Lens.lens (\IpAddressUpdate' {subnetId} -> subnetId) (\s@IpAddressUpdate' {} a -> s {subnetId = a} :: IpAddressUpdate)

instance Prelude.Hashable IpAddressUpdate where
  hashWithSalt _salt IpAddressUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` ip
      `Prelude.hashWithSalt` ipId
      `Prelude.hashWithSalt` subnetId

instance Prelude.NFData IpAddressUpdate where
  rnf IpAddressUpdate' {..} =
    Prelude.rnf ip `Prelude.seq`
      Prelude.rnf ipId `Prelude.seq`
        Prelude.rnf subnetId

instance Data.ToJSON IpAddressUpdate where
  toJSON IpAddressUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Ip" Data..=) Prelude.<$> ip,
            ("IpId" Data..=) Prelude.<$> ipId,
            ("SubnetId" Data..=) Prelude.<$> subnetId
          ]
      )
