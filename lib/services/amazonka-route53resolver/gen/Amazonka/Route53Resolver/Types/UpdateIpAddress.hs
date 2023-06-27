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
-- Module      : Amazonka.Route53Resolver.Types.UpdateIpAddress
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.UpdateIpAddress where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the IP address type in response to
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_UpdateResolverEndpoint.html UpdateResolverEndpoint>.
--
-- /See:/ 'newUpdateIpAddress' smart constructor.
data UpdateIpAddress = UpdateIpAddress'
  { -- | The ID of the IP address, specified by the @ResolverEndpointId@.
    ipId :: Prelude.Text,
    -- | The IPv6 address that you want to use for DNS queries.
    ipv6 :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIpAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipId', 'updateIpAddress_ipId' - The ID of the IP address, specified by the @ResolverEndpointId@.
--
-- 'ipv6', 'updateIpAddress_ipv6' - The IPv6 address that you want to use for DNS queries.
newUpdateIpAddress ::
  -- | 'ipId'
  Prelude.Text ->
  -- | 'ipv6'
  Prelude.Text ->
  UpdateIpAddress
newUpdateIpAddress pIpId_ pIpv6_ =
  UpdateIpAddress' {ipId = pIpId_, ipv6 = pIpv6_}

-- | The ID of the IP address, specified by the @ResolverEndpointId@.
updateIpAddress_ipId :: Lens.Lens' UpdateIpAddress Prelude.Text
updateIpAddress_ipId = Lens.lens (\UpdateIpAddress' {ipId} -> ipId) (\s@UpdateIpAddress' {} a -> s {ipId = a} :: UpdateIpAddress)

-- | The IPv6 address that you want to use for DNS queries.
updateIpAddress_ipv6 :: Lens.Lens' UpdateIpAddress Prelude.Text
updateIpAddress_ipv6 = Lens.lens (\UpdateIpAddress' {ipv6} -> ipv6) (\s@UpdateIpAddress' {} a -> s {ipv6 = a} :: UpdateIpAddress)

instance Prelude.Hashable UpdateIpAddress where
  hashWithSalt _salt UpdateIpAddress' {..} =
    _salt
      `Prelude.hashWithSalt` ipId
      `Prelude.hashWithSalt` ipv6

instance Prelude.NFData UpdateIpAddress where
  rnf UpdateIpAddress' {..} =
    Prelude.rnf ipId `Prelude.seq` Prelude.rnf ipv6

instance Data.ToJSON UpdateIpAddress where
  toJSON UpdateIpAddress' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("IpId" Data..= ipId),
            Prelude.Just ("Ipv6" Data..= ipv6)
          ]
      )
