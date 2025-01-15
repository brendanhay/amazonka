{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Redshift.RevokeEndpointAccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes access to a cluster.
module Amazonka.Redshift.RevokeEndpointAccess
  ( -- * Creating a Request
    RevokeEndpointAccess (..),
    newRevokeEndpointAccess,

    -- * Request Lenses
    revokeEndpointAccess_account,
    revokeEndpointAccess_clusterIdentifier,
    revokeEndpointAccess_force,
    revokeEndpointAccess_vpcIds,

    -- * Destructuring the Response
    EndpointAuthorization (..),
    newEndpointAuthorization,

    -- * Response Lenses
    endpointAuthorization_allowedAllVPCs,
    endpointAuthorization_allowedVPCs,
    endpointAuthorization_authorizeTime,
    endpointAuthorization_clusterIdentifier,
    endpointAuthorization_clusterStatus,
    endpointAuthorization_endpointCount,
    endpointAuthorization_grantee,
    endpointAuthorization_grantor,
    endpointAuthorization_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRevokeEndpointAccess' smart constructor.
data RevokeEndpointAccess = RevokeEndpointAccess'
  { -- | The Amazon Web Services account ID whose access is to be revoked.
    account :: Prelude.Maybe Prelude.Text,
    -- | The cluster to revoke access from.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to force the revoke action. If true, the
    -- Redshift-managed VPC endpoints associated with the endpoint
    -- authorization are also deleted.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The virtual private cloud (VPC) identifiers for which access is to be
    -- revoked.
    vpcIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevokeEndpointAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'account', 'revokeEndpointAccess_account' - The Amazon Web Services account ID whose access is to be revoked.
--
-- 'clusterIdentifier', 'revokeEndpointAccess_clusterIdentifier' - The cluster to revoke access from.
--
-- 'force', 'revokeEndpointAccess_force' - Indicates whether to force the revoke action. If true, the
-- Redshift-managed VPC endpoints associated with the endpoint
-- authorization are also deleted.
--
-- 'vpcIds', 'revokeEndpointAccess_vpcIds' - The virtual private cloud (VPC) identifiers for which access is to be
-- revoked.
newRevokeEndpointAccess ::
  RevokeEndpointAccess
newRevokeEndpointAccess =
  RevokeEndpointAccess'
    { account = Prelude.Nothing,
      clusterIdentifier = Prelude.Nothing,
      force = Prelude.Nothing,
      vpcIds = Prelude.Nothing
    }

-- | The Amazon Web Services account ID whose access is to be revoked.
revokeEndpointAccess_account :: Lens.Lens' RevokeEndpointAccess (Prelude.Maybe Prelude.Text)
revokeEndpointAccess_account = Lens.lens (\RevokeEndpointAccess' {account} -> account) (\s@RevokeEndpointAccess' {} a -> s {account = a} :: RevokeEndpointAccess)

-- | The cluster to revoke access from.
revokeEndpointAccess_clusterIdentifier :: Lens.Lens' RevokeEndpointAccess (Prelude.Maybe Prelude.Text)
revokeEndpointAccess_clusterIdentifier = Lens.lens (\RevokeEndpointAccess' {clusterIdentifier} -> clusterIdentifier) (\s@RevokeEndpointAccess' {} a -> s {clusterIdentifier = a} :: RevokeEndpointAccess)

-- | Indicates whether to force the revoke action. If true, the
-- Redshift-managed VPC endpoints associated with the endpoint
-- authorization are also deleted.
revokeEndpointAccess_force :: Lens.Lens' RevokeEndpointAccess (Prelude.Maybe Prelude.Bool)
revokeEndpointAccess_force = Lens.lens (\RevokeEndpointAccess' {force} -> force) (\s@RevokeEndpointAccess' {} a -> s {force = a} :: RevokeEndpointAccess)

-- | The virtual private cloud (VPC) identifiers for which access is to be
-- revoked.
revokeEndpointAccess_vpcIds :: Lens.Lens' RevokeEndpointAccess (Prelude.Maybe [Prelude.Text])
revokeEndpointAccess_vpcIds = Lens.lens (\RevokeEndpointAccess' {vpcIds} -> vpcIds) (\s@RevokeEndpointAccess' {} a -> s {vpcIds = a} :: RevokeEndpointAccess) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest RevokeEndpointAccess where
  type
    AWSResponse RevokeEndpointAccess =
      EndpointAuthorization
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RevokeEndpointAccessResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable RevokeEndpointAccess where
  hashWithSalt _salt RevokeEndpointAccess' {..} =
    _salt
      `Prelude.hashWithSalt` account
      `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` force
      `Prelude.hashWithSalt` vpcIds

instance Prelude.NFData RevokeEndpointAccess where
  rnf RevokeEndpointAccess' {..} =
    Prelude.rnf account `Prelude.seq`
      Prelude.rnf clusterIdentifier `Prelude.seq`
        Prelude.rnf force `Prelude.seq`
          Prelude.rnf vpcIds

instance Data.ToHeaders RevokeEndpointAccess where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RevokeEndpointAccess where
  toPath = Prelude.const "/"

instance Data.ToQuery RevokeEndpointAccess where
  toQuery RevokeEndpointAccess' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("RevokeEndpointAccess" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "Account" Data.=: account,
        "ClusterIdentifier" Data.=: clusterIdentifier,
        "Force" Data.=: force,
        "VpcIds"
          Data.=: Data.toQuery
            ( Data.toQueryList "VpcIdentifier"
                Prelude.<$> vpcIds
            )
      ]
