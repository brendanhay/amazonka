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
-- Module      : Amazonka.Redshift.AuthorizeEndpointAccess
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants access to a cluster.
module Amazonka.Redshift.AuthorizeEndpointAccess
  ( -- * Creating a Request
    AuthorizeEndpointAccess (..),
    newAuthorizeEndpointAccess,

    -- * Request Lenses
    authorizeEndpointAccess_clusterIdentifier,
    authorizeEndpointAccess_vpcIds,
    authorizeEndpointAccess_account,

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

-- | /See:/ 'newAuthorizeEndpointAccess' smart constructor.
data AuthorizeEndpointAccess = AuthorizeEndpointAccess'
  { -- | The cluster identifier of the cluster to grant access to.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The virtual private cloud (VPC) identifiers to grant access to.
    vpcIds :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Web Services account ID to grant access to.
    account :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthorizeEndpointAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'authorizeEndpointAccess_clusterIdentifier' - The cluster identifier of the cluster to grant access to.
--
-- 'vpcIds', 'authorizeEndpointAccess_vpcIds' - The virtual private cloud (VPC) identifiers to grant access to.
--
-- 'account', 'authorizeEndpointAccess_account' - The Amazon Web Services account ID to grant access to.
newAuthorizeEndpointAccess ::
  -- | 'account'
  Prelude.Text ->
  AuthorizeEndpointAccess
newAuthorizeEndpointAccess pAccount_ =
  AuthorizeEndpointAccess'
    { clusterIdentifier =
        Prelude.Nothing,
      vpcIds = Prelude.Nothing,
      account = pAccount_
    }

-- | The cluster identifier of the cluster to grant access to.
authorizeEndpointAccess_clusterIdentifier :: Lens.Lens' AuthorizeEndpointAccess (Prelude.Maybe Prelude.Text)
authorizeEndpointAccess_clusterIdentifier = Lens.lens (\AuthorizeEndpointAccess' {clusterIdentifier} -> clusterIdentifier) (\s@AuthorizeEndpointAccess' {} a -> s {clusterIdentifier = a} :: AuthorizeEndpointAccess)

-- | The virtual private cloud (VPC) identifiers to grant access to.
authorizeEndpointAccess_vpcIds :: Lens.Lens' AuthorizeEndpointAccess (Prelude.Maybe [Prelude.Text])
authorizeEndpointAccess_vpcIds = Lens.lens (\AuthorizeEndpointAccess' {vpcIds} -> vpcIds) (\s@AuthorizeEndpointAccess' {} a -> s {vpcIds = a} :: AuthorizeEndpointAccess) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services account ID to grant access to.
authorizeEndpointAccess_account :: Lens.Lens' AuthorizeEndpointAccess Prelude.Text
authorizeEndpointAccess_account = Lens.lens (\AuthorizeEndpointAccess' {account} -> account) (\s@AuthorizeEndpointAccess' {} a -> s {account = a} :: AuthorizeEndpointAccess)

instance Core.AWSRequest AuthorizeEndpointAccess where
  type
    AWSResponse AuthorizeEndpointAccess =
      EndpointAuthorization
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "AuthorizeEndpointAccessResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable AuthorizeEndpointAccess where
  hashWithSalt _salt AuthorizeEndpointAccess' {..} =
    _salt `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` vpcIds
      `Prelude.hashWithSalt` account

instance Prelude.NFData AuthorizeEndpointAccess where
  rnf AuthorizeEndpointAccess' {..} =
    Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf vpcIds
      `Prelude.seq` Prelude.rnf account

instance Data.ToHeaders AuthorizeEndpointAccess where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AuthorizeEndpointAccess where
  toPath = Prelude.const "/"

instance Data.ToQuery AuthorizeEndpointAccess where
  toQuery AuthorizeEndpointAccess' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AuthorizeEndpointAccess" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "ClusterIdentifier" Data.=: clusterIdentifier,
        "VpcIds"
          Data.=: Data.toQuery
            ( Data.toQueryList "VpcIdentifier"
                Prelude.<$> vpcIds
            ),
        "Account" Data.=: account
      ]
