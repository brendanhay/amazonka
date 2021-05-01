{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.RDS.PromoteReadReplicaDBCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Promotes a read replica DB cluster to a standalone DB cluster.
--
-- This action only applies to Aurora DB clusters.
module Network.AWS.RDS.PromoteReadReplicaDBCluster
  ( -- * Creating a Request
    PromoteReadReplicaDBCluster (..),
    newPromoteReadReplicaDBCluster,

    -- * Request Lenses
    promoteReadReplicaDBCluster_dbClusterIdentifier,

    -- * Destructuring the Response
    PromoteReadReplicaDBClusterResponse (..),
    newPromoteReadReplicaDBClusterResponse,

    -- * Response Lenses
    promoteReadReplicaDBClusterResponse_dbCluster,
    promoteReadReplicaDBClusterResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newPromoteReadReplicaDBCluster' smart constructor.
data PromoteReadReplicaDBCluster = PromoteReadReplicaDBCluster'
  { -- | The identifier of the DB cluster read replica to promote. This parameter
    -- isn\'t case-sensitive.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing DB cluster read replica.
    --
    -- Example: @my-cluster-replica1@
    dbClusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PromoteReadReplicaDBCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterIdentifier', 'promoteReadReplicaDBCluster_dbClusterIdentifier' - The identifier of the DB cluster read replica to promote. This parameter
-- isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DB cluster read replica.
--
-- Example: @my-cluster-replica1@
newPromoteReadReplicaDBCluster ::
  -- | 'dbClusterIdentifier'
  Prelude.Text ->
  PromoteReadReplicaDBCluster
newPromoteReadReplicaDBCluster pDBClusterIdentifier_ =
  PromoteReadReplicaDBCluster'
    { dbClusterIdentifier =
        pDBClusterIdentifier_
    }

-- | The identifier of the DB cluster read replica to promote. This parameter
-- isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DB cluster read replica.
--
-- Example: @my-cluster-replica1@
promoteReadReplicaDBCluster_dbClusterIdentifier :: Lens.Lens' PromoteReadReplicaDBCluster Prelude.Text
promoteReadReplicaDBCluster_dbClusterIdentifier = Lens.lens (\PromoteReadReplicaDBCluster' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@PromoteReadReplicaDBCluster' {} a -> s {dbClusterIdentifier = a} :: PromoteReadReplicaDBCluster)

instance
  Prelude.AWSRequest
    PromoteReadReplicaDBCluster
  where
  type
    Rs PromoteReadReplicaDBCluster =
      PromoteReadReplicaDBClusterResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "PromoteReadReplicaDBClusterResult"
      ( \s h x ->
          PromoteReadReplicaDBClusterResponse'
            Prelude.<$> (x Prelude..@? "DBCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PromoteReadReplicaDBCluster

instance Prelude.NFData PromoteReadReplicaDBCluster

instance
  Prelude.ToHeaders
    PromoteReadReplicaDBCluster
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath PromoteReadReplicaDBCluster where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PromoteReadReplicaDBCluster where
  toQuery PromoteReadReplicaDBCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "PromoteReadReplicaDBCluster" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2014-10-31" :: Prelude.ByteString),
        "DBClusterIdentifier" Prelude.=: dbClusterIdentifier
      ]

-- | /See:/ 'newPromoteReadReplicaDBClusterResponse' smart constructor.
data PromoteReadReplicaDBClusterResponse = PromoteReadReplicaDBClusterResponse'
  { dbCluster :: Prelude.Maybe DBCluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PromoteReadReplicaDBClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbCluster', 'promoteReadReplicaDBClusterResponse_dbCluster' - Undocumented member.
--
-- 'httpStatus', 'promoteReadReplicaDBClusterResponse_httpStatus' - The response's http status code.
newPromoteReadReplicaDBClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PromoteReadReplicaDBClusterResponse
newPromoteReadReplicaDBClusterResponse pHttpStatus_ =
  PromoteReadReplicaDBClusterResponse'
    { dbCluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
promoteReadReplicaDBClusterResponse_dbCluster :: Lens.Lens' PromoteReadReplicaDBClusterResponse (Prelude.Maybe DBCluster)
promoteReadReplicaDBClusterResponse_dbCluster = Lens.lens (\PromoteReadReplicaDBClusterResponse' {dbCluster} -> dbCluster) (\s@PromoteReadReplicaDBClusterResponse' {} a -> s {dbCluster = a} :: PromoteReadReplicaDBClusterResponse)

-- | The response's http status code.
promoteReadReplicaDBClusterResponse_httpStatus :: Lens.Lens' PromoteReadReplicaDBClusterResponse Prelude.Int
promoteReadReplicaDBClusterResponse_httpStatus = Lens.lens (\PromoteReadReplicaDBClusterResponse' {httpStatus} -> httpStatus) (\s@PromoteReadReplicaDBClusterResponse' {} a -> s {httpStatus = a} :: PromoteReadReplicaDBClusterResponse)

instance
  Prelude.NFData
    PromoteReadReplicaDBClusterResponse
