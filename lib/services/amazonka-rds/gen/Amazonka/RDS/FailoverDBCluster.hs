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
-- Module      : Amazonka.RDS.FailoverDBCluster
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Forces a failover for a DB cluster.
--
-- For an Aurora DB cluster, failover for a DB cluster promotes one of the
-- Aurora Replicas (read-only instances) in the DB cluster to be the
-- primary DB instance (the cluster writer).
--
-- For a Multi-AZ DB cluster, failover for a DB cluster promotes one of the
-- readable standby DB instances (read-only instances) in the DB cluster to
-- be the primary DB instance (the cluster writer).
--
-- An Amazon Aurora DB cluster automatically fails over to an Aurora
-- Replica, if one exists, when the primary DB instance fails. A Multi-AZ
-- DB cluster automatically fails over to a readable standby DB instance
-- when the primary DB instance fails.
--
-- To simulate a failure of a primary instance for testing, you can force a
-- failover. Because each instance in a DB cluster has its own endpoint
-- address, make sure to clean up and re-establish any existing connections
-- that use those endpoint addresses when the failover is complete.
--
-- For more information on Amazon Aurora DB clusters, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What is Amazon Aurora?>
-- in the /Amazon Aurora User Guide/.
--
-- For more information on Multi-AZ DB clusters, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/multi-az-db-clusters-concepts.html Multi-AZ deployments with two readable standby DB instances>
-- in the /Amazon RDS User Guide/.
module Amazonka.RDS.FailoverDBCluster
  ( -- * Creating a Request
    FailoverDBCluster (..),
    newFailoverDBCluster,

    -- * Request Lenses
    failoverDBCluster_targetDBInstanceIdentifier,
    failoverDBCluster_dbClusterIdentifier,

    -- * Destructuring the Response
    FailoverDBClusterResponse (..),
    newFailoverDBClusterResponse,

    -- * Response Lenses
    failoverDBClusterResponse_dbCluster,
    failoverDBClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newFailoverDBCluster' smart constructor.
data FailoverDBCluster = FailoverDBCluster'
  { -- | The name of the DB instance to promote to the primary DB instance.
    --
    -- Specify the DB instance identifier for an Aurora Replica or a Multi-AZ
    -- readable standby in the DB cluster, for example @mydbcluster-replica1@.
    --
    -- This setting isn\'t supported for RDS for MySQL Multi-AZ DB clusters.
    targetDBInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A DB cluster identifier to force a failover for. This parameter isn\'t
    -- case-sensitive.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing DBCluster.
    dbClusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailoverDBCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetDBInstanceIdentifier', 'failoverDBCluster_targetDBInstanceIdentifier' - The name of the DB instance to promote to the primary DB instance.
--
-- Specify the DB instance identifier for an Aurora Replica or a Multi-AZ
-- readable standby in the DB cluster, for example @mydbcluster-replica1@.
--
-- This setting isn\'t supported for RDS for MySQL Multi-AZ DB clusters.
--
-- 'dbClusterIdentifier', 'failoverDBCluster_dbClusterIdentifier' - A DB cluster identifier to force a failover for. This parameter isn\'t
-- case-sensitive.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBCluster.
newFailoverDBCluster ::
  -- | 'dbClusterIdentifier'
  Prelude.Text ->
  FailoverDBCluster
newFailoverDBCluster pDBClusterIdentifier_ =
  FailoverDBCluster'
    { targetDBInstanceIdentifier =
        Prelude.Nothing,
      dbClusterIdentifier = pDBClusterIdentifier_
    }

-- | The name of the DB instance to promote to the primary DB instance.
--
-- Specify the DB instance identifier for an Aurora Replica or a Multi-AZ
-- readable standby in the DB cluster, for example @mydbcluster-replica1@.
--
-- This setting isn\'t supported for RDS for MySQL Multi-AZ DB clusters.
failoverDBCluster_targetDBInstanceIdentifier :: Lens.Lens' FailoverDBCluster (Prelude.Maybe Prelude.Text)
failoverDBCluster_targetDBInstanceIdentifier = Lens.lens (\FailoverDBCluster' {targetDBInstanceIdentifier} -> targetDBInstanceIdentifier) (\s@FailoverDBCluster' {} a -> s {targetDBInstanceIdentifier = a} :: FailoverDBCluster)

-- | A DB cluster identifier to force a failover for. This parameter isn\'t
-- case-sensitive.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBCluster.
failoverDBCluster_dbClusterIdentifier :: Lens.Lens' FailoverDBCluster Prelude.Text
failoverDBCluster_dbClusterIdentifier = Lens.lens (\FailoverDBCluster' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@FailoverDBCluster' {} a -> s {dbClusterIdentifier = a} :: FailoverDBCluster)

instance Core.AWSRequest FailoverDBCluster where
  type
    AWSResponse FailoverDBCluster =
      FailoverDBClusterResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "FailoverDBClusterResult"
      ( \s h x ->
          FailoverDBClusterResponse'
            Prelude.<$> (x Data..@? "DBCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable FailoverDBCluster where
  hashWithSalt _salt FailoverDBCluster' {..} =
    _salt
      `Prelude.hashWithSalt` targetDBInstanceIdentifier
      `Prelude.hashWithSalt` dbClusterIdentifier

instance Prelude.NFData FailoverDBCluster where
  rnf FailoverDBCluster' {..} =
    Prelude.rnf targetDBInstanceIdentifier
      `Prelude.seq` Prelude.rnf dbClusterIdentifier

instance Data.ToHeaders FailoverDBCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath FailoverDBCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery FailoverDBCluster where
  toQuery FailoverDBCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("FailoverDBCluster" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "TargetDBInstanceIdentifier"
          Data.=: targetDBInstanceIdentifier,
        "DBClusterIdentifier" Data.=: dbClusterIdentifier
      ]

-- | /See:/ 'newFailoverDBClusterResponse' smart constructor.
data FailoverDBClusterResponse = FailoverDBClusterResponse'
  { dbCluster :: Prelude.Maybe DBCluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailoverDBClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbCluster', 'failoverDBClusterResponse_dbCluster' - Undocumented member.
--
-- 'httpStatus', 'failoverDBClusterResponse_httpStatus' - The response's http status code.
newFailoverDBClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  FailoverDBClusterResponse
newFailoverDBClusterResponse pHttpStatus_ =
  FailoverDBClusterResponse'
    { dbCluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
failoverDBClusterResponse_dbCluster :: Lens.Lens' FailoverDBClusterResponse (Prelude.Maybe DBCluster)
failoverDBClusterResponse_dbCluster = Lens.lens (\FailoverDBClusterResponse' {dbCluster} -> dbCluster) (\s@FailoverDBClusterResponse' {} a -> s {dbCluster = a} :: FailoverDBClusterResponse)

-- | The response's http status code.
failoverDBClusterResponse_httpStatus :: Lens.Lens' FailoverDBClusterResponse Prelude.Int
failoverDBClusterResponse_httpStatus = Lens.lens (\FailoverDBClusterResponse' {httpStatus} -> httpStatus) (\s@FailoverDBClusterResponse' {} a -> s {httpStatus = a} :: FailoverDBClusterResponse)

instance Prelude.NFData FailoverDBClusterResponse where
  rnf FailoverDBClusterResponse' {..} =
    Prelude.rnf dbCluster
      `Prelude.seq` Prelude.rnf httpStatus
