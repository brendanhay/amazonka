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
-- Module      : Network.AWS.RDS.StopDBCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an Amazon Aurora DB cluster. When you stop a DB cluster, Aurora
-- retains the DB cluster\'s metadata, including its endpoints and DB
-- parameter groups. Aurora also retains the transaction logs so you can do
-- a point-in-time restore if necessary.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-cluster-stop-start.html Stopping and Starting an Aurora Cluster>
-- in the /Amazon Aurora User Guide./
--
-- This action only applies to Aurora DB clusters.
module Network.AWS.RDS.StopDBCluster
  ( -- * Creating a Request
    StopDBCluster (..),
    newStopDBCluster,

    -- * Request Lenses
    stopDBCluster_dbClusterIdentifier,

    -- * Destructuring the Response
    StopDBClusterResponse (..),
    newStopDBClusterResponse,

    -- * Response Lenses
    stopDBClusterResponse_dbCluster,
    stopDBClusterResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopDBCluster' smart constructor.
data StopDBCluster = StopDBCluster'
  { -- | The DB cluster identifier of the Amazon Aurora DB cluster to be stopped.
    -- This parameter is stored as a lowercase string.
    dbClusterIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopDBCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterIdentifier', 'stopDBCluster_dbClusterIdentifier' - The DB cluster identifier of the Amazon Aurora DB cluster to be stopped.
-- This parameter is stored as a lowercase string.
newStopDBCluster ::
  -- | 'dbClusterIdentifier'
  Core.Text ->
  StopDBCluster
newStopDBCluster pDBClusterIdentifier_ =
  StopDBCluster'
    { dbClusterIdentifier =
        pDBClusterIdentifier_
    }

-- | The DB cluster identifier of the Amazon Aurora DB cluster to be stopped.
-- This parameter is stored as a lowercase string.
stopDBCluster_dbClusterIdentifier :: Lens.Lens' StopDBCluster Core.Text
stopDBCluster_dbClusterIdentifier = Lens.lens (\StopDBCluster' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@StopDBCluster' {} a -> s {dbClusterIdentifier = a} :: StopDBCluster)

instance Core.AWSRequest StopDBCluster where
  type
    AWSResponse StopDBCluster =
      StopDBClusterResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "StopDBClusterResult"
      ( \s h x ->
          StopDBClusterResponse'
            Core.<$> (x Core..@? "DBCluster")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopDBCluster

instance Core.NFData StopDBCluster

instance Core.ToHeaders StopDBCluster where
  toHeaders = Core.const Core.mempty

instance Core.ToPath StopDBCluster where
  toPath = Core.const "/"

instance Core.ToQuery StopDBCluster where
  toQuery StopDBCluster' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("StopDBCluster" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "DBClusterIdentifier" Core.=: dbClusterIdentifier
      ]

-- | /See:/ 'newStopDBClusterResponse' smart constructor.
data StopDBClusterResponse = StopDBClusterResponse'
  { dbCluster :: Core.Maybe DBCluster,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopDBClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbCluster', 'stopDBClusterResponse_dbCluster' - Undocumented member.
--
-- 'httpStatus', 'stopDBClusterResponse_httpStatus' - The response's http status code.
newStopDBClusterResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopDBClusterResponse
newStopDBClusterResponse pHttpStatus_ =
  StopDBClusterResponse'
    { dbCluster = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
stopDBClusterResponse_dbCluster :: Lens.Lens' StopDBClusterResponse (Core.Maybe DBCluster)
stopDBClusterResponse_dbCluster = Lens.lens (\StopDBClusterResponse' {dbCluster} -> dbCluster) (\s@StopDBClusterResponse' {} a -> s {dbCluster = a} :: StopDBClusterResponse)

-- | The response's http status code.
stopDBClusterResponse_httpStatus :: Lens.Lens' StopDBClusterResponse Core.Int
stopDBClusterResponse_httpStatus = Lens.lens (\StopDBClusterResponse' {httpStatus} -> httpStatus) (\s@StopDBClusterResponse' {} a -> s {httpStatus = a} :: StopDBClusterResponse)

instance Core.NFData StopDBClusterResponse
