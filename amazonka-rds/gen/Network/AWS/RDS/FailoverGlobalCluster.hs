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
-- Module      : Network.AWS.RDS.FailoverGlobalCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates the failover process for an Aurora global database
-- (GlobalCluster).
--
-- A failover for an Aurora global database promotes one of secondary
-- read-only DB clusters to be the primary DB cluster and demotes the
-- primary DB cluster to being a secondary (read-only) DB cluster. In other
-- words, the role of the current primary DB cluster and the selected
-- (target) DB cluster are switched. The selected secondary DB cluster
-- assumes full read\/write capabilities for the Aurora global database.
--
-- For more information about failing over an Amazon Aurora global
-- database, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-global-database-disaster-recovery.managed-failover Managed planned failover for Amazon Aurora global databases>
-- in the /Amazon Aurora User Guide./
--
-- This action applies to GlobalCluster (Aurora global databases) only. Use
-- this action only on healthy Aurora global databases with running Aurora
-- DB clusters and no Region-wide outages, to test disaster recovery
-- scenarios or to reconfigure your Aurora global database topology.
module Network.AWS.RDS.FailoverGlobalCluster
  ( -- * Creating a Request
    FailoverGlobalCluster (..),
    newFailoverGlobalCluster,

    -- * Request Lenses
    failoverGlobalCluster_globalClusterIdentifier,
    failoverGlobalCluster_targetDbClusterIdentifier,

    -- * Destructuring the Response
    FailoverGlobalClusterResponse (..),
    newFailoverGlobalClusterResponse,

    -- * Response Lenses
    failoverGlobalClusterResponse_globalCluster,
    failoverGlobalClusterResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newFailoverGlobalCluster' smart constructor.
data FailoverGlobalCluster = FailoverGlobalCluster'
  { -- | Identifier of the Aurora global database (GlobalCluster) that should be
    -- failed over. The identifier is the unique key assigned by the user when
    -- the Aurora global database was created. In other words, it\'s the name
    -- of the Aurora global database that you want to fail over.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing GlobalCluster (Aurora
    --     global database).
    globalClusterIdentifier :: Prelude.Text,
    -- | Identifier of the secondary Aurora DB cluster that you want to promote
    -- to primary for the Aurora global database (GlobalCluster.) Use the
    -- Amazon Resource Name (ARN) for the identifier so that Aurora can locate
    -- the cluster in its AWS Region.
    targetDbClusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailoverGlobalCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalClusterIdentifier', 'failoverGlobalCluster_globalClusterIdentifier' - Identifier of the Aurora global database (GlobalCluster) that should be
-- failed over. The identifier is the unique key assigned by the user when
-- the Aurora global database was created. In other words, it\'s the name
-- of the Aurora global database that you want to fail over.
--
-- Constraints:
--
-- -   Must match the identifier of an existing GlobalCluster (Aurora
--     global database).
--
-- 'targetDbClusterIdentifier', 'failoverGlobalCluster_targetDbClusterIdentifier' - Identifier of the secondary Aurora DB cluster that you want to promote
-- to primary for the Aurora global database (GlobalCluster.) Use the
-- Amazon Resource Name (ARN) for the identifier so that Aurora can locate
-- the cluster in its AWS Region.
newFailoverGlobalCluster ::
  -- | 'globalClusterIdentifier'
  Prelude.Text ->
  -- | 'targetDbClusterIdentifier'
  Prelude.Text ->
  FailoverGlobalCluster
newFailoverGlobalCluster
  pGlobalClusterIdentifier_
  pTargetDbClusterIdentifier_ =
    FailoverGlobalCluster'
      { globalClusterIdentifier =
          pGlobalClusterIdentifier_,
        targetDbClusterIdentifier =
          pTargetDbClusterIdentifier_
      }

-- | Identifier of the Aurora global database (GlobalCluster) that should be
-- failed over. The identifier is the unique key assigned by the user when
-- the Aurora global database was created. In other words, it\'s the name
-- of the Aurora global database that you want to fail over.
--
-- Constraints:
--
-- -   Must match the identifier of an existing GlobalCluster (Aurora
--     global database).
failoverGlobalCluster_globalClusterIdentifier :: Lens.Lens' FailoverGlobalCluster Prelude.Text
failoverGlobalCluster_globalClusterIdentifier = Lens.lens (\FailoverGlobalCluster' {globalClusterIdentifier} -> globalClusterIdentifier) (\s@FailoverGlobalCluster' {} a -> s {globalClusterIdentifier = a} :: FailoverGlobalCluster)

-- | Identifier of the secondary Aurora DB cluster that you want to promote
-- to primary for the Aurora global database (GlobalCluster.) Use the
-- Amazon Resource Name (ARN) for the identifier so that Aurora can locate
-- the cluster in its AWS Region.
failoverGlobalCluster_targetDbClusterIdentifier :: Lens.Lens' FailoverGlobalCluster Prelude.Text
failoverGlobalCluster_targetDbClusterIdentifier = Lens.lens (\FailoverGlobalCluster' {targetDbClusterIdentifier} -> targetDbClusterIdentifier) (\s@FailoverGlobalCluster' {} a -> s {targetDbClusterIdentifier = a} :: FailoverGlobalCluster)

instance Core.AWSRequest FailoverGlobalCluster where
  type
    AWSResponse FailoverGlobalCluster =
      FailoverGlobalClusterResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "FailoverGlobalClusterResult"
      ( \s h x ->
          FailoverGlobalClusterResponse'
            Prelude.<$> (x Core..@? "GlobalCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable FailoverGlobalCluster

instance Prelude.NFData FailoverGlobalCluster

instance Core.ToHeaders FailoverGlobalCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath FailoverGlobalCluster where
  toPath = Prelude.const "/"

instance Core.ToQuery FailoverGlobalCluster where
  toQuery FailoverGlobalCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("FailoverGlobalCluster" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "GlobalClusterIdentifier"
          Core.=: globalClusterIdentifier,
        "TargetDbClusterIdentifier"
          Core.=: targetDbClusterIdentifier
      ]

-- | /See:/ 'newFailoverGlobalClusterResponse' smart constructor.
data FailoverGlobalClusterResponse = FailoverGlobalClusterResponse'
  { globalCluster :: Prelude.Maybe GlobalCluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailoverGlobalClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalCluster', 'failoverGlobalClusterResponse_globalCluster' - Undocumented member.
--
-- 'httpStatus', 'failoverGlobalClusterResponse_httpStatus' - The response's http status code.
newFailoverGlobalClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  FailoverGlobalClusterResponse
newFailoverGlobalClusterResponse pHttpStatus_ =
  FailoverGlobalClusterResponse'
    { globalCluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
failoverGlobalClusterResponse_globalCluster :: Lens.Lens' FailoverGlobalClusterResponse (Prelude.Maybe GlobalCluster)
failoverGlobalClusterResponse_globalCluster = Lens.lens (\FailoverGlobalClusterResponse' {globalCluster} -> globalCluster) (\s@FailoverGlobalClusterResponse' {} a -> s {globalCluster = a} :: FailoverGlobalClusterResponse)

-- | The response's http status code.
failoverGlobalClusterResponse_httpStatus :: Lens.Lens' FailoverGlobalClusterResponse Prelude.Int
failoverGlobalClusterResponse_httpStatus = Lens.lens (\FailoverGlobalClusterResponse' {httpStatus} -> httpStatus) (\s@FailoverGlobalClusterResponse' {} a -> s {httpStatus = a} :: FailoverGlobalClusterResponse)

instance Prelude.NFData FailoverGlobalClusterResponse
