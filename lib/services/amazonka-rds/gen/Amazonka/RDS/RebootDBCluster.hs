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
-- Module      : Amazonka.RDS.RebootDBCluster
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You might need to reboot your DB cluster, usually for maintenance
-- reasons. For example, if you make certain modifications, or if you
-- change the DB cluster parameter group associated with the DB cluster,
-- reboot the DB cluster for the changes to take effect.
--
-- Rebooting a DB cluster restarts the database engine service. Rebooting a
-- DB cluster results in a momentary outage, during which the DB cluster
-- status is set to rebooting.
--
-- Use this operation only for a non-Aurora Multi-AZ DB cluster.
--
-- For more information on Multi-AZ DB clusters, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/multi-az-db-clusters-concepts.html Multi-AZ deployments with two readable standby DB instances>
-- in the /Amazon RDS User Guide./
module Amazonka.RDS.RebootDBCluster
  ( -- * Creating a Request
    RebootDBCluster (..),
    newRebootDBCluster,

    -- * Request Lenses
    rebootDBCluster_dbClusterIdentifier,

    -- * Destructuring the Response
    RebootDBClusterResponse (..),
    newRebootDBClusterResponse,

    -- * Response Lenses
    rebootDBClusterResponse_dbCluster,
    rebootDBClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRebootDBCluster' smart constructor.
data RebootDBCluster = RebootDBCluster'
  { -- | The DB cluster identifier. This parameter is stored as a lowercase
    -- string.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing DBCluster.
    dbClusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebootDBCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterIdentifier', 'rebootDBCluster_dbClusterIdentifier' - The DB cluster identifier. This parameter is stored as a lowercase
-- string.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBCluster.
newRebootDBCluster ::
  -- | 'dbClusterIdentifier'
  Prelude.Text ->
  RebootDBCluster
newRebootDBCluster pDBClusterIdentifier_ =
  RebootDBCluster'
    { dbClusterIdentifier =
        pDBClusterIdentifier_
    }

-- | The DB cluster identifier. This parameter is stored as a lowercase
-- string.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBCluster.
rebootDBCluster_dbClusterIdentifier :: Lens.Lens' RebootDBCluster Prelude.Text
rebootDBCluster_dbClusterIdentifier = Lens.lens (\RebootDBCluster' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@RebootDBCluster' {} a -> s {dbClusterIdentifier = a} :: RebootDBCluster)

instance Core.AWSRequest RebootDBCluster where
  type
    AWSResponse RebootDBCluster =
      RebootDBClusterResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RebootDBClusterResult"
      ( \s h x ->
          RebootDBClusterResponse'
            Prelude.<$> (x Data..@? "DBCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RebootDBCluster where
  hashWithSalt _salt RebootDBCluster' {..} =
    _salt `Prelude.hashWithSalt` dbClusterIdentifier

instance Prelude.NFData RebootDBCluster where
  rnf RebootDBCluster' {..} =
    Prelude.rnf dbClusterIdentifier

instance Data.ToHeaders RebootDBCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RebootDBCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery RebootDBCluster where
  toQuery RebootDBCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("RebootDBCluster" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBClusterIdentifier" Data.=: dbClusterIdentifier
      ]

-- | /See:/ 'newRebootDBClusterResponse' smart constructor.
data RebootDBClusterResponse = RebootDBClusterResponse'
  { dbCluster :: Prelude.Maybe DBCluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebootDBClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbCluster', 'rebootDBClusterResponse_dbCluster' - Undocumented member.
--
-- 'httpStatus', 'rebootDBClusterResponse_httpStatus' - The response's http status code.
newRebootDBClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RebootDBClusterResponse
newRebootDBClusterResponse pHttpStatus_ =
  RebootDBClusterResponse'
    { dbCluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
rebootDBClusterResponse_dbCluster :: Lens.Lens' RebootDBClusterResponse (Prelude.Maybe DBCluster)
rebootDBClusterResponse_dbCluster = Lens.lens (\RebootDBClusterResponse' {dbCluster} -> dbCluster) (\s@RebootDBClusterResponse' {} a -> s {dbCluster = a} :: RebootDBClusterResponse)

-- | The response's http status code.
rebootDBClusterResponse_httpStatus :: Lens.Lens' RebootDBClusterResponse Prelude.Int
rebootDBClusterResponse_httpStatus = Lens.lens (\RebootDBClusterResponse' {httpStatus} -> httpStatus) (\s@RebootDBClusterResponse' {} a -> s {httpStatus = a} :: RebootDBClusterResponse)

instance Prelude.NFData RebootDBClusterResponse where
  rnf RebootDBClusterResponse' {..} =
    Prelude.rnf dbCluster
      `Prelude.seq` Prelude.rnf httpStatus
