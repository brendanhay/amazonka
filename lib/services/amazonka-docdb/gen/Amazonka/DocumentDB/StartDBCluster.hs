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
-- Module      : Amazonka.DocumentDB.StartDBCluster
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restarts the stopped cluster that is specified by @DBClusterIdentifier@.
-- For more information, see
-- <https://docs.aws.amazon.com/documentdb/latest/developerguide/db-cluster-stop-start.html Stopping and Starting an Amazon DocumentDB Cluster>.
module Amazonka.DocumentDB.StartDBCluster
  ( -- * Creating a Request
    StartDBCluster (..),
    newStartDBCluster,

    -- * Request Lenses
    startDBCluster_dbClusterIdentifier,

    -- * Destructuring the Response
    StartDBClusterResponse (..),
    newStartDBClusterResponse,

    -- * Response Lenses
    startDBClusterResponse_dbCluster,
    startDBClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DocumentDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartDBCluster' smart constructor.
data StartDBCluster = StartDBCluster'
  { -- | The identifier of the cluster to restart. Example:
    -- @docdb-2019-05-28-15-24-52@
    dbClusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDBCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterIdentifier', 'startDBCluster_dbClusterIdentifier' - The identifier of the cluster to restart. Example:
-- @docdb-2019-05-28-15-24-52@
newStartDBCluster ::
  -- | 'dbClusterIdentifier'
  Prelude.Text ->
  StartDBCluster
newStartDBCluster pDBClusterIdentifier_ =
  StartDBCluster'
    { dbClusterIdentifier =
        pDBClusterIdentifier_
    }

-- | The identifier of the cluster to restart. Example:
-- @docdb-2019-05-28-15-24-52@
startDBCluster_dbClusterIdentifier :: Lens.Lens' StartDBCluster Prelude.Text
startDBCluster_dbClusterIdentifier = Lens.lens (\StartDBCluster' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@StartDBCluster' {} a -> s {dbClusterIdentifier = a} :: StartDBCluster)

instance Core.AWSRequest StartDBCluster where
  type
    AWSResponse StartDBCluster =
      StartDBClusterResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "StartDBClusterResult"
      ( \s h x ->
          StartDBClusterResponse'
            Prelude.<$> (x Data..@? "DBCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartDBCluster where
  hashWithSalt _salt StartDBCluster' {..} =
    _salt `Prelude.hashWithSalt` dbClusterIdentifier

instance Prelude.NFData StartDBCluster where
  rnf StartDBCluster' {..} =
    Prelude.rnf dbClusterIdentifier

instance Data.ToHeaders StartDBCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath StartDBCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery StartDBCluster where
  toQuery StartDBCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("StartDBCluster" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBClusterIdentifier" Data.=: dbClusterIdentifier
      ]

-- | /See:/ 'newStartDBClusterResponse' smart constructor.
data StartDBClusterResponse = StartDBClusterResponse'
  { dbCluster :: Prelude.Maybe DBCluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDBClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbCluster', 'startDBClusterResponse_dbCluster' - Undocumented member.
--
-- 'httpStatus', 'startDBClusterResponse_httpStatus' - The response's http status code.
newStartDBClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartDBClusterResponse
newStartDBClusterResponse pHttpStatus_ =
  StartDBClusterResponse'
    { dbCluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
startDBClusterResponse_dbCluster :: Lens.Lens' StartDBClusterResponse (Prelude.Maybe DBCluster)
startDBClusterResponse_dbCluster = Lens.lens (\StartDBClusterResponse' {dbCluster} -> dbCluster) (\s@StartDBClusterResponse' {} a -> s {dbCluster = a} :: StartDBClusterResponse)

-- | The response's http status code.
startDBClusterResponse_httpStatus :: Lens.Lens' StartDBClusterResponse Prelude.Int
startDBClusterResponse_httpStatus = Lens.lens (\StartDBClusterResponse' {httpStatus} -> httpStatus) (\s@StartDBClusterResponse' {} a -> s {httpStatus = a} :: StartDBClusterResponse)

instance Prelude.NFData StartDBClusterResponse where
  rnf StartDBClusterResponse' {..} =
    Prelude.rnf dbCluster
      `Prelude.seq` Prelude.rnf httpStatus
