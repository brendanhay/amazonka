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
-- Module      : Amazonka.Neptune.PromoteReadReplicaDBCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Not supported.
module Amazonka.Neptune.PromoteReadReplicaDBCluster
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPromoteReadReplicaDBCluster' smart constructor.
data PromoteReadReplicaDBCluster = PromoteReadReplicaDBCluster'
  { -- | Not supported.
    dbClusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PromoteReadReplicaDBCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterIdentifier', 'promoteReadReplicaDBCluster_dbClusterIdentifier' - Not supported.
newPromoteReadReplicaDBCluster ::
  -- | 'dbClusterIdentifier'
  Prelude.Text ->
  PromoteReadReplicaDBCluster
newPromoteReadReplicaDBCluster pDBClusterIdentifier_ =
  PromoteReadReplicaDBCluster'
    { dbClusterIdentifier =
        pDBClusterIdentifier_
    }

-- | Not supported.
promoteReadReplicaDBCluster_dbClusterIdentifier :: Lens.Lens' PromoteReadReplicaDBCluster Prelude.Text
promoteReadReplicaDBCluster_dbClusterIdentifier = Lens.lens (\PromoteReadReplicaDBCluster' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@PromoteReadReplicaDBCluster' {} a -> s {dbClusterIdentifier = a} :: PromoteReadReplicaDBCluster)

instance Core.AWSRequest PromoteReadReplicaDBCluster where
  type
    AWSResponse PromoteReadReplicaDBCluster =
      PromoteReadReplicaDBClusterResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "PromoteReadReplicaDBClusterResult"
      ( \s h x ->
          PromoteReadReplicaDBClusterResponse'
            Prelude.<$> (x Data..@? "DBCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PromoteReadReplicaDBCluster where
  hashWithSalt _salt PromoteReadReplicaDBCluster' {..} =
    _salt `Prelude.hashWithSalt` dbClusterIdentifier

instance Prelude.NFData PromoteReadReplicaDBCluster where
  rnf PromoteReadReplicaDBCluster' {..} =
    Prelude.rnf dbClusterIdentifier

instance Data.ToHeaders PromoteReadReplicaDBCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath PromoteReadReplicaDBCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery PromoteReadReplicaDBCluster where
  toQuery PromoteReadReplicaDBCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "PromoteReadReplicaDBCluster" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBClusterIdentifier" Data.=: dbClusterIdentifier
      ]

-- | /See:/ 'newPromoteReadReplicaDBClusterResponse' smart constructor.
data PromoteReadReplicaDBClusterResponse = PromoteReadReplicaDBClusterResponse'
  { dbCluster :: Prelude.Maybe DBCluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf PromoteReadReplicaDBClusterResponse' {..} =
    Prelude.rnf dbCluster `Prelude.seq`
      Prelude.rnf httpStatus
