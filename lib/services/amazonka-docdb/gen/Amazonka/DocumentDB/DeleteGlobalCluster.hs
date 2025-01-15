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
-- Module      : Amazonka.DocumentDB.DeleteGlobalCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a global cluster. The primary and secondary clusters must
-- already be detached or deleted before attempting to delete a global
-- cluster.
--
-- This action only applies to Amazon DocumentDB clusters.
module Amazonka.DocumentDB.DeleteGlobalCluster
  ( -- * Creating a Request
    DeleteGlobalCluster (..),
    newDeleteGlobalCluster,

    -- * Request Lenses
    deleteGlobalCluster_globalClusterIdentifier,

    -- * Destructuring the Response
    DeleteGlobalClusterResponse (..),
    newDeleteGlobalClusterResponse,

    -- * Response Lenses
    deleteGlobalClusterResponse_globalCluster,
    deleteGlobalClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DocumentDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input to DeleteGlobalCluster.
--
-- /See:/ 'newDeleteGlobalCluster' smart constructor.
data DeleteGlobalCluster = DeleteGlobalCluster'
  { -- | The cluster identifier of the global cluster being deleted.
    globalClusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGlobalCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalClusterIdentifier', 'deleteGlobalCluster_globalClusterIdentifier' - The cluster identifier of the global cluster being deleted.
newDeleteGlobalCluster ::
  -- | 'globalClusterIdentifier'
  Prelude.Text ->
  DeleteGlobalCluster
newDeleteGlobalCluster pGlobalClusterIdentifier_ =
  DeleteGlobalCluster'
    { globalClusterIdentifier =
        pGlobalClusterIdentifier_
    }

-- | The cluster identifier of the global cluster being deleted.
deleteGlobalCluster_globalClusterIdentifier :: Lens.Lens' DeleteGlobalCluster Prelude.Text
deleteGlobalCluster_globalClusterIdentifier = Lens.lens (\DeleteGlobalCluster' {globalClusterIdentifier} -> globalClusterIdentifier) (\s@DeleteGlobalCluster' {} a -> s {globalClusterIdentifier = a} :: DeleteGlobalCluster)

instance Core.AWSRequest DeleteGlobalCluster where
  type
    AWSResponse DeleteGlobalCluster =
      DeleteGlobalClusterResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteGlobalClusterResult"
      ( \s h x ->
          DeleteGlobalClusterResponse'
            Prelude.<$> (x Data..@? "GlobalCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteGlobalCluster where
  hashWithSalt _salt DeleteGlobalCluster' {..} =
    _salt
      `Prelude.hashWithSalt` globalClusterIdentifier

instance Prelude.NFData DeleteGlobalCluster where
  rnf DeleteGlobalCluster' {..} =
    Prelude.rnf globalClusterIdentifier

instance Data.ToHeaders DeleteGlobalCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteGlobalCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteGlobalCluster where
  toQuery DeleteGlobalCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteGlobalCluster" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "GlobalClusterIdentifier"
          Data.=: globalClusterIdentifier
      ]

-- | /See:/ 'newDeleteGlobalClusterResponse' smart constructor.
data DeleteGlobalClusterResponse = DeleteGlobalClusterResponse'
  { globalCluster :: Prelude.Maybe GlobalCluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGlobalClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalCluster', 'deleteGlobalClusterResponse_globalCluster' - Undocumented member.
--
-- 'httpStatus', 'deleteGlobalClusterResponse_httpStatus' - The response's http status code.
newDeleteGlobalClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteGlobalClusterResponse
newDeleteGlobalClusterResponse pHttpStatus_ =
  DeleteGlobalClusterResponse'
    { globalCluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteGlobalClusterResponse_globalCluster :: Lens.Lens' DeleteGlobalClusterResponse (Prelude.Maybe GlobalCluster)
deleteGlobalClusterResponse_globalCluster = Lens.lens (\DeleteGlobalClusterResponse' {globalCluster} -> globalCluster) (\s@DeleteGlobalClusterResponse' {} a -> s {globalCluster = a} :: DeleteGlobalClusterResponse)

-- | The response's http status code.
deleteGlobalClusterResponse_httpStatus :: Lens.Lens' DeleteGlobalClusterResponse Prelude.Int
deleteGlobalClusterResponse_httpStatus = Lens.lens (\DeleteGlobalClusterResponse' {httpStatus} -> httpStatus) (\s@DeleteGlobalClusterResponse' {} a -> s {httpStatus = a} :: DeleteGlobalClusterResponse)

instance Prelude.NFData DeleteGlobalClusterResponse where
  rnf DeleteGlobalClusterResponse' {..} =
    Prelude.rnf globalCluster `Prelude.seq`
      Prelude.rnf httpStatus
