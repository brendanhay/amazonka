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
-- Module      : Amazonka.EC2.DeleteCoipPool
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a pool of customer-owned IP (CoIP) addresses.
module Amazonka.EC2.DeleteCoipPool
  ( -- * Creating a Request
    DeleteCoipPool (..),
    newDeleteCoipPool,

    -- * Request Lenses
    deleteCoipPool_dryRun,
    deleteCoipPool_coipPoolId,

    -- * Destructuring the Response
    DeleteCoipPoolResponse (..),
    newDeleteCoipPoolResponse,

    -- * Response Lenses
    deleteCoipPoolResponse_coipPool,
    deleteCoipPoolResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCoipPool' smart constructor.
data DeleteCoipPool = DeleteCoipPool'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the CoIP pool that you want to delete.
    coipPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCoipPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteCoipPool_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'coipPoolId', 'deleteCoipPool_coipPoolId' - The ID of the CoIP pool that you want to delete.
newDeleteCoipPool ::
  -- | 'coipPoolId'
  Prelude.Text ->
  DeleteCoipPool
newDeleteCoipPool pCoipPoolId_ =
  DeleteCoipPool'
    { dryRun = Prelude.Nothing,
      coipPoolId = pCoipPoolId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteCoipPool_dryRun :: Lens.Lens' DeleteCoipPool (Prelude.Maybe Prelude.Bool)
deleteCoipPool_dryRun = Lens.lens (\DeleteCoipPool' {dryRun} -> dryRun) (\s@DeleteCoipPool' {} a -> s {dryRun = a} :: DeleteCoipPool)

-- | The ID of the CoIP pool that you want to delete.
deleteCoipPool_coipPoolId :: Lens.Lens' DeleteCoipPool Prelude.Text
deleteCoipPool_coipPoolId = Lens.lens (\DeleteCoipPool' {coipPoolId} -> coipPoolId) (\s@DeleteCoipPool' {} a -> s {coipPoolId = a} :: DeleteCoipPool)

instance Core.AWSRequest DeleteCoipPool where
  type
    AWSResponse DeleteCoipPool =
      DeleteCoipPoolResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteCoipPoolResponse'
            Prelude.<$> (x Data..@? "coipPool")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCoipPool where
  hashWithSalt _salt DeleteCoipPool' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` coipPoolId

instance Prelude.NFData DeleteCoipPool where
  rnf DeleteCoipPool' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf coipPoolId

instance Data.ToHeaders DeleteCoipPool where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteCoipPool where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteCoipPool where
  toQuery DeleteCoipPool' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteCoipPool" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "CoipPoolId" Data.=: coipPoolId
      ]

-- | /See:/ 'newDeleteCoipPoolResponse' smart constructor.
data DeleteCoipPoolResponse = DeleteCoipPoolResponse'
  { -- | Information about the CoIP address pool.
    coipPool :: Prelude.Maybe CoipPool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCoipPoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coipPool', 'deleteCoipPoolResponse_coipPool' - Information about the CoIP address pool.
--
-- 'httpStatus', 'deleteCoipPoolResponse_httpStatus' - The response's http status code.
newDeleteCoipPoolResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCoipPoolResponse
newDeleteCoipPoolResponse pHttpStatus_ =
  DeleteCoipPoolResponse'
    { coipPool = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the CoIP address pool.
deleteCoipPoolResponse_coipPool :: Lens.Lens' DeleteCoipPoolResponse (Prelude.Maybe CoipPool)
deleteCoipPoolResponse_coipPool = Lens.lens (\DeleteCoipPoolResponse' {coipPool} -> coipPool) (\s@DeleteCoipPoolResponse' {} a -> s {coipPool = a} :: DeleteCoipPoolResponse)

-- | The response's http status code.
deleteCoipPoolResponse_httpStatus :: Lens.Lens' DeleteCoipPoolResponse Prelude.Int
deleteCoipPoolResponse_httpStatus = Lens.lens (\DeleteCoipPoolResponse' {httpStatus} -> httpStatus) (\s@DeleteCoipPoolResponse' {} a -> s {httpStatus = a} :: DeleteCoipPoolResponse)

instance Prelude.NFData DeleteCoipPoolResponse where
  rnf DeleteCoipPoolResponse' {..} =
    Prelude.rnf coipPool `Prelude.seq`
      Prelude.rnf httpStatus
