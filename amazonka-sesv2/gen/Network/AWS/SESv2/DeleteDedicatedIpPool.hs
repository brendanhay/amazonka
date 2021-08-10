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
-- Module      : Network.AWS.SESv2.DeleteDedicatedIpPool
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a dedicated IP pool.
module Network.AWS.SESv2.DeleteDedicatedIpPool
  ( -- * Creating a Request
    DeleteDedicatedIpPool (..),
    newDeleteDedicatedIpPool,

    -- * Request Lenses
    deleteDedicatedIpPool_poolName,

    -- * Destructuring the Response
    DeleteDedicatedIpPoolResponse (..),
    newDeleteDedicatedIpPoolResponse,

    -- * Response Lenses
    deleteDedicatedIpPoolResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | A request to delete a dedicated IP pool.
--
-- /See:/ 'newDeleteDedicatedIpPool' smart constructor.
data DeleteDedicatedIpPool = DeleteDedicatedIpPool'
  { -- | The name of the dedicated IP pool that you want to delete.
    poolName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDedicatedIpPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'poolName', 'deleteDedicatedIpPool_poolName' - The name of the dedicated IP pool that you want to delete.
newDeleteDedicatedIpPool ::
  -- | 'poolName'
  Prelude.Text ->
  DeleteDedicatedIpPool
newDeleteDedicatedIpPool pPoolName_ =
  DeleteDedicatedIpPool' {poolName = pPoolName_}

-- | The name of the dedicated IP pool that you want to delete.
deleteDedicatedIpPool_poolName :: Lens.Lens' DeleteDedicatedIpPool Prelude.Text
deleteDedicatedIpPool_poolName = Lens.lens (\DeleteDedicatedIpPool' {poolName} -> poolName) (\s@DeleteDedicatedIpPool' {} a -> s {poolName = a} :: DeleteDedicatedIpPool)

instance Core.AWSRequest DeleteDedicatedIpPool where
  type
    AWSResponse DeleteDedicatedIpPool =
      DeleteDedicatedIpPoolResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDedicatedIpPoolResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDedicatedIpPool

instance Prelude.NFData DeleteDedicatedIpPool

instance Core.ToHeaders DeleteDedicatedIpPool where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteDedicatedIpPool where
  toPath DeleteDedicatedIpPool' {..} =
    Prelude.mconcat
      ["/v2/email/dedicated-ip-pools/", Core.toBS poolName]

instance Core.ToQuery DeleteDedicatedIpPool where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newDeleteDedicatedIpPoolResponse' smart constructor.
data DeleteDedicatedIpPoolResponse = DeleteDedicatedIpPoolResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDedicatedIpPoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDedicatedIpPoolResponse_httpStatus' - The response's http status code.
newDeleteDedicatedIpPoolResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDedicatedIpPoolResponse
newDeleteDedicatedIpPoolResponse pHttpStatus_ =
  DeleteDedicatedIpPoolResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteDedicatedIpPoolResponse_httpStatus :: Lens.Lens' DeleteDedicatedIpPoolResponse Prelude.Int
deleteDedicatedIpPoolResponse_httpStatus = Lens.lens (\DeleteDedicatedIpPoolResponse' {httpStatus} -> httpStatus) (\s@DeleteDedicatedIpPoolResponse' {} a -> s {httpStatus = a} :: DeleteDedicatedIpPoolResponse)

instance Prelude.NFData DeleteDedicatedIpPoolResponse
