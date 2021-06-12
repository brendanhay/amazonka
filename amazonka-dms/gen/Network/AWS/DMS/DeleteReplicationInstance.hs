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
-- Module      : Network.AWS.DMS.DeleteReplicationInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified replication instance.
--
-- You must delete any migration tasks that are associated with the
-- replication instance before you can delete it.
module Network.AWS.DMS.DeleteReplicationInstance
  ( -- * Creating a Request
    DeleteReplicationInstance (..),
    newDeleteReplicationInstance,

    -- * Request Lenses
    deleteReplicationInstance_replicationInstanceArn,

    -- * Destructuring the Response
    DeleteReplicationInstanceResponse (..),
    newDeleteReplicationInstanceResponse,

    -- * Response Lenses
    deleteReplicationInstanceResponse_replicationInstance,
    deleteReplicationInstanceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDeleteReplicationInstance' smart constructor.
data DeleteReplicationInstance = DeleteReplicationInstance'
  { -- | The Amazon Resource Name (ARN) of the replication instance to be
    -- deleted.
    replicationInstanceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteReplicationInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationInstanceArn', 'deleteReplicationInstance_replicationInstanceArn' - The Amazon Resource Name (ARN) of the replication instance to be
-- deleted.
newDeleteReplicationInstance ::
  -- | 'replicationInstanceArn'
  Core.Text ->
  DeleteReplicationInstance
newDeleteReplicationInstance pReplicationInstanceArn_ =
  DeleteReplicationInstance'
    { replicationInstanceArn =
        pReplicationInstanceArn_
    }

-- | The Amazon Resource Name (ARN) of the replication instance to be
-- deleted.
deleteReplicationInstance_replicationInstanceArn :: Lens.Lens' DeleteReplicationInstance Core.Text
deleteReplicationInstance_replicationInstanceArn = Lens.lens (\DeleteReplicationInstance' {replicationInstanceArn} -> replicationInstanceArn) (\s@DeleteReplicationInstance' {} a -> s {replicationInstanceArn = a} :: DeleteReplicationInstance)

instance Core.AWSRequest DeleteReplicationInstance where
  type
    AWSResponse DeleteReplicationInstance =
      DeleteReplicationInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteReplicationInstanceResponse'
            Core.<$> (x Core..?> "ReplicationInstance")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteReplicationInstance

instance Core.NFData DeleteReplicationInstance

instance Core.ToHeaders DeleteReplicationInstance where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DeleteReplicationInstance" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteReplicationInstance where
  toJSON DeleteReplicationInstance' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "ReplicationInstanceArn"
                  Core..= replicationInstanceArn
              )
          ]
      )

instance Core.ToPath DeleteReplicationInstance where
  toPath = Core.const "/"

instance Core.ToQuery DeleteReplicationInstance where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newDeleteReplicationInstanceResponse' smart constructor.
data DeleteReplicationInstanceResponse = DeleteReplicationInstanceResponse'
  { -- | The replication instance that was deleted.
    replicationInstance :: Core.Maybe ReplicationInstance,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteReplicationInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationInstance', 'deleteReplicationInstanceResponse_replicationInstance' - The replication instance that was deleted.
--
-- 'httpStatus', 'deleteReplicationInstanceResponse_httpStatus' - The response's http status code.
newDeleteReplicationInstanceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteReplicationInstanceResponse
newDeleteReplicationInstanceResponse pHttpStatus_ =
  DeleteReplicationInstanceResponse'
    { replicationInstance =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The replication instance that was deleted.
deleteReplicationInstanceResponse_replicationInstance :: Lens.Lens' DeleteReplicationInstanceResponse (Core.Maybe ReplicationInstance)
deleteReplicationInstanceResponse_replicationInstance = Lens.lens (\DeleteReplicationInstanceResponse' {replicationInstance} -> replicationInstance) (\s@DeleteReplicationInstanceResponse' {} a -> s {replicationInstance = a} :: DeleteReplicationInstanceResponse)

-- | The response's http status code.
deleteReplicationInstanceResponse_httpStatus :: Lens.Lens' DeleteReplicationInstanceResponse Core.Int
deleteReplicationInstanceResponse_httpStatus = Lens.lens (\DeleteReplicationInstanceResponse' {httpStatus} -> httpStatus) (\s@DeleteReplicationInstanceResponse' {} a -> s {httpStatus = a} :: DeleteReplicationInstanceResponse)

instance
  Core.NFData
    DeleteReplicationInstanceResponse
