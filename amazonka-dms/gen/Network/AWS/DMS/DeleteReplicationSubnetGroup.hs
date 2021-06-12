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
-- Module      : Network.AWS.DMS.DeleteReplicationSubnetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a subnet group.
module Network.AWS.DMS.DeleteReplicationSubnetGroup
  ( -- * Creating a Request
    DeleteReplicationSubnetGroup (..),
    newDeleteReplicationSubnetGroup,

    -- * Request Lenses
    deleteReplicationSubnetGroup_replicationSubnetGroupIdentifier,

    -- * Destructuring the Response
    DeleteReplicationSubnetGroupResponse (..),
    newDeleteReplicationSubnetGroupResponse,

    -- * Response Lenses
    deleteReplicationSubnetGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDeleteReplicationSubnetGroup' smart constructor.
data DeleteReplicationSubnetGroup = DeleteReplicationSubnetGroup'
  { -- | The subnet group name of the replication instance.
    replicationSubnetGroupIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteReplicationSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationSubnetGroupIdentifier', 'deleteReplicationSubnetGroup_replicationSubnetGroupIdentifier' - The subnet group name of the replication instance.
newDeleteReplicationSubnetGroup ::
  -- | 'replicationSubnetGroupIdentifier'
  Core.Text ->
  DeleteReplicationSubnetGroup
newDeleteReplicationSubnetGroup
  pReplicationSubnetGroupIdentifier_ =
    DeleteReplicationSubnetGroup'
      { replicationSubnetGroupIdentifier =
          pReplicationSubnetGroupIdentifier_
      }

-- | The subnet group name of the replication instance.
deleteReplicationSubnetGroup_replicationSubnetGroupIdentifier :: Lens.Lens' DeleteReplicationSubnetGroup Core.Text
deleteReplicationSubnetGroup_replicationSubnetGroupIdentifier = Lens.lens (\DeleteReplicationSubnetGroup' {replicationSubnetGroupIdentifier} -> replicationSubnetGroupIdentifier) (\s@DeleteReplicationSubnetGroup' {} a -> s {replicationSubnetGroupIdentifier = a} :: DeleteReplicationSubnetGroup)

instance Core.AWSRequest DeleteReplicationSubnetGroup where
  type
    AWSResponse DeleteReplicationSubnetGroup =
      DeleteReplicationSubnetGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteReplicationSubnetGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteReplicationSubnetGroup

instance Core.NFData DeleteReplicationSubnetGroup

instance Core.ToHeaders DeleteReplicationSubnetGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DeleteReplicationSubnetGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteReplicationSubnetGroup where
  toJSON DeleteReplicationSubnetGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "ReplicationSubnetGroupIdentifier"
                  Core..= replicationSubnetGroupIdentifier
              )
          ]
      )

instance Core.ToPath DeleteReplicationSubnetGroup where
  toPath = Core.const "/"

instance Core.ToQuery DeleteReplicationSubnetGroup where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newDeleteReplicationSubnetGroupResponse' smart constructor.
data DeleteReplicationSubnetGroupResponse = DeleteReplicationSubnetGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteReplicationSubnetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteReplicationSubnetGroupResponse_httpStatus' - The response's http status code.
newDeleteReplicationSubnetGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteReplicationSubnetGroupResponse
newDeleteReplicationSubnetGroupResponse pHttpStatus_ =
  DeleteReplicationSubnetGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteReplicationSubnetGroupResponse_httpStatus :: Lens.Lens' DeleteReplicationSubnetGroupResponse Core.Int
deleteReplicationSubnetGroupResponse_httpStatus = Lens.lens (\DeleteReplicationSubnetGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteReplicationSubnetGroupResponse' {} a -> s {httpStatus = a} :: DeleteReplicationSubnetGroupResponse)

instance
  Core.NFData
    DeleteReplicationSubnetGroupResponse
