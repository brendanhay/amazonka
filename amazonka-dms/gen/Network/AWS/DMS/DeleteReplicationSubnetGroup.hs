{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDeleteReplicationSubnetGroup' smart constructor.
data DeleteReplicationSubnetGroup = DeleteReplicationSubnetGroup'
  { -- | The subnet group name of the replication instance.
    replicationSubnetGroupIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteReplicationSubnetGroup
newDeleteReplicationSubnetGroup
  pReplicationSubnetGroupIdentifier_ =
    DeleteReplicationSubnetGroup'
      { replicationSubnetGroupIdentifier =
          pReplicationSubnetGroupIdentifier_
      }

-- | The subnet group name of the replication instance.
deleteReplicationSubnetGroup_replicationSubnetGroupIdentifier :: Lens.Lens' DeleteReplicationSubnetGroup Prelude.Text
deleteReplicationSubnetGroup_replicationSubnetGroupIdentifier = Lens.lens (\DeleteReplicationSubnetGroup' {replicationSubnetGroupIdentifier} -> replicationSubnetGroupIdentifier) (\s@DeleteReplicationSubnetGroup' {} a -> s {replicationSubnetGroupIdentifier = a} :: DeleteReplicationSubnetGroup)

instance
  Prelude.AWSRequest
    DeleteReplicationSubnetGroup
  where
  type
    Rs DeleteReplicationSubnetGroup =
      DeleteReplicationSubnetGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteReplicationSubnetGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteReplicationSubnetGroup

instance Prelude.NFData DeleteReplicationSubnetGroup

instance
  Prelude.ToHeaders
    DeleteReplicationSubnetGroup
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonDMSv20160101.DeleteReplicationSubnetGroup" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteReplicationSubnetGroup where
  toJSON DeleteReplicationSubnetGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ReplicationSubnetGroupIdentifier"
                  Prelude..= replicationSubnetGroupIdentifier
              )
          ]
      )

instance Prelude.ToPath DeleteReplicationSubnetGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteReplicationSubnetGroup where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDeleteReplicationSubnetGroupResponse' smart constructor.
data DeleteReplicationSubnetGroupResponse = DeleteReplicationSubnetGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteReplicationSubnetGroupResponse
newDeleteReplicationSubnetGroupResponse pHttpStatus_ =
  DeleteReplicationSubnetGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteReplicationSubnetGroupResponse_httpStatus :: Lens.Lens' DeleteReplicationSubnetGroupResponse Prelude.Int
deleteReplicationSubnetGroupResponse_httpStatus = Lens.lens (\DeleteReplicationSubnetGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteReplicationSubnetGroupResponse' {} a -> s {httpStatus = a} :: DeleteReplicationSubnetGroupResponse)

instance
  Prelude.NFData
    DeleteReplicationSubnetGroupResponse
