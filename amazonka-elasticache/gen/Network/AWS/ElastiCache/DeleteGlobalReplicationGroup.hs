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
-- Module      : Network.AWS.ElastiCache.DeleteGlobalReplicationGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deleting a Global Datastore is a two-step process:
--
-- -   First, you must DisassociateGlobalReplicationGroup to remove the
--     secondary clusters in the Global Datastore.
--
-- -   Once the Global Datastore contains only the primary cluster, you can
--     use DeleteGlobalReplicationGroup API to delete the Global Datastore
--     while retainining the primary cluster using Retain…= true.
--
-- Since the Global Datastore has only a primary cluster, you can delete
-- the Global Datastore while retaining the primary by setting
-- @RetainPrimaryCluster=true@.
--
-- When you receive a successful response from this operation, Amazon
-- ElastiCache immediately begins deleting the selected resources; you
-- cannot cancel or revert this operation.
module Network.AWS.ElastiCache.DeleteGlobalReplicationGroup
  ( -- * Creating a Request
    DeleteGlobalReplicationGroup (..),
    newDeleteGlobalReplicationGroup,

    -- * Request Lenses
    deleteGlobalReplicationGroup_globalReplicationGroupId,
    deleteGlobalReplicationGroup_retainPrimaryReplicationGroup,

    -- * Destructuring the Response
    DeleteGlobalReplicationGroupResponse (..),
    newDeleteGlobalReplicationGroupResponse,

    -- * Response Lenses
    deleteGlobalReplicationGroupResponse_globalReplicationGroup,
    deleteGlobalReplicationGroupResponse_httpStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteGlobalReplicationGroup' smart constructor.
data DeleteGlobalReplicationGroup = DeleteGlobalReplicationGroup'
  { -- | The name of the Global Datastore
    globalReplicationGroupId :: Prelude.Text,
    -- | The primary replication group is retained as a standalone replication
    -- group.
    retainPrimaryReplicationGroup :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteGlobalReplicationGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalReplicationGroupId', 'deleteGlobalReplicationGroup_globalReplicationGroupId' - The name of the Global Datastore
--
-- 'retainPrimaryReplicationGroup', 'deleteGlobalReplicationGroup_retainPrimaryReplicationGroup' - The primary replication group is retained as a standalone replication
-- group.
newDeleteGlobalReplicationGroup ::
  -- | 'globalReplicationGroupId'
  Prelude.Text ->
  -- | 'retainPrimaryReplicationGroup'
  Prelude.Bool ->
  DeleteGlobalReplicationGroup
newDeleteGlobalReplicationGroup
  pGlobalReplicationGroupId_
  pRetainPrimaryReplicationGroup_ =
    DeleteGlobalReplicationGroup'
      { globalReplicationGroupId =
          pGlobalReplicationGroupId_,
        retainPrimaryReplicationGroup =
          pRetainPrimaryReplicationGroup_
      }

-- | The name of the Global Datastore
deleteGlobalReplicationGroup_globalReplicationGroupId :: Lens.Lens' DeleteGlobalReplicationGroup Prelude.Text
deleteGlobalReplicationGroup_globalReplicationGroupId = Lens.lens (\DeleteGlobalReplicationGroup' {globalReplicationGroupId} -> globalReplicationGroupId) (\s@DeleteGlobalReplicationGroup' {} a -> s {globalReplicationGroupId = a} :: DeleteGlobalReplicationGroup)

-- | The primary replication group is retained as a standalone replication
-- group.
deleteGlobalReplicationGroup_retainPrimaryReplicationGroup :: Lens.Lens' DeleteGlobalReplicationGroup Prelude.Bool
deleteGlobalReplicationGroup_retainPrimaryReplicationGroup = Lens.lens (\DeleteGlobalReplicationGroup' {retainPrimaryReplicationGroup} -> retainPrimaryReplicationGroup) (\s@DeleteGlobalReplicationGroup' {} a -> s {retainPrimaryReplicationGroup = a} :: DeleteGlobalReplicationGroup)

instance
  Prelude.AWSRequest
    DeleteGlobalReplicationGroup
  where
  type
    Rs DeleteGlobalReplicationGroup =
      DeleteGlobalReplicationGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteGlobalReplicationGroupResult"
      ( \s h x ->
          DeleteGlobalReplicationGroupResponse'
            Prelude.<$> (x Prelude..@? "GlobalReplicationGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteGlobalReplicationGroup

instance Prelude.NFData DeleteGlobalReplicationGroup

instance
  Prelude.ToHeaders
    DeleteGlobalReplicationGroup
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteGlobalReplicationGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteGlobalReplicationGroup where
  toQuery DeleteGlobalReplicationGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DeleteGlobalReplicationGroup" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2015-02-02" :: Prelude.ByteString),
        "GlobalReplicationGroupId"
          Prelude.=: globalReplicationGroupId,
        "RetainPrimaryReplicationGroup"
          Prelude.=: retainPrimaryReplicationGroup
      ]

-- | /See:/ 'newDeleteGlobalReplicationGroupResponse' smart constructor.
data DeleteGlobalReplicationGroupResponse = DeleteGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Prelude.Maybe GlobalReplicationGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteGlobalReplicationGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalReplicationGroup', 'deleteGlobalReplicationGroupResponse_globalReplicationGroup' - Undocumented member.
--
-- 'httpStatus', 'deleteGlobalReplicationGroupResponse_httpStatus' - The response's http status code.
newDeleteGlobalReplicationGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteGlobalReplicationGroupResponse
newDeleteGlobalReplicationGroupResponse pHttpStatus_ =
  DeleteGlobalReplicationGroupResponse'
    { globalReplicationGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteGlobalReplicationGroupResponse_globalReplicationGroup :: Lens.Lens' DeleteGlobalReplicationGroupResponse (Prelude.Maybe GlobalReplicationGroup)
deleteGlobalReplicationGroupResponse_globalReplicationGroup = Lens.lens (\DeleteGlobalReplicationGroupResponse' {globalReplicationGroup} -> globalReplicationGroup) (\s@DeleteGlobalReplicationGroupResponse' {} a -> s {globalReplicationGroup = a} :: DeleteGlobalReplicationGroupResponse)

-- | The response's http status code.
deleteGlobalReplicationGroupResponse_httpStatus :: Lens.Lens' DeleteGlobalReplicationGroupResponse Prelude.Int
deleteGlobalReplicationGroupResponse_httpStatus = Lens.lens (\DeleteGlobalReplicationGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteGlobalReplicationGroupResponse' {} a -> s {httpStatus = a} :: DeleteGlobalReplicationGroupResponse)

instance
  Prelude.NFData
    DeleteGlobalReplicationGroupResponse
