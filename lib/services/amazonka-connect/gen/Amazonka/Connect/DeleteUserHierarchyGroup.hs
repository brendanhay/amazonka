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
-- Module      : Amazonka.Connect.DeleteUserHierarchyGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing user hierarchy group. It must not be associated with
-- any agents or have any active child groups.
module Amazonka.Connect.DeleteUserHierarchyGroup
  ( -- * Creating a Request
    DeleteUserHierarchyGroup (..),
    newDeleteUserHierarchyGroup,

    -- * Request Lenses
    deleteUserHierarchyGroup_hierarchyGroupId,
    deleteUserHierarchyGroup_instanceId,

    -- * Destructuring the Response
    DeleteUserHierarchyGroupResponse (..),
    newDeleteUserHierarchyGroupResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteUserHierarchyGroup' smart constructor.
data DeleteUserHierarchyGroup = DeleteUserHierarchyGroup'
  { -- | The identifier of the hierarchy group.
    hierarchyGroupId :: Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserHierarchyGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hierarchyGroupId', 'deleteUserHierarchyGroup_hierarchyGroupId' - The identifier of the hierarchy group.
--
-- 'instanceId', 'deleteUserHierarchyGroup_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
newDeleteUserHierarchyGroup ::
  -- | 'hierarchyGroupId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  DeleteUserHierarchyGroup
newDeleteUserHierarchyGroup
  pHierarchyGroupId_
  pInstanceId_ =
    DeleteUserHierarchyGroup'
      { hierarchyGroupId =
          pHierarchyGroupId_,
        instanceId = pInstanceId_
      }

-- | The identifier of the hierarchy group.
deleteUserHierarchyGroup_hierarchyGroupId :: Lens.Lens' DeleteUserHierarchyGroup Prelude.Text
deleteUserHierarchyGroup_hierarchyGroupId = Lens.lens (\DeleteUserHierarchyGroup' {hierarchyGroupId} -> hierarchyGroupId) (\s@DeleteUserHierarchyGroup' {} a -> s {hierarchyGroupId = a} :: DeleteUserHierarchyGroup)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
deleteUserHierarchyGroup_instanceId :: Lens.Lens' DeleteUserHierarchyGroup Prelude.Text
deleteUserHierarchyGroup_instanceId = Lens.lens (\DeleteUserHierarchyGroup' {instanceId} -> instanceId) (\s@DeleteUserHierarchyGroup' {} a -> s {instanceId = a} :: DeleteUserHierarchyGroup)

instance Core.AWSRequest DeleteUserHierarchyGroup where
  type
    AWSResponse DeleteUserHierarchyGroup =
      DeleteUserHierarchyGroupResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteUserHierarchyGroupResponse'

instance Prelude.Hashable DeleteUserHierarchyGroup where
  hashWithSalt _salt DeleteUserHierarchyGroup' {..} =
    _salt `Prelude.hashWithSalt` hierarchyGroupId
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData DeleteUserHierarchyGroup where
  rnf DeleteUserHierarchyGroup' {..} =
    Prelude.rnf hierarchyGroupId
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders DeleteUserHierarchyGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteUserHierarchyGroup where
  toPath DeleteUserHierarchyGroup' {..} =
    Prelude.mconcat
      [ "/user-hierarchy-groups/",
        Data.toBS instanceId,
        "/",
        Data.toBS hierarchyGroupId
      ]

instance Data.ToQuery DeleteUserHierarchyGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteUserHierarchyGroupResponse' smart constructor.
data DeleteUserHierarchyGroupResponse = DeleteUserHierarchyGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserHierarchyGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteUserHierarchyGroupResponse ::
  DeleteUserHierarchyGroupResponse
newDeleteUserHierarchyGroupResponse =
  DeleteUserHierarchyGroupResponse'

instance
  Prelude.NFData
    DeleteUserHierarchyGroupResponse
  where
  rnf _ = ()
