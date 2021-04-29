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
-- Module      : Network.AWS.WorkSpaces.DeleteIpGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified IP access control group.
--
-- You cannot delete an IP access control group that is associated with a
-- directory.
module Network.AWS.WorkSpaces.DeleteIpGroup
  ( -- * Creating a Request
    DeleteIpGroup (..),
    newDeleteIpGroup,

    -- * Request Lenses
    deleteIpGroup_groupId,

    -- * Destructuring the Response
    DeleteIpGroupResponse (..),
    newDeleteIpGroupResponse,

    -- * Response Lenses
    deleteIpGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newDeleteIpGroup' smart constructor.
data DeleteIpGroup = DeleteIpGroup'
  { -- | The identifier of the IP access control group.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteIpGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'deleteIpGroup_groupId' - The identifier of the IP access control group.
newDeleteIpGroup ::
  -- | 'groupId'
  Prelude.Text ->
  DeleteIpGroup
newDeleteIpGroup pGroupId_ =
  DeleteIpGroup' {groupId = pGroupId_}

-- | The identifier of the IP access control group.
deleteIpGroup_groupId :: Lens.Lens' DeleteIpGroup Prelude.Text
deleteIpGroup_groupId = Lens.lens (\DeleteIpGroup' {groupId} -> groupId) (\s@DeleteIpGroup' {} a -> s {groupId = a} :: DeleteIpGroup)

instance Prelude.AWSRequest DeleteIpGroup where
  type Rs DeleteIpGroup = DeleteIpGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteIpGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteIpGroup

instance Prelude.NFData DeleteIpGroup

instance Prelude.ToHeaders DeleteIpGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkspacesService.DeleteIpGroup" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteIpGroup where
  toJSON DeleteIpGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("GroupId" Prelude..= groupId)]
      )

instance Prelude.ToPath DeleteIpGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteIpGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteIpGroupResponse' smart constructor.
data DeleteIpGroupResponse = DeleteIpGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteIpGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteIpGroupResponse_httpStatus' - The response's http status code.
newDeleteIpGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteIpGroupResponse
newDeleteIpGroupResponse pHttpStatus_ =
  DeleteIpGroupResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteIpGroupResponse_httpStatus :: Lens.Lens' DeleteIpGroupResponse Prelude.Int
deleteIpGroupResponse_httpStatus = Lens.lens (\DeleteIpGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteIpGroupResponse' {} a -> s {httpStatus = a} :: DeleteIpGroupResponse)

instance Prelude.NFData DeleteIpGroupResponse
