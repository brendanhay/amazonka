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
-- Module      : Amazonka.WorkMail.DeleteGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a group from WorkMail.
module Amazonka.WorkMail.DeleteGroup
  ( -- * Creating a Request
    DeleteGroup (..),
    newDeleteGroup,

    -- * Request Lenses
    deleteGroup_organizationId,
    deleteGroup_groupId,

    -- * Destructuring the Response
    DeleteGroupResponse (..),
    newDeleteGroupResponse,

    -- * Response Lenses
    deleteGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newDeleteGroup' smart constructor.
data DeleteGroup = DeleteGroup'
  { -- | The organization that contains the group.
    organizationId :: Prelude.Text,
    -- | The identifier of the group to be deleted.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'deleteGroup_organizationId' - The organization that contains the group.
--
-- 'groupId', 'deleteGroup_groupId' - The identifier of the group to be deleted.
newDeleteGroup ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'groupId'
  Prelude.Text ->
  DeleteGroup
newDeleteGroup pOrganizationId_ pGroupId_ =
  DeleteGroup'
    { organizationId = pOrganizationId_,
      groupId = pGroupId_
    }

-- | The organization that contains the group.
deleteGroup_organizationId :: Lens.Lens' DeleteGroup Prelude.Text
deleteGroup_organizationId = Lens.lens (\DeleteGroup' {organizationId} -> organizationId) (\s@DeleteGroup' {} a -> s {organizationId = a} :: DeleteGroup)

-- | The identifier of the group to be deleted.
deleteGroup_groupId :: Lens.Lens' DeleteGroup Prelude.Text
deleteGroup_groupId = Lens.lens (\DeleteGroup' {groupId} -> groupId) (\s@DeleteGroup' {} a -> s {groupId = a} :: DeleteGroup)

instance Core.AWSRequest DeleteGroup where
  type AWSResponse DeleteGroup = DeleteGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteGroup where
  hashWithSalt _salt DeleteGroup' {..} =
    _salt
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` groupId

instance Prelude.NFData DeleteGroup where
  rnf DeleteGroup' {..} =
    Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf groupId

instance Data.ToHeaders DeleteGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.DeleteGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteGroup where
  toJSON DeleteGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("GroupId" Data..= groupId)
          ]
      )

instance Data.ToPath DeleteGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteGroupResponse' smart constructor.
data DeleteGroupResponse = DeleteGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteGroupResponse_httpStatus' - The response's http status code.
newDeleteGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteGroupResponse
newDeleteGroupResponse pHttpStatus_ =
  DeleteGroupResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteGroupResponse_httpStatus :: Lens.Lens' DeleteGroupResponse Prelude.Int
deleteGroupResponse_httpStatus = Lens.lens (\DeleteGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteGroupResponse' {} a -> s {httpStatus = a} :: DeleteGroupResponse)

instance Prelude.NFData DeleteGroupResponse where
  rnf DeleteGroupResponse' {..} = Prelude.rnf httpStatus
