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
-- Module      : Network.AWS.Shield.DeleteProtectionGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified protection group.
module Network.AWS.Shield.DeleteProtectionGroup
  ( -- * Creating a Request
    DeleteProtectionGroup (..),
    newDeleteProtectionGroup,

    -- * Request Lenses
    deleteProtectionGroup_protectionGroupId,

    -- * Destructuring the Response
    DeleteProtectionGroupResponse (..),
    newDeleteProtectionGroupResponse,

    -- * Response Lenses
    deleteProtectionGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newDeleteProtectionGroup' smart constructor.
data DeleteProtectionGroup = DeleteProtectionGroup'
  { -- | The name of the protection group. You use this to identify the
    -- protection group in lists and to manage the protection group, for
    -- example to update, delete, or describe it.
    protectionGroupId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteProtectionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protectionGroupId', 'deleteProtectionGroup_protectionGroupId' - The name of the protection group. You use this to identify the
-- protection group in lists and to manage the protection group, for
-- example to update, delete, or describe it.
newDeleteProtectionGroup ::
  -- | 'protectionGroupId'
  Core.Text ->
  DeleteProtectionGroup
newDeleteProtectionGroup pProtectionGroupId_ =
  DeleteProtectionGroup'
    { protectionGroupId =
        pProtectionGroupId_
    }

-- | The name of the protection group. You use this to identify the
-- protection group in lists and to manage the protection group, for
-- example to update, delete, or describe it.
deleteProtectionGroup_protectionGroupId :: Lens.Lens' DeleteProtectionGroup Core.Text
deleteProtectionGroup_protectionGroupId = Lens.lens (\DeleteProtectionGroup' {protectionGroupId} -> protectionGroupId) (\s@DeleteProtectionGroup' {} a -> s {protectionGroupId = a} :: DeleteProtectionGroup)

instance Core.AWSRequest DeleteProtectionGroup where
  type
    AWSResponse DeleteProtectionGroup =
      DeleteProtectionGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProtectionGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteProtectionGroup

instance Core.NFData DeleteProtectionGroup

instance Core.ToHeaders DeleteProtectionGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShield_20160616.DeleteProtectionGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteProtectionGroup where
  toJSON DeleteProtectionGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ProtectionGroupId" Core..= protectionGroupId)
          ]
      )

instance Core.ToPath DeleteProtectionGroup where
  toPath = Core.const "/"

instance Core.ToQuery DeleteProtectionGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteProtectionGroupResponse' smart constructor.
data DeleteProtectionGroupResponse = DeleteProtectionGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteProtectionGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteProtectionGroupResponse_httpStatus' - The response's http status code.
newDeleteProtectionGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteProtectionGroupResponse
newDeleteProtectionGroupResponse pHttpStatus_ =
  DeleteProtectionGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteProtectionGroupResponse_httpStatus :: Lens.Lens' DeleteProtectionGroupResponse Core.Int
deleteProtectionGroupResponse_httpStatus = Lens.lens (\DeleteProtectionGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteProtectionGroupResponse' {} a -> s {httpStatus = a} :: DeleteProtectionGroupResponse)

instance Core.NFData DeleteProtectionGroupResponse
