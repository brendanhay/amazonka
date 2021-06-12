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
-- Module      : Network.AWS.MediaLive.DeleteInputSecurityGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Input Security Group
module Network.AWS.MediaLive.DeleteInputSecurityGroup
  ( -- * Creating a Request
    DeleteInputSecurityGroup (..),
    newDeleteInputSecurityGroup,

    -- * Request Lenses
    deleteInputSecurityGroup_inputSecurityGroupId,

    -- * Destructuring the Response
    DeleteInputSecurityGroupResponse (..),
    newDeleteInputSecurityGroupResponse,

    -- * Response Lenses
    deleteInputSecurityGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DeleteInputSecurityGroupRequest
--
-- /See:/ 'newDeleteInputSecurityGroup' smart constructor.
data DeleteInputSecurityGroup = DeleteInputSecurityGroup'
  { -- | The Input Security Group to delete
    inputSecurityGroupId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteInputSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputSecurityGroupId', 'deleteInputSecurityGroup_inputSecurityGroupId' - The Input Security Group to delete
newDeleteInputSecurityGroup ::
  -- | 'inputSecurityGroupId'
  Core.Text ->
  DeleteInputSecurityGroup
newDeleteInputSecurityGroup pInputSecurityGroupId_ =
  DeleteInputSecurityGroup'
    { inputSecurityGroupId =
        pInputSecurityGroupId_
    }

-- | The Input Security Group to delete
deleteInputSecurityGroup_inputSecurityGroupId :: Lens.Lens' DeleteInputSecurityGroup Core.Text
deleteInputSecurityGroup_inputSecurityGroupId = Lens.lens (\DeleteInputSecurityGroup' {inputSecurityGroupId} -> inputSecurityGroupId) (\s@DeleteInputSecurityGroup' {} a -> s {inputSecurityGroupId = a} :: DeleteInputSecurityGroup)

instance Core.AWSRequest DeleteInputSecurityGroup where
  type
    AWSResponse DeleteInputSecurityGroup =
      DeleteInputSecurityGroupResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteInputSecurityGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteInputSecurityGroup

instance Core.NFData DeleteInputSecurityGroup

instance Core.ToHeaders DeleteInputSecurityGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteInputSecurityGroup where
  toPath DeleteInputSecurityGroup' {..} =
    Core.mconcat
      [ "/prod/inputSecurityGroups/",
        Core.toBS inputSecurityGroupId
      ]

instance Core.ToQuery DeleteInputSecurityGroup where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for DeleteInputSecurityGroupResponse
--
-- /See:/ 'newDeleteInputSecurityGroupResponse' smart constructor.
data DeleteInputSecurityGroupResponse = DeleteInputSecurityGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteInputSecurityGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteInputSecurityGroupResponse_httpStatus' - The response's http status code.
newDeleteInputSecurityGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteInputSecurityGroupResponse
newDeleteInputSecurityGroupResponse pHttpStatus_ =
  DeleteInputSecurityGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteInputSecurityGroupResponse_httpStatus :: Lens.Lens' DeleteInputSecurityGroupResponse Core.Int
deleteInputSecurityGroupResponse_httpStatus = Lens.lens (\DeleteInputSecurityGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteInputSecurityGroupResponse' {} a -> s {httpStatus = a} :: DeleteInputSecurityGroupResponse)

instance Core.NFData DeleteInputSecurityGroupResponse
