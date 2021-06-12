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
-- Module      : Network.AWS.ELBv2.DeleteTargetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified target group.
--
-- You can delete a target group if it is not referenced by any actions.
-- Deleting a target group also deletes any associated health checks.
-- Deleting a target group does not affect its registered targets. For
-- example, any EC2 instances continue to run until you stop or terminate
-- them.
module Network.AWS.ELBv2.DeleteTargetGroup
  ( -- * Creating a Request
    DeleteTargetGroup (..),
    newDeleteTargetGroup,

    -- * Request Lenses
    deleteTargetGroup_targetGroupArn,

    -- * Destructuring the Response
    DeleteTargetGroupResponse (..),
    newDeleteTargetGroupResponse,

    -- * Response Lenses
    deleteTargetGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteTargetGroup' smart constructor.
data DeleteTargetGroup = DeleteTargetGroup'
  { -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTargetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetGroupArn', 'deleteTargetGroup_targetGroupArn' - The Amazon Resource Name (ARN) of the target group.
newDeleteTargetGroup ::
  -- | 'targetGroupArn'
  Core.Text ->
  DeleteTargetGroup
newDeleteTargetGroup pTargetGroupArn_ =
  DeleteTargetGroup'
    { targetGroupArn =
        pTargetGroupArn_
    }

-- | The Amazon Resource Name (ARN) of the target group.
deleteTargetGroup_targetGroupArn :: Lens.Lens' DeleteTargetGroup Core.Text
deleteTargetGroup_targetGroupArn = Lens.lens (\DeleteTargetGroup' {targetGroupArn} -> targetGroupArn) (\s@DeleteTargetGroup' {} a -> s {targetGroupArn = a} :: DeleteTargetGroup)

instance Core.AWSRequest DeleteTargetGroup where
  type
    AWSResponse DeleteTargetGroup =
      DeleteTargetGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteTargetGroupResult"
      ( \s h x ->
          DeleteTargetGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteTargetGroup

instance Core.NFData DeleteTargetGroup

instance Core.ToHeaders DeleteTargetGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteTargetGroup where
  toPath = Core.const "/"

instance Core.ToQuery DeleteTargetGroup where
  toQuery DeleteTargetGroup' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteTargetGroup" :: Core.ByteString),
        "Version" Core.=: ("2015-12-01" :: Core.ByteString),
        "TargetGroupArn" Core.=: targetGroupArn
      ]

-- | /See:/ 'newDeleteTargetGroupResponse' smart constructor.
data DeleteTargetGroupResponse = DeleteTargetGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTargetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteTargetGroupResponse_httpStatus' - The response's http status code.
newDeleteTargetGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteTargetGroupResponse
newDeleteTargetGroupResponse pHttpStatus_ =
  DeleteTargetGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteTargetGroupResponse_httpStatus :: Lens.Lens' DeleteTargetGroupResponse Core.Int
deleteTargetGroupResponse_httpStatus = Lens.lens (\DeleteTargetGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteTargetGroupResponse' {} a -> s {httpStatus = a} :: DeleteTargetGroupResponse)

instance Core.NFData DeleteTargetGroupResponse
