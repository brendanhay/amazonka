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

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteTargetGroup' smart constructor.
data DeleteTargetGroup = DeleteTargetGroup'
  { -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteTargetGroup
newDeleteTargetGroup pTargetGroupArn_ =
  DeleteTargetGroup'
    { targetGroupArn =
        pTargetGroupArn_
    }

-- | The Amazon Resource Name (ARN) of the target group.
deleteTargetGroup_targetGroupArn :: Lens.Lens' DeleteTargetGroup Prelude.Text
deleteTargetGroup_targetGroupArn = Lens.lens (\DeleteTargetGroup' {targetGroupArn} -> targetGroupArn) (\s@DeleteTargetGroup' {} a -> s {targetGroupArn = a} :: DeleteTargetGroup)

instance Prelude.AWSRequest DeleteTargetGroup where
  type Rs DeleteTargetGroup = DeleteTargetGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteTargetGroupResult"
      ( \s h x ->
          DeleteTargetGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTargetGroup

instance Prelude.NFData DeleteTargetGroup

instance Prelude.ToHeaders DeleteTargetGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteTargetGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteTargetGroup where
  toQuery DeleteTargetGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteTargetGroup" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2015-12-01" :: Prelude.ByteString),
        "TargetGroupArn" Prelude.=: targetGroupArn
      ]

-- | /See:/ 'newDeleteTargetGroupResponse' smart constructor.
data DeleteTargetGroupResponse = DeleteTargetGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteTargetGroupResponse
newDeleteTargetGroupResponse pHttpStatus_ =
  DeleteTargetGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteTargetGroupResponse_httpStatus :: Lens.Lens' DeleteTargetGroupResponse Prelude.Int
deleteTargetGroupResponse_httpStatus = Lens.lens (\DeleteTargetGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteTargetGroupResponse' {} a -> s {httpStatus = a} :: DeleteTargetGroupResponse)

instance Prelude.NFData DeleteTargetGroupResponse
