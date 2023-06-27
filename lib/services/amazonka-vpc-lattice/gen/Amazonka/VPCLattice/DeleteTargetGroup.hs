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
-- Module      : Amazonka.VPCLattice.DeleteTargetGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a target group. You can\'t delete a target group if it is used
-- in a listener rule or if the target group creation is in progress.
module Amazonka.VPCLattice.DeleteTargetGroup
  ( -- * Creating a Request
    DeleteTargetGroup (..),
    newDeleteTargetGroup,

    -- * Request Lenses
    deleteTargetGroup_targetGroupIdentifier,

    -- * Destructuring the Response
    DeleteTargetGroupResponse (..),
    newDeleteTargetGroupResponse,

    -- * Response Lenses
    deleteTargetGroupResponse_arn,
    deleteTargetGroupResponse_id,
    deleteTargetGroupResponse_status,
    deleteTargetGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newDeleteTargetGroup' smart constructor.
data DeleteTargetGroup = DeleteTargetGroup'
  { -- | The ID or Amazon Resource Name (ARN) of the target group.
    targetGroupIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTargetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetGroupIdentifier', 'deleteTargetGroup_targetGroupIdentifier' - The ID or Amazon Resource Name (ARN) of the target group.
newDeleteTargetGroup ::
  -- | 'targetGroupIdentifier'
  Prelude.Text ->
  DeleteTargetGroup
newDeleteTargetGroup pTargetGroupIdentifier_ =
  DeleteTargetGroup'
    { targetGroupIdentifier =
        pTargetGroupIdentifier_
    }

-- | The ID or Amazon Resource Name (ARN) of the target group.
deleteTargetGroup_targetGroupIdentifier :: Lens.Lens' DeleteTargetGroup Prelude.Text
deleteTargetGroup_targetGroupIdentifier = Lens.lens (\DeleteTargetGroup' {targetGroupIdentifier} -> targetGroupIdentifier) (\s@DeleteTargetGroup' {} a -> s {targetGroupIdentifier = a} :: DeleteTargetGroup)

instance Core.AWSRequest DeleteTargetGroup where
  type
    AWSResponse DeleteTargetGroup =
      DeleteTargetGroupResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTargetGroupResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTargetGroup where
  hashWithSalt _salt DeleteTargetGroup' {..} =
    _salt `Prelude.hashWithSalt` targetGroupIdentifier

instance Prelude.NFData DeleteTargetGroup where
  rnf DeleteTargetGroup' {..} =
    Prelude.rnf targetGroupIdentifier

instance Data.ToHeaders DeleteTargetGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteTargetGroup where
  toPath DeleteTargetGroup' {..} =
    Prelude.mconcat
      ["/targetgroups/", Data.toBS targetGroupIdentifier]

instance Data.ToQuery DeleteTargetGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTargetGroupResponse' smart constructor.
data DeleteTargetGroupResponse = DeleteTargetGroupResponse'
  { -- | The Amazon Resource Name (ARN) of the target group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the target group.
    id :: Prelude.Maybe Prelude.Text,
    -- | The status. You can retry the operation if the status is
    -- @DELETE_FAILED@. However, if you retry it while the status is
    -- @DELETE_IN_PROGRESS@, the status doesn\'t change.
    status :: Prelude.Maybe TargetGroupStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTargetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteTargetGroupResponse_arn' - The Amazon Resource Name (ARN) of the target group.
--
-- 'id', 'deleteTargetGroupResponse_id' - The ID of the target group.
--
-- 'status', 'deleteTargetGroupResponse_status' - The status. You can retry the operation if the status is
-- @DELETE_FAILED@. However, if you retry it while the status is
-- @DELETE_IN_PROGRESS@, the status doesn\'t change.
--
-- 'httpStatus', 'deleteTargetGroupResponse_httpStatus' - The response's http status code.
newDeleteTargetGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTargetGroupResponse
newDeleteTargetGroupResponse pHttpStatus_ =
  DeleteTargetGroupResponse'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the target group.
deleteTargetGroupResponse_arn :: Lens.Lens' DeleteTargetGroupResponse (Prelude.Maybe Prelude.Text)
deleteTargetGroupResponse_arn = Lens.lens (\DeleteTargetGroupResponse' {arn} -> arn) (\s@DeleteTargetGroupResponse' {} a -> s {arn = a} :: DeleteTargetGroupResponse)

-- | The ID of the target group.
deleteTargetGroupResponse_id :: Lens.Lens' DeleteTargetGroupResponse (Prelude.Maybe Prelude.Text)
deleteTargetGroupResponse_id = Lens.lens (\DeleteTargetGroupResponse' {id} -> id) (\s@DeleteTargetGroupResponse' {} a -> s {id = a} :: DeleteTargetGroupResponse)

-- | The status. You can retry the operation if the status is
-- @DELETE_FAILED@. However, if you retry it while the status is
-- @DELETE_IN_PROGRESS@, the status doesn\'t change.
deleteTargetGroupResponse_status :: Lens.Lens' DeleteTargetGroupResponse (Prelude.Maybe TargetGroupStatus)
deleteTargetGroupResponse_status = Lens.lens (\DeleteTargetGroupResponse' {status} -> status) (\s@DeleteTargetGroupResponse' {} a -> s {status = a} :: DeleteTargetGroupResponse)

-- | The response's http status code.
deleteTargetGroupResponse_httpStatus :: Lens.Lens' DeleteTargetGroupResponse Prelude.Int
deleteTargetGroupResponse_httpStatus = Lens.lens (\DeleteTargetGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteTargetGroupResponse' {} a -> s {httpStatus = a} :: DeleteTargetGroupResponse)

instance Prelude.NFData DeleteTargetGroupResponse where
  rnf DeleteTargetGroupResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
