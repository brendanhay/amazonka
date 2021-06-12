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
-- Module      : Network.AWS.StepFunctions.DeleteActivity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an activity.
module Network.AWS.StepFunctions.DeleteActivity
  ( -- * Creating a Request
    DeleteActivity (..),
    newDeleteActivity,

    -- * Request Lenses
    deleteActivity_activityArn,

    -- * Destructuring the Response
    DeleteActivityResponse (..),
    newDeleteActivityResponse,

    -- * Response Lenses
    deleteActivityResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StepFunctions.Types

-- | /See:/ 'newDeleteActivity' smart constructor.
data DeleteActivity = DeleteActivity'
  { -- | The Amazon Resource Name (ARN) of the activity to delete.
    activityArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activityArn', 'deleteActivity_activityArn' - The Amazon Resource Name (ARN) of the activity to delete.
newDeleteActivity ::
  -- | 'activityArn'
  Core.Text ->
  DeleteActivity
newDeleteActivity pActivityArn_ =
  DeleteActivity' {activityArn = pActivityArn_}

-- | The Amazon Resource Name (ARN) of the activity to delete.
deleteActivity_activityArn :: Lens.Lens' DeleteActivity Core.Text
deleteActivity_activityArn = Lens.lens (\DeleteActivity' {activityArn} -> activityArn) (\s@DeleteActivity' {} a -> s {activityArn = a} :: DeleteActivity)

instance Core.AWSRequest DeleteActivity where
  type
    AWSResponse DeleteActivity =
      DeleteActivityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteActivityResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteActivity

instance Core.NFData DeleteActivity

instance Core.ToHeaders DeleteActivity where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSStepFunctions.DeleteActivity" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteActivity where
  toJSON DeleteActivity' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("activityArn" Core..= activityArn)]
      )

instance Core.ToPath DeleteActivity where
  toPath = Core.const "/"

instance Core.ToQuery DeleteActivity where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteActivityResponse' smart constructor.
data DeleteActivityResponse = DeleteActivityResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteActivityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteActivityResponse_httpStatus' - The response's http status code.
newDeleteActivityResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteActivityResponse
newDeleteActivityResponse pHttpStatus_ =
  DeleteActivityResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteActivityResponse_httpStatus :: Lens.Lens' DeleteActivityResponse Core.Int
deleteActivityResponse_httpStatus = Lens.lens (\DeleteActivityResponse' {httpStatus} -> httpStatus) (\s@DeleteActivityResponse' {} a -> s {httpStatus = a} :: DeleteActivityResponse)

instance Core.NFData DeleteActivityResponse
