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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StepFunctions.Types

-- | /See:/ 'newDeleteActivity' smart constructor.
data DeleteActivity = DeleteActivity'
  { -- | The Amazon Resource Name (ARN) of the activity to delete.
    activityArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteActivity
newDeleteActivity pActivityArn_ =
  DeleteActivity' {activityArn = pActivityArn_}

-- | The Amazon Resource Name (ARN) of the activity to delete.
deleteActivity_activityArn :: Lens.Lens' DeleteActivity Prelude.Text
deleteActivity_activityArn = Lens.lens (\DeleteActivity' {activityArn} -> activityArn) (\s@DeleteActivity' {} a -> s {activityArn = a} :: DeleteActivity)

instance Prelude.AWSRequest DeleteActivity where
  type Rs DeleteActivity = DeleteActivityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteActivityResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteActivity

instance Prelude.NFData DeleteActivity

instance Prelude.ToHeaders DeleteActivity where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSStepFunctions.DeleteActivity" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.0" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteActivity where
  toJSON DeleteActivity' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("activityArn" Prelude..= activityArn)
          ]
      )

instance Prelude.ToPath DeleteActivity where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteActivity where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteActivityResponse' smart constructor.
data DeleteActivityResponse = DeleteActivityResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteActivityResponse
newDeleteActivityResponse pHttpStatus_ =
  DeleteActivityResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteActivityResponse_httpStatus :: Lens.Lens' DeleteActivityResponse Prelude.Int
deleteActivityResponse_httpStatus = Lens.lens (\DeleteActivityResponse' {httpStatus} -> httpStatus) (\s@DeleteActivityResponse' {} a -> s {httpStatus = a} :: DeleteActivityResponse)

instance Prelude.NFData DeleteActivityResponse
