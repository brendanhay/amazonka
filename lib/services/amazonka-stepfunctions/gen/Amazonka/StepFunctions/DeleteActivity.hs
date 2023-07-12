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
-- Module      : Amazonka.StepFunctions.DeleteActivity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an activity.
module Amazonka.StepFunctions.DeleteActivity
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StepFunctions.Types

-- | /See:/ 'newDeleteActivity' smart constructor.
data DeleteActivity = DeleteActivity'
  { -- | The Amazon Resource Name (ARN) of the activity to delete.
    activityArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteActivity where
  type
    AWSResponse DeleteActivity =
      DeleteActivityResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteActivityResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteActivity where
  hashWithSalt _salt DeleteActivity' {..} =
    _salt `Prelude.hashWithSalt` activityArn

instance Prelude.NFData DeleteActivity where
  rnf DeleteActivity' {..} = Prelude.rnf activityArn

instance Data.ToHeaders DeleteActivity where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSStepFunctions.DeleteActivity" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteActivity where
  toJSON DeleteActivity' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("activityArn" Data..= activityArn)]
      )

instance Data.ToPath DeleteActivity where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteActivity where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteActivityResponse' smart constructor.
data DeleteActivityResponse = DeleteActivityResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData DeleteActivityResponse where
  rnf DeleteActivityResponse' {..} =
    Prelude.rnf httpStatus
