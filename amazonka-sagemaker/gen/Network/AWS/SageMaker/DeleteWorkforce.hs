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
-- Module      : Network.AWS.SageMaker.DeleteWorkforce
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to delete a workforce.
--
-- If you want to create a new workforce in an AWS Region where a workforce
-- already exists, use this operation to delete the existing workforce and
-- then use to create a new workforce.
--
-- If a private workforce contains one or more work teams, you must use the
-- operation to delete all work teams before you delete the workforce. If
-- you try to delete a workforce that contains one or more work teams, you
-- will recieve a @ResourceInUse@ error.
module Network.AWS.SageMaker.DeleteWorkforce
  ( -- * Creating a Request
    DeleteWorkforce (..),
    newDeleteWorkforce,

    -- * Request Lenses
    deleteWorkforce_workforceName,

    -- * Destructuring the Response
    DeleteWorkforceResponse (..),
    newDeleteWorkforceResponse,

    -- * Response Lenses
    deleteWorkforceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteWorkforce' smart constructor.
data DeleteWorkforce = DeleteWorkforce'
  { -- | The name of the workforce.
    workforceName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteWorkforce' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workforceName', 'deleteWorkforce_workforceName' - The name of the workforce.
newDeleteWorkforce ::
  -- | 'workforceName'
  Core.Text ->
  DeleteWorkforce
newDeleteWorkforce pWorkforceName_ =
  DeleteWorkforce' {workforceName = pWorkforceName_}

-- | The name of the workforce.
deleteWorkforce_workforceName :: Lens.Lens' DeleteWorkforce Core.Text
deleteWorkforce_workforceName = Lens.lens (\DeleteWorkforce' {workforceName} -> workforceName) (\s@DeleteWorkforce' {} a -> s {workforceName = a} :: DeleteWorkforce)

instance Core.AWSRequest DeleteWorkforce where
  type
    AWSResponse DeleteWorkforce =
      DeleteWorkforceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteWorkforceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteWorkforce

instance Core.NFData DeleteWorkforce

instance Core.ToHeaders DeleteWorkforce where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DeleteWorkforce" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteWorkforce where
  toJSON DeleteWorkforce' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("WorkforceName" Core..= workforceName)]
      )

instance Core.ToPath DeleteWorkforce where
  toPath = Core.const "/"

instance Core.ToQuery DeleteWorkforce where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteWorkforceResponse' smart constructor.
data DeleteWorkforceResponse = DeleteWorkforceResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteWorkforceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteWorkforceResponse_httpStatus' - The response's http status code.
newDeleteWorkforceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteWorkforceResponse
newDeleteWorkforceResponse pHttpStatus_ =
  DeleteWorkforceResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteWorkforceResponse_httpStatus :: Lens.Lens' DeleteWorkforceResponse Core.Int
deleteWorkforceResponse_httpStatus = Lens.lens (\DeleteWorkforceResponse' {httpStatus} -> httpStatus) (\s@DeleteWorkforceResponse' {} a -> s {httpStatus = a} :: DeleteWorkforceResponse)

instance Core.NFData DeleteWorkforceResponse
