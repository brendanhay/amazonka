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
-- Module      : Network.AWS.CloudHSM.DeleteHapg
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__. For more
-- information, see
-- <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs>,
-- the
-- <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide>,
-- and the
-- <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference>.
--
-- __For information about the current version of AWS CloudHSM__, see
-- <http://aws.amazon.com/cloudhsm/ AWS CloudHSM>, the
-- <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide>,
-- and the
-- <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference>.
--
-- Deletes a high-availability partition group.
module Network.AWS.CloudHSM.DeleteHapg
  ( -- * Creating a Request
    DeleteHapg (..),
    newDeleteHapg,

    -- * Request Lenses
    deleteHapg_hapgArn,

    -- * Destructuring the Response
    DeleteHapgResponse (..),
    newDeleteHapgResponse,

    -- * Response Lenses
    deleteHapgResponse_httpStatus,
    deleteHapgResponse_status,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the DeleteHapg action.
--
-- /See:/ 'newDeleteHapg' smart constructor.
data DeleteHapg = DeleteHapg'
  { -- | The ARN of the high-availability partition group to delete.
    hapgArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteHapg' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hapgArn', 'deleteHapg_hapgArn' - The ARN of the high-availability partition group to delete.
newDeleteHapg ::
  -- | 'hapgArn'
  Core.Text ->
  DeleteHapg
newDeleteHapg pHapgArn_ =
  DeleteHapg' {hapgArn = pHapgArn_}

-- | The ARN of the high-availability partition group to delete.
deleteHapg_hapgArn :: Lens.Lens' DeleteHapg Core.Text
deleteHapg_hapgArn = Lens.lens (\DeleteHapg' {hapgArn} -> hapgArn) (\s@DeleteHapg' {} a -> s {hapgArn = a} :: DeleteHapg)

instance Core.AWSRequest DeleteHapg where
  type AWSResponse DeleteHapg = DeleteHapgResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteHapgResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "Status")
      )

instance Core.Hashable DeleteHapg

instance Core.NFData DeleteHapg

instance Core.ToHeaders DeleteHapg where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CloudHsmFrontendService.DeleteHapg" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteHapg where
  toJSON DeleteHapg' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("HapgArn" Core..= hapgArn)]
      )

instance Core.ToPath DeleteHapg where
  toPath = Core.const "/"

instance Core.ToQuery DeleteHapg where
  toQuery = Core.const Core.mempty

-- | Contains the output of the DeleteHapg action.
--
-- /See:/ 'newDeleteHapgResponse' smart constructor.
data DeleteHapgResponse = DeleteHapgResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The status of the action.
    status :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteHapgResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteHapgResponse_httpStatus' - The response's http status code.
--
-- 'status', 'deleteHapgResponse_status' - The status of the action.
newDeleteHapgResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'status'
  Core.Text ->
  DeleteHapgResponse
newDeleteHapgResponse pHttpStatus_ pStatus_ =
  DeleteHapgResponse'
    { httpStatus = pHttpStatus_,
      status = pStatus_
    }

-- | The response's http status code.
deleteHapgResponse_httpStatus :: Lens.Lens' DeleteHapgResponse Core.Int
deleteHapgResponse_httpStatus = Lens.lens (\DeleteHapgResponse' {httpStatus} -> httpStatus) (\s@DeleteHapgResponse' {} a -> s {httpStatus = a} :: DeleteHapgResponse)

-- | The status of the action.
deleteHapgResponse_status :: Lens.Lens' DeleteHapgResponse Core.Text
deleteHapgResponse_status = Lens.lens (\DeleteHapgResponse' {status} -> status) (\s@DeleteHapgResponse' {} a -> s {status = a} :: DeleteHapgResponse)

instance Core.NFData DeleteHapgResponse
