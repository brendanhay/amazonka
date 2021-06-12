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
-- Module      : Network.AWS.CloudHSM.DeleteHsm
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
-- Deletes an HSM. After completion, this operation cannot be undone and
-- your key material cannot be recovered.
module Network.AWS.CloudHSM.DeleteHsm
  ( -- * Creating a Request
    DeleteHsm (..),
    newDeleteHsm,

    -- * Request Lenses
    deleteHsm_hsmArn,

    -- * Destructuring the Response
    DeleteHsmResponse (..),
    newDeleteHsmResponse,

    -- * Response Lenses
    deleteHsmResponse_httpStatus,
    deleteHsmResponse_status,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the DeleteHsm operation.
--
-- /See:/ 'newDeleteHsm' smart constructor.
data DeleteHsm = DeleteHsm'
  { -- | The ARN of the HSM to delete.
    hsmArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteHsm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hsmArn', 'deleteHsm_hsmArn' - The ARN of the HSM to delete.
newDeleteHsm ::
  -- | 'hsmArn'
  Core.Text ->
  DeleteHsm
newDeleteHsm pHsmArn_ = DeleteHsm' {hsmArn = pHsmArn_}

-- | The ARN of the HSM to delete.
deleteHsm_hsmArn :: Lens.Lens' DeleteHsm Core.Text
deleteHsm_hsmArn = Lens.lens (\DeleteHsm' {hsmArn} -> hsmArn) (\s@DeleteHsm' {} a -> s {hsmArn = a} :: DeleteHsm)

instance Core.AWSRequest DeleteHsm where
  type AWSResponse DeleteHsm = DeleteHsmResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteHsmResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "Status")
      )

instance Core.Hashable DeleteHsm

instance Core.NFData DeleteHsm

instance Core.ToHeaders DeleteHsm where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CloudHsmFrontendService.DeleteHsm" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteHsm where
  toJSON DeleteHsm' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("HsmArn" Core..= hsmArn)]
      )

instance Core.ToPath DeleteHsm where
  toPath = Core.const "/"

instance Core.ToQuery DeleteHsm where
  toQuery = Core.const Core.mempty

-- | Contains the output of the DeleteHsm operation.
--
-- /See:/ 'newDeleteHsmResponse' smart constructor.
data DeleteHsmResponse = DeleteHsmResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The status of the operation.
    status :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteHsmResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteHsmResponse_httpStatus' - The response's http status code.
--
-- 'status', 'deleteHsmResponse_status' - The status of the operation.
newDeleteHsmResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'status'
  Core.Text ->
  DeleteHsmResponse
newDeleteHsmResponse pHttpStatus_ pStatus_ =
  DeleteHsmResponse'
    { httpStatus = pHttpStatus_,
      status = pStatus_
    }

-- | The response's http status code.
deleteHsmResponse_httpStatus :: Lens.Lens' DeleteHsmResponse Core.Int
deleteHsmResponse_httpStatus = Lens.lens (\DeleteHsmResponse' {httpStatus} -> httpStatus) (\s@DeleteHsmResponse' {} a -> s {httpStatus = a} :: DeleteHsmResponse)

-- | The status of the operation.
deleteHsmResponse_status :: Lens.Lens' DeleteHsmResponse Core.Text
deleteHsmResponse_status = Lens.lens (\DeleteHsmResponse' {status} -> status) (\s@DeleteHsmResponse' {} a -> s {status = a} :: DeleteHsmResponse)

instance Core.NFData DeleteHsmResponse
