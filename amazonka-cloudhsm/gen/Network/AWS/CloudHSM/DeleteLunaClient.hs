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
-- Module      : Network.AWS.CloudHSM.DeleteLunaClient
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
-- Deletes a client.
module Network.AWS.CloudHSM.DeleteLunaClient
  ( -- * Creating a Request
    DeleteLunaClient (..),
    newDeleteLunaClient,

    -- * Request Lenses
    deleteLunaClient_clientArn,

    -- * Destructuring the Response
    DeleteLunaClientResponse (..),
    newDeleteLunaClientResponse,

    -- * Response Lenses
    deleteLunaClientResponse_httpStatus,
    deleteLunaClientResponse_status,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteLunaClient' smart constructor.
data DeleteLunaClient = DeleteLunaClient'
  { -- | The ARN of the client to delete.
    clientArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLunaClient' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientArn', 'deleteLunaClient_clientArn' - The ARN of the client to delete.
newDeleteLunaClient ::
  -- | 'clientArn'
  Prelude.Text ->
  DeleteLunaClient
newDeleteLunaClient pClientArn_ =
  DeleteLunaClient' {clientArn = pClientArn_}

-- | The ARN of the client to delete.
deleteLunaClient_clientArn :: Lens.Lens' DeleteLunaClient Prelude.Text
deleteLunaClient_clientArn = Lens.lens (\DeleteLunaClient' {clientArn} -> clientArn) (\s@DeleteLunaClient' {} a -> s {clientArn = a} :: DeleteLunaClient)

instance Core.AWSRequest DeleteLunaClient where
  type
    AWSResponse DeleteLunaClient =
      DeleteLunaClientResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteLunaClientResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "Status")
      )

instance Prelude.Hashable DeleteLunaClient

instance Prelude.NFData DeleteLunaClient

instance Core.ToHeaders DeleteLunaClient where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CloudHsmFrontendService.DeleteLunaClient" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteLunaClient where
  toJSON DeleteLunaClient' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ClientArn" Core..= clientArn)]
      )

instance Core.ToPath DeleteLunaClient where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteLunaClient where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLunaClientResponse' smart constructor.
data DeleteLunaClientResponse = DeleteLunaClientResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the action.
    status :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLunaClientResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteLunaClientResponse_httpStatus' - The response's http status code.
--
-- 'status', 'deleteLunaClientResponse_status' - The status of the action.
newDeleteLunaClientResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'status'
  Prelude.Text ->
  DeleteLunaClientResponse
newDeleteLunaClientResponse pHttpStatus_ pStatus_ =
  DeleteLunaClientResponse'
    { httpStatus =
        pHttpStatus_,
      status = pStatus_
    }

-- | The response's http status code.
deleteLunaClientResponse_httpStatus :: Lens.Lens' DeleteLunaClientResponse Prelude.Int
deleteLunaClientResponse_httpStatus = Lens.lens (\DeleteLunaClientResponse' {httpStatus} -> httpStatus) (\s@DeleteLunaClientResponse' {} a -> s {httpStatus = a} :: DeleteLunaClientResponse)

-- | The status of the action.
deleteLunaClientResponse_status :: Lens.Lens' DeleteLunaClientResponse Prelude.Text
deleteLunaClientResponse_status = Lens.lens (\DeleteLunaClientResponse' {status} -> status) (\s@DeleteLunaClientResponse' {} a -> s {status = a} :: DeleteLunaClientResponse)

instance Prelude.NFData DeleteLunaClientResponse
