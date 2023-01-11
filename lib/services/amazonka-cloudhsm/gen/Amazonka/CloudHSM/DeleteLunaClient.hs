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
-- Module      : Amazonka.CloudHSM.DeleteLunaClient
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__. For more
-- information, see
-- <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs>,
-- the
-- <https://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide>,
-- and the
-- <https://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference>.
--
-- __For information about the current version of AWS CloudHSM__, see
-- <http://aws.amazon.com/cloudhsm/ AWS CloudHSM>, the
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide>,
-- and the
-- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference>.
--
-- Deletes a client.
module Amazonka.CloudHSM.DeleteLunaClient
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

import Amazonka.CloudHSM.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteLunaClientResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Status")
      )

instance Prelude.Hashable DeleteLunaClient where
  hashWithSalt _salt DeleteLunaClient' {..} =
    _salt `Prelude.hashWithSalt` clientArn

instance Prelude.NFData DeleteLunaClient where
  rnf DeleteLunaClient' {..} = Prelude.rnf clientArn

instance Data.ToHeaders DeleteLunaClient where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CloudHsmFrontendService.DeleteLunaClient" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteLunaClient where
  toJSON DeleteLunaClient' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ClientArn" Data..= clientArn)]
      )

instance Data.ToPath DeleteLunaClient where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteLunaClient where
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

instance Prelude.NFData DeleteLunaClientResponse where
  rnf DeleteLunaClientResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf status
