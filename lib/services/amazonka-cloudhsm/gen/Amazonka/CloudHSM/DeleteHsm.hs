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
-- Module      : Amazonka.CloudHSM.DeleteHsm
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
-- Deletes an HSM. After completion, this operation cannot be undone and
-- your key material cannot be recovered.
module Amazonka.CloudHSM.DeleteHsm
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

import Amazonka.CloudHSM.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the inputs for the DeleteHsm operation.
--
-- /See:/ 'newDeleteHsm' smart constructor.
data DeleteHsm = DeleteHsm'
  { -- | The ARN of the HSM to delete.
    hsmArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteHsm
newDeleteHsm pHsmArn_ = DeleteHsm' {hsmArn = pHsmArn_}

-- | The ARN of the HSM to delete.
deleteHsm_hsmArn :: Lens.Lens' DeleteHsm Prelude.Text
deleteHsm_hsmArn = Lens.lens (\DeleteHsm' {hsmArn} -> hsmArn) (\s@DeleteHsm' {} a -> s {hsmArn = a} :: DeleteHsm)

instance Core.AWSRequest DeleteHsm where
  type AWSResponse DeleteHsm = DeleteHsmResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteHsmResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Status")
      )

instance Prelude.Hashable DeleteHsm where
  hashWithSalt _salt DeleteHsm' {..} =
    _salt `Prelude.hashWithSalt` hsmArn

instance Prelude.NFData DeleteHsm where
  rnf DeleteHsm' {..} = Prelude.rnf hsmArn

instance Data.ToHeaders DeleteHsm where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CloudHsmFrontendService.DeleteHsm" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteHsm where
  toJSON DeleteHsm' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("HsmArn" Data..= hsmArn)]
      )

instance Data.ToPath DeleteHsm where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteHsm where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of the DeleteHsm operation.
--
-- /See:/ 'newDeleteHsmResponse' smart constructor.
data DeleteHsmResponse = DeleteHsmResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the operation.
    status :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'status'
  Prelude.Text ->
  DeleteHsmResponse
newDeleteHsmResponse pHttpStatus_ pStatus_ =
  DeleteHsmResponse'
    { httpStatus = pHttpStatus_,
      status = pStatus_
    }

-- | The response's http status code.
deleteHsmResponse_httpStatus :: Lens.Lens' DeleteHsmResponse Prelude.Int
deleteHsmResponse_httpStatus = Lens.lens (\DeleteHsmResponse' {httpStatus} -> httpStatus) (\s@DeleteHsmResponse' {} a -> s {httpStatus = a} :: DeleteHsmResponse)

-- | The status of the operation.
deleteHsmResponse_status :: Lens.Lens' DeleteHsmResponse Prelude.Text
deleteHsmResponse_status = Lens.lens (\DeleteHsmResponse' {status} -> status) (\s@DeleteHsmResponse' {} a -> s {status = a} :: DeleteHsmResponse)

instance Prelude.NFData DeleteHsmResponse where
  rnf DeleteHsmResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf status
