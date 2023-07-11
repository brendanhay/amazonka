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
-- Module      : Amazonka.CloudHSM.DeleteHapg
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
-- Deletes a high-availability partition group.
module Amazonka.CloudHSM.DeleteHapg
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

import Amazonka.CloudHSM.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the inputs for the DeleteHapg action.
--
-- /See:/ 'newDeleteHapg' smart constructor.
data DeleteHapg = DeleteHapg'
  { -- | The ARN of the high-availability partition group to delete.
    hapgArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteHapg
newDeleteHapg pHapgArn_ =
  DeleteHapg' {hapgArn = pHapgArn_}

-- | The ARN of the high-availability partition group to delete.
deleteHapg_hapgArn :: Lens.Lens' DeleteHapg Prelude.Text
deleteHapg_hapgArn = Lens.lens (\DeleteHapg' {hapgArn} -> hapgArn) (\s@DeleteHapg' {} a -> s {hapgArn = a} :: DeleteHapg)

instance Core.AWSRequest DeleteHapg where
  type AWSResponse DeleteHapg = DeleteHapgResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteHapgResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Status")
      )

instance Prelude.Hashable DeleteHapg where
  hashWithSalt _salt DeleteHapg' {..} =
    _salt `Prelude.hashWithSalt` hapgArn

instance Prelude.NFData DeleteHapg where
  rnf DeleteHapg' {..} = Prelude.rnf hapgArn

instance Data.ToHeaders DeleteHapg where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CloudHsmFrontendService.DeleteHapg" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteHapg where
  toJSON DeleteHapg' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("HapgArn" Data..= hapgArn)]
      )

instance Data.ToPath DeleteHapg where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteHapg where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of the DeleteHapg action.
--
-- /See:/ 'newDeleteHapgResponse' smart constructor.
data DeleteHapgResponse = DeleteHapgResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the action.
    status :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'status'
  Prelude.Text ->
  DeleteHapgResponse
newDeleteHapgResponse pHttpStatus_ pStatus_ =
  DeleteHapgResponse'
    { httpStatus = pHttpStatus_,
      status = pStatus_
    }

-- | The response's http status code.
deleteHapgResponse_httpStatus :: Lens.Lens' DeleteHapgResponse Prelude.Int
deleteHapgResponse_httpStatus = Lens.lens (\DeleteHapgResponse' {httpStatus} -> httpStatus) (\s@DeleteHapgResponse' {} a -> s {httpStatus = a} :: DeleteHapgResponse)

-- | The status of the action.
deleteHapgResponse_status :: Lens.Lens' DeleteHapgResponse Prelude.Text
deleteHapgResponse_status = Lens.lens (\DeleteHapgResponse' {status} -> status) (\s@DeleteHapgResponse' {} a -> s {status = a} :: DeleteHapgResponse)

instance Prelude.NFData DeleteHapgResponse where
  rnf DeleteHapgResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf status
