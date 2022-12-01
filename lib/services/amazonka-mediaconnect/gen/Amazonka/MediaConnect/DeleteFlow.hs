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
-- Module      : Amazonka.MediaConnect.DeleteFlow
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a flow. Before you can delete a flow, you must stop the flow.
module Amazonka.MediaConnect.DeleteFlow
  ( -- * Creating a Request
    DeleteFlow (..),
    newDeleteFlow,

    -- * Request Lenses
    deleteFlow_flowArn,

    -- * Destructuring the Response
    DeleteFlowResponse (..),
    newDeleteFlowResponse,

    -- * Response Lenses
    deleteFlowResponse_status,
    deleteFlowResponse_flowArn,
    deleteFlowResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFlow' smart constructor.
data DeleteFlow = DeleteFlow'
  { -- | The ARN of the flow that you want to delete.
    flowArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFlow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowArn', 'deleteFlow_flowArn' - The ARN of the flow that you want to delete.
newDeleteFlow ::
  -- | 'flowArn'
  Prelude.Text ->
  DeleteFlow
newDeleteFlow pFlowArn_ =
  DeleteFlow' {flowArn = pFlowArn_}

-- | The ARN of the flow that you want to delete.
deleteFlow_flowArn :: Lens.Lens' DeleteFlow Prelude.Text
deleteFlow_flowArn = Lens.lens (\DeleteFlow' {flowArn} -> flowArn) (\s@DeleteFlow' {} a -> s {flowArn = a} :: DeleteFlow)

instance Core.AWSRequest DeleteFlow where
  type AWSResponse DeleteFlow = DeleteFlowResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteFlowResponse'
            Prelude.<$> (x Core..?> "status")
            Prelude.<*> (x Core..?> "flowArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFlow where
  hashWithSalt _salt DeleteFlow' {..} =
    _salt `Prelude.hashWithSalt` flowArn

instance Prelude.NFData DeleteFlow where
  rnf DeleteFlow' {..} = Prelude.rnf flowArn

instance Core.ToHeaders DeleteFlow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteFlow where
  toPath DeleteFlow' {..} =
    Prelude.mconcat ["/v1/flows/", Core.toBS flowArn]

instance Core.ToQuery DeleteFlow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFlowResponse' smart constructor.
data DeleteFlowResponse = DeleteFlowResponse'
  { -- | The status of the flow when the DeleteFlow process begins.
    status :: Prelude.Maybe Status,
    -- | The ARN of the flow that was deleted.
    flowArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFlowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'deleteFlowResponse_status' - The status of the flow when the DeleteFlow process begins.
--
-- 'flowArn', 'deleteFlowResponse_flowArn' - The ARN of the flow that was deleted.
--
-- 'httpStatus', 'deleteFlowResponse_httpStatus' - The response's http status code.
newDeleteFlowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteFlowResponse
newDeleteFlowResponse pHttpStatus_ =
  DeleteFlowResponse'
    { status = Prelude.Nothing,
      flowArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the flow when the DeleteFlow process begins.
deleteFlowResponse_status :: Lens.Lens' DeleteFlowResponse (Prelude.Maybe Status)
deleteFlowResponse_status = Lens.lens (\DeleteFlowResponse' {status} -> status) (\s@DeleteFlowResponse' {} a -> s {status = a} :: DeleteFlowResponse)

-- | The ARN of the flow that was deleted.
deleteFlowResponse_flowArn :: Lens.Lens' DeleteFlowResponse (Prelude.Maybe Prelude.Text)
deleteFlowResponse_flowArn = Lens.lens (\DeleteFlowResponse' {flowArn} -> flowArn) (\s@DeleteFlowResponse' {} a -> s {flowArn = a} :: DeleteFlowResponse)

-- | The response's http status code.
deleteFlowResponse_httpStatus :: Lens.Lens' DeleteFlowResponse Prelude.Int
deleteFlowResponse_httpStatus = Lens.lens (\DeleteFlowResponse' {httpStatus} -> httpStatus) (\s@DeleteFlowResponse' {} a -> s {httpStatus = a} :: DeleteFlowResponse)

instance Prelude.NFData DeleteFlowResponse where
  rnf DeleteFlowResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf httpStatus
