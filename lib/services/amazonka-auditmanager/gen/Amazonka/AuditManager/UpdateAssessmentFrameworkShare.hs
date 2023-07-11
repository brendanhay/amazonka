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
-- Module      : Amazonka.AuditManager.UpdateAssessmentFrameworkShare
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a share request for a custom framework in Audit Manager.
module Amazonka.AuditManager.UpdateAssessmentFrameworkShare
  ( -- * Creating a Request
    UpdateAssessmentFrameworkShare (..),
    newUpdateAssessmentFrameworkShare,

    -- * Request Lenses
    updateAssessmentFrameworkShare_requestId,
    updateAssessmentFrameworkShare_requestType,
    updateAssessmentFrameworkShare_action,

    -- * Destructuring the Response
    UpdateAssessmentFrameworkShareResponse (..),
    newUpdateAssessmentFrameworkShareResponse,

    -- * Response Lenses
    updateAssessmentFrameworkShareResponse_assessmentFrameworkShareRequest,
    updateAssessmentFrameworkShareResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAssessmentFrameworkShare' smart constructor.
data UpdateAssessmentFrameworkShare = UpdateAssessmentFrameworkShare'
  { -- | The unique identifier for the share request.
    requestId :: Prelude.Text,
    -- | Specifies whether the share request is a sent request or a received
    -- request.
    requestType :: ShareRequestType,
    -- | Specifies the update action for the share request.
    action :: ShareRequestAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAssessmentFrameworkShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'updateAssessmentFrameworkShare_requestId' - The unique identifier for the share request.
--
-- 'requestType', 'updateAssessmentFrameworkShare_requestType' - Specifies whether the share request is a sent request or a received
-- request.
--
-- 'action', 'updateAssessmentFrameworkShare_action' - Specifies the update action for the share request.
newUpdateAssessmentFrameworkShare ::
  -- | 'requestId'
  Prelude.Text ->
  -- | 'requestType'
  ShareRequestType ->
  -- | 'action'
  ShareRequestAction ->
  UpdateAssessmentFrameworkShare
newUpdateAssessmentFrameworkShare
  pRequestId_
  pRequestType_
  pAction_ =
    UpdateAssessmentFrameworkShare'
      { requestId =
          pRequestId_,
        requestType = pRequestType_,
        action = pAction_
      }

-- | The unique identifier for the share request.
updateAssessmentFrameworkShare_requestId :: Lens.Lens' UpdateAssessmentFrameworkShare Prelude.Text
updateAssessmentFrameworkShare_requestId = Lens.lens (\UpdateAssessmentFrameworkShare' {requestId} -> requestId) (\s@UpdateAssessmentFrameworkShare' {} a -> s {requestId = a} :: UpdateAssessmentFrameworkShare)

-- | Specifies whether the share request is a sent request or a received
-- request.
updateAssessmentFrameworkShare_requestType :: Lens.Lens' UpdateAssessmentFrameworkShare ShareRequestType
updateAssessmentFrameworkShare_requestType = Lens.lens (\UpdateAssessmentFrameworkShare' {requestType} -> requestType) (\s@UpdateAssessmentFrameworkShare' {} a -> s {requestType = a} :: UpdateAssessmentFrameworkShare)

-- | Specifies the update action for the share request.
updateAssessmentFrameworkShare_action :: Lens.Lens' UpdateAssessmentFrameworkShare ShareRequestAction
updateAssessmentFrameworkShare_action = Lens.lens (\UpdateAssessmentFrameworkShare' {action} -> action) (\s@UpdateAssessmentFrameworkShare' {} a -> s {action = a} :: UpdateAssessmentFrameworkShare)

instance
  Core.AWSRequest
    UpdateAssessmentFrameworkShare
  where
  type
    AWSResponse UpdateAssessmentFrameworkShare =
      UpdateAssessmentFrameworkShareResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAssessmentFrameworkShareResponse'
            Prelude.<$> (x Data..?> "assessmentFrameworkShareRequest")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateAssessmentFrameworkShare
  where
  hashWithSalt
    _salt
    UpdateAssessmentFrameworkShare' {..} =
      _salt
        `Prelude.hashWithSalt` requestId
        `Prelude.hashWithSalt` requestType
        `Prelude.hashWithSalt` action

instance
  Prelude.NFData
    UpdateAssessmentFrameworkShare
  where
  rnf UpdateAssessmentFrameworkShare' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf requestType
      `Prelude.seq` Prelude.rnf action

instance
  Data.ToHeaders
    UpdateAssessmentFrameworkShare
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAssessmentFrameworkShare where
  toJSON UpdateAssessmentFrameworkShare' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("requestType" Data..= requestType),
            Prelude.Just ("action" Data..= action)
          ]
      )

instance Data.ToPath UpdateAssessmentFrameworkShare where
  toPath UpdateAssessmentFrameworkShare' {..} =
    Prelude.mconcat
      [ "/assessmentFrameworkShareRequests/",
        Data.toBS requestId
      ]

instance Data.ToQuery UpdateAssessmentFrameworkShare where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAssessmentFrameworkShareResponse' smart constructor.
data UpdateAssessmentFrameworkShareResponse = UpdateAssessmentFrameworkShareResponse'
  { -- | The updated share request that\'s returned by the
    -- @UpdateAssessmentFrameworkShare@ operation.
    assessmentFrameworkShareRequest :: Prelude.Maybe AssessmentFrameworkShareRequest,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAssessmentFrameworkShareResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentFrameworkShareRequest', 'updateAssessmentFrameworkShareResponse_assessmentFrameworkShareRequest' - The updated share request that\'s returned by the
-- @UpdateAssessmentFrameworkShare@ operation.
--
-- 'httpStatus', 'updateAssessmentFrameworkShareResponse_httpStatus' - The response's http status code.
newUpdateAssessmentFrameworkShareResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAssessmentFrameworkShareResponse
newUpdateAssessmentFrameworkShareResponse
  pHttpStatus_ =
    UpdateAssessmentFrameworkShareResponse'
      { assessmentFrameworkShareRequest =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The updated share request that\'s returned by the
-- @UpdateAssessmentFrameworkShare@ operation.
updateAssessmentFrameworkShareResponse_assessmentFrameworkShareRequest :: Lens.Lens' UpdateAssessmentFrameworkShareResponse (Prelude.Maybe AssessmentFrameworkShareRequest)
updateAssessmentFrameworkShareResponse_assessmentFrameworkShareRequest = Lens.lens (\UpdateAssessmentFrameworkShareResponse' {assessmentFrameworkShareRequest} -> assessmentFrameworkShareRequest) (\s@UpdateAssessmentFrameworkShareResponse' {} a -> s {assessmentFrameworkShareRequest = a} :: UpdateAssessmentFrameworkShareResponse)

-- | The response's http status code.
updateAssessmentFrameworkShareResponse_httpStatus :: Lens.Lens' UpdateAssessmentFrameworkShareResponse Prelude.Int
updateAssessmentFrameworkShareResponse_httpStatus = Lens.lens (\UpdateAssessmentFrameworkShareResponse' {httpStatus} -> httpStatus) (\s@UpdateAssessmentFrameworkShareResponse' {} a -> s {httpStatus = a} :: UpdateAssessmentFrameworkShareResponse)

instance
  Prelude.NFData
    UpdateAssessmentFrameworkShareResponse
  where
  rnf UpdateAssessmentFrameworkShareResponse' {..} =
    Prelude.rnf assessmentFrameworkShareRequest
      `Prelude.seq` Prelude.rnf httpStatus
