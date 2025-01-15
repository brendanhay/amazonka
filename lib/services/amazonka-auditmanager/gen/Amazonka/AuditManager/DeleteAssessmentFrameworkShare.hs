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
-- Module      : Amazonka.AuditManager.DeleteAssessmentFrameworkShare
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a share request for a custom framework in Audit Manager.
module Amazonka.AuditManager.DeleteAssessmentFrameworkShare
  ( -- * Creating a Request
    DeleteAssessmentFrameworkShare (..),
    newDeleteAssessmentFrameworkShare,

    -- * Request Lenses
    deleteAssessmentFrameworkShare_requestId,
    deleteAssessmentFrameworkShare_requestType,

    -- * Destructuring the Response
    DeleteAssessmentFrameworkShareResponse (..),
    newDeleteAssessmentFrameworkShareResponse,

    -- * Response Lenses
    deleteAssessmentFrameworkShareResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAssessmentFrameworkShare' smart constructor.
data DeleteAssessmentFrameworkShare = DeleteAssessmentFrameworkShare'
  { -- | The unique identifier for the share request to be deleted.
    requestId :: Prelude.Text,
    -- | Specifies whether the share request is a sent request or a received
    -- request.
    requestType :: ShareRequestType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAssessmentFrameworkShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'deleteAssessmentFrameworkShare_requestId' - The unique identifier for the share request to be deleted.
--
-- 'requestType', 'deleteAssessmentFrameworkShare_requestType' - Specifies whether the share request is a sent request or a received
-- request.
newDeleteAssessmentFrameworkShare ::
  -- | 'requestId'
  Prelude.Text ->
  -- | 'requestType'
  ShareRequestType ->
  DeleteAssessmentFrameworkShare
newDeleteAssessmentFrameworkShare
  pRequestId_
  pRequestType_ =
    DeleteAssessmentFrameworkShare'
      { requestId =
          pRequestId_,
        requestType = pRequestType_
      }

-- | The unique identifier for the share request to be deleted.
deleteAssessmentFrameworkShare_requestId :: Lens.Lens' DeleteAssessmentFrameworkShare Prelude.Text
deleteAssessmentFrameworkShare_requestId = Lens.lens (\DeleteAssessmentFrameworkShare' {requestId} -> requestId) (\s@DeleteAssessmentFrameworkShare' {} a -> s {requestId = a} :: DeleteAssessmentFrameworkShare)

-- | Specifies whether the share request is a sent request or a received
-- request.
deleteAssessmentFrameworkShare_requestType :: Lens.Lens' DeleteAssessmentFrameworkShare ShareRequestType
deleteAssessmentFrameworkShare_requestType = Lens.lens (\DeleteAssessmentFrameworkShare' {requestType} -> requestType) (\s@DeleteAssessmentFrameworkShare' {} a -> s {requestType = a} :: DeleteAssessmentFrameworkShare)

instance
  Core.AWSRequest
    DeleteAssessmentFrameworkShare
  where
  type
    AWSResponse DeleteAssessmentFrameworkShare =
      DeleteAssessmentFrameworkShareResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAssessmentFrameworkShareResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteAssessmentFrameworkShare
  where
  hashWithSalt
    _salt
    DeleteAssessmentFrameworkShare' {..} =
      _salt
        `Prelude.hashWithSalt` requestId
        `Prelude.hashWithSalt` requestType

instance
  Prelude.NFData
    DeleteAssessmentFrameworkShare
  where
  rnf DeleteAssessmentFrameworkShare' {..} =
    Prelude.rnf requestId `Prelude.seq`
      Prelude.rnf requestType

instance
  Data.ToHeaders
    DeleteAssessmentFrameworkShare
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

instance Data.ToPath DeleteAssessmentFrameworkShare where
  toPath DeleteAssessmentFrameworkShare' {..} =
    Prelude.mconcat
      [ "/assessmentFrameworkShareRequests/",
        Data.toBS requestId
      ]

instance Data.ToQuery DeleteAssessmentFrameworkShare where
  toQuery DeleteAssessmentFrameworkShare' {..} =
    Prelude.mconcat ["requestType" Data.=: requestType]

-- | /See:/ 'newDeleteAssessmentFrameworkShareResponse' smart constructor.
data DeleteAssessmentFrameworkShareResponse = DeleteAssessmentFrameworkShareResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAssessmentFrameworkShareResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAssessmentFrameworkShareResponse_httpStatus' - The response's http status code.
newDeleteAssessmentFrameworkShareResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAssessmentFrameworkShareResponse
newDeleteAssessmentFrameworkShareResponse
  pHttpStatus_ =
    DeleteAssessmentFrameworkShareResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteAssessmentFrameworkShareResponse_httpStatus :: Lens.Lens' DeleteAssessmentFrameworkShareResponse Prelude.Int
deleteAssessmentFrameworkShareResponse_httpStatus = Lens.lens (\DeleteAssessmentFrameworkShareResponse' {httpStatus} -> httpStatus) (\s@DeleteAssessmentFrameworkShareResponse' {} a -> s {httpStatus = a} :: DeleteAssessmentFrameworkShareResponse)

instance
  Prelude.NFData
    DeleteAssessmentFrameworkShareResponse
  where
  rnf DeleteAssessmentFrameworkShareResponse' {..} =
    Prelude.rnf httpStatus
