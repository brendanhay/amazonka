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
-- Module      : Amazonka.Route53Domains.GetOperationDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns the current status of an operation that is not
-- completed.
module Amazonka.Route53Domains.GetOperationDetail
  ( -- * Creating a Request
    GetOperationDetail (..),
    newGetOperationDetail,

    -- * Request Lenses
    getOperationDetail_operationId,

    -- * Destructuring the Response
    GetOperationDetailResponse (..),
    newGetOperationDetailResponse,

    -- * Response Lenses
    getOperationDetailResponse_submittedDate,
    getOperationDetailResponse_message,
    getOperationDetailResponse_type,
    getOperationDetailResponse_domainName,
    getOperationDetailResponse_operationId,
    getOperationDetailResponse_status,
    getOperationDetailResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Domains.Types

-- | The
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>
-- request includes the following element.
--
-- /See:/ 'newGetOperationDetail' smart constructor.
data GetOperationDetail = GetOperationDetail'
  { -- | The identifier for the operation for which you want to get the status.
    -- Route 53 returned the identifier in the response to the original
    -- request.
    operationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOperationDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'getOperationDetail_operationId' - The identifier for the operation for which you want to get the status.
-- Route 53 returned the identifier in the response to the original
-- request.
newGetOperationDetail ::
  -- | 'operationId'
  Prelude.Text ->
  GetOperationDetail
newGetOperationDetail pOperationId_ =
  GetOperationDetail' {operationId = pOperationId_}

-- | The identifier for the operation for which you want to get the status.
-- Route 53 returned the identifier in the response to the original
-- request.
getOperationDetail_operationId :: Lens.Lens' GetOperationDetail Prelude.Text
getOperationDetail_operationId = Lens.lens (\GetOperationDetail' {operationId} -> operationId) (\s@GetOperationDetail' {} a -> s {operationId = a} :: GetOperationDetail)

instance Core.AWSRequest GetOperationDetail where
  type
    AWSResponse GetOperationDetail =
      GetOperationDetailResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOperationDetailResponse'
            Prelude.<$> (x Data..?> "SubmittedDate")
            Prelude.<*> (x Data..?> "Message")
            Prelude.<*> (x Data..?> "Type")
            Prelude.<*> (x Data..?> "DomainName")
            Prelude.<*> (x Data..?> "OperationId")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetOperationDetail where
  hashWithSalt _salt GetOperationDetail' {..} =
    _salt `Prelude.hashWithSalt` operationId

instance Prelude.NFData GetOperationDetail where
  rnf GetOperationDetail' {..} = Prelude.rnf operationId

instance Data.ToHeaders GetOperationDetail where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Domains_v20140515.GetOperationDetail" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetOperationDetail where
  toJSON GetOperationDetail' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("OperationId" Data..= operationId)]
      )

instance Data.ToPath GetOperationDetail where
  toPath = Prelude.const "/"

instance Data.ToQuery GetOperationDetail where
  toQuery = Prelude.const Prelude.mempty

-- | The GetOperationDetail response includes the following elements.
--
-- /See:/ 'newGetOperationDetailResponse' smart constructor.
data GetOperationDetailResponse = GetOperationDetailResponse'
  { -- | The date when the request was submitted.
    submittedDate :: Prelude.Maybe Data.POSIX,
    -- | Detailed information on the status including possible errors.
    message :: Prelude.Maybe Prelude.Text,
    -- | The type of operation that was requested.
    type' :: Prelude.Maybe OperationType,
    -- | The name of a domain.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the operation.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The current status of the requested operation in the system.
    status :: Prelude.Maybe OperationStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOperationDetailResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'submittedDate', 'getOperationDetailResponse_submittedDate' - The date when the request was submitted.
--
-- 'message', 'getOperationDetailResponse_message' - Detailed information on the status including possible errors.
--
-- 'type'', 'getOperationDetailResponse_type' - The type of operation that was requested.
--
-- 'domainName', 'getOperationDetailResponse_domainName' - The name of a domain.
--
-- 'operationId', 'getOperationDetailResponse_operationId' - The identifier for the operation.
--
-- 'status', 'getOperationDetailResponse_status' - The current status of the requested operation in the system.
--
-- 'httpStatus', 'getOperationDetailResponse_httpStatus' - The response's http status code.
newGetOperationDetailResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetOperationDetailResponse
newGetOperationDetailResponse pHttpStatus_ =
  GetOperationDetailResponse'
    { submittedDate =
        Prelude.Nothing,
      message = Prelude.Nothing,
      type' = Prelude.Nothing,
      domainName = Prelude.Nothing,
      operationId = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date when the request was submitted.
getOperationDetailResponse_submittedDate :: Lens.Lens' GetOperationDetailResponse (Prelude.Maybe Prelude.UTCTime)
getOperationDetailResponse_submittedDate = Lens.lens (\GetOperationDetailResponse' {submittedDate} -> submittedDate) (\s@GetOperationDetailResponse' {} a -> s {submittedDate = a} :: GetOperationDetailResponse) Prelude.. Lens.mapping Data._Time

-- | Detailed information on the status including possible errors.
getOperationDetailResponse_message :: Lens.Lens' GetOperationDetailResponse (Prelude.Maybe Prelude.Text)
getOperationDetailResponse_message = Lens.lens (\GetOperationDetailResponse' {message} -> message) (\s@GetOperationDetailResponse' {} a -> s {message = a} :: GetOperationDetailResponse)

-- | The type of operation that was requested.
getOperationDetailResponse_type :: Lens.Lens' GetOperationDetailResponse (Prelude.Maybe OperationType)
getOperationDetailResponse_type = Lens.lens (\GetOperationDetailResponse' {type'} -> type') (\s@GetOperationDetailResponse' {} a -> s {type' = a} :: GetOperationDetailResponse)

-- | The name of a domain.
getOperationDetailResponse_domainName :: Lens.Lens' GetOperationDetailResponse (Prelude.Maybe Prelude.Text)
getOperationDetailResponse_domainName = Lens.lens (\GetOperationDetailResponse' {domainName} -> domainName) (\s@GetOperationDetailResponse' {} a -> s {domainName = a} :: GetOperationDetailResponse)

-- | The identifier for the operation.
getOperationDetailResponse_operationId :: Lens.Lens' GetOperationDetailResponse (Prelude.Maybe Prelude.Text)
getOperationDetailResponse_operationId = Lens.lens (\GetOperationDetailResponse' {operationId} -> operationId) (\s@GetOperationDetailResponse' {} a -> s {operationId = a} :: GetOperationDetailResponse)

-- | The current status of the requested operation in the system.
getOperationDetailResponse_status :: Lens.Lens' GetOperationDetailResponse (Prelude.Maybe OperationStatus)
getOperationDetailResponse_status = Lens.lens (\GetOperationDetailResponse' {status} -> status) (\s@GetOperationDetailResponse' {} a -> s {status = a} :: GetOperationDetailResponse)

-- | The response's http status code.
getOperationDetailResponse_httpStatus :: Lens.Lens' GetOperationDetailResponse Prelude.Int
getOperationDetailResponse_httpStatus = Lens.lens (\GetOperationDetailResponse' {httpStatus} -> httpStatus) (\s@GetOperationDetailResponse' {} a -> s {httpStatus = a} :: GetOperationDetailResponse)

instance Prelude.NFData GetOperationDetailResponse where
  rnf GetOperationDetailResponse' {..} =
    Prelude.rnf submittedDate
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf operationId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
