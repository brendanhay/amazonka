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
-- Module      : Amazonka.TNB.GetSolNetworkOperation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the details of a network operation, including the tasks involved in
-- the network operation and the status of the tasks.
--
-- A network operation is any operation that is done to your network, such
-- as network instance instantiation or termination.
module Amazonka.TNB.GetSolNetworkOperation
  ( -- * Creating a Request
    GetSolNetworkOperation (..),
    newGetSolNetworkOperation,

    -- * Request Lenses
    getSolNetworkOperation_nsLcmOpOccId,

    -- * Destructuring the Response
    GetSolNetworkOperationResponse (..),
    newGetSolNetworkOperationResponse,

    -- * Response Lenses
    getSolNetworkOperationResponse_error,
    getSolNetworkOperationResponse_id,
    getSolNetworkOperationResponse_lcmOperationType,
    getSolNetworkOperationResponse_metadata,
    getSolNetworkOperationResponse_nsInstanceId,
    getSolNetworkOperationResponse_operationState,
    getSolNetworkOperationResponse_tags,
    getSolNetworkOperationResponse_tasks,
    getSolNetworkOperationResponse_httpStatus,
    getSolNetworkOperationResponse_arn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newGetSolNetworkOperation' smart constructor.
data GetSolNetworkOperation = GetSolNetworkOperation'
  { -- | The identifier of the network operation.
    nsLcmOpOccId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolNetworkOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nsLcmOpOccId', 'getSolNetworkOperation_nsLcmOpOccId' - The identifier of the network operation.
newGetSolNetworkOperation ::
  -- | 'nsLcmOpOccId'
  Prelude.Text ->
  GetSolNetworkOperation
newGetSolNetworkOperation pNsLcmOpOccId_ =
  GetSolNetworkOperation'
    { nsLcmOpOccId =
        pNsLcmOpOccId_
    }

-- | The identifier of the network operation.
getSolNetworkOperation_nsLcmOpOccId :: Lens.Lens' GetSolNetworkOperation Prelude.Text
getSolNetworkOperation_nsLcmOpOccId = Lens.lens (\GetSolNetworkOperation' {nsLcmOpOccId} -> nsLcmOpOccId) (\s@GetSolNetworkOperation' {} a -> s {nsLcmOpOccId = a} :: GetSolNetworkOperation)

instance Core.AWSRequest GetSolNetworkOperation where
  type
    AWSResponse GetSolNetworkOperation =
      GetSolNetworkOperationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSolNetworkOperationResponse'
            Prelude.<$> (x Data..?> "error")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "lcmOperationType")
            Prelude.<*> (x Data..?> "metadata")
            Prelude.<*> (x Data..?> "nsInstanceId")
            Prelude.<*> (x Data..?> "operationState")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "tasks" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
      )

instance Prelude.Hashable GetSolNetworkOperation where
  hashWithSalt _salt GetSolNetworkOperation' {..} =
    _salt `Prelude.hashWithSalt` nsLcmOpOccId

instance Prelude.NFData GetSolNetworkOperation where
  rnf GetSolNetworkOperation' {..} =
    Prelude.rnf nsLcmOpOccId

instance Data.ToHeaders GetSolNetworkOperation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetSolNetworkOperation where
  toPath GetSolNetworkOperation' {..} =
    Prelude.mconcat
      [ "/sol/nslcm/v1/ns_lcm_op_occs/",
        Data.toBS nsLcmOpOccId
      ]

instance Data.ToQuery GetSolNetworkOperation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSolNetworkOperationResponse' smart constructor.
data GetSolNetworkOperationResponse = GetSolNetworkOperationResponse'
  { -- | Error related to this specific network operation occurrence.
    error :: Prelude.Maybe ProblemDetails,
    -- | ID of this network operation occurrence.
    id :: Prelude.Maybe Prelude.Text,
    -- | Type of the operation represented by this occurrence.
    lcmOperationType :: Prelude.Maybe LcmOperationType,
    -- | Metadata of this network operation occurrence.
    metadata :: Prelude.Maybe GetSolNetworkOperationMetadata,
    -- | ID of the network operation instance.
    nsInstanceId :: Prelude.Maybe Prelude.Text,
    -- | The state of the network operation.
    operationState :: Prelude.Maybe NsLcmOperationState,
    -- | A tag is a label that you assign to an Amazon Web Services resource.
    -- Each tag consists of a key and an optional value. You can use tags to
    -- search and filter your resources or track your Amazon Web Services
    -- costs.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | All tasks associated with this operation occurrence.
    tasks :: Prelude.Maybe [GetSolNetworkOperationTaskDetails],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Network operation ARN.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolNetworkOperationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'getSolNetworkOperationResponse_error' - Error related to this specific network operation occurrence.
--
-- 'id', 'getSolNetworkOperationResponse_id' - ID of this network operation occurrence.
--
-- 'lcmOperationType', 'getSolNetworkOperationResponse_lcmOperationType' - Type of the operation represented by this occurrence.
--
-- 'metadata', 'getSolNetworkOperationResponse_metadata' - Metadata of this network operation occurrence.
--
-- 'nsInstanceId', 'getSolNetworkOperationResponse_nsInstanceId' - ID of the network operation instance.
--
-- 'operationState', 'getSolNetworkOperationResponse_operationState' - The state of the network operation.
--
-- 'tags', 'getSolNetworkOperationResponse_tags' - A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. You can use tags to
-- search and filter your resources or track your Amazon Web Services
-- costs.
--
-- 'tasks', 'getSolNetworkOperationResponse_tasks' - All tasks associated with this operation occurrence.
--
-- 'httpStatus', 'getSolNetworkOperationResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'getSolNetworkOperationResponse_arn' - Network operation ARN.
newGetSolNetworkOperationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  GetSolNetworkOperationResponse
newGetSolNetworkOperationResponse pHttpStatus_ pArn_ =
  GetSolNetworkOperationResponse'
    { error =
        Prelude.Nothing,
      id = Prelude.Nothing,
      lcmOperationType = Prelude.Nothing,
      metadata = Prelude.Nothing,
      nsInstanceId = Prelude.Nothing,
      operationState = Prelude.Nothing,
      tags = Prelude.Nothing,
      tasks = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      arn = pArn_
    }

-- | Error related to this specific network operation occurrence.
getSolNetworkOperationResponse_error :: Lens.Lens' GetSolNetworkOperationResponse (Prelude.Maybe ProblemDetails)
getSolNetworkOperationResponse_error = Lens.lens (\GetSolNetworkOperationResponse' {error} -> error) (\s@GetSolNetworkOperationResponse' {} a -> s {error = a} :: GetSolNetworkOperationResponse)

-- | ID of this network operation occurrence.
getSolNetworkOperationResponse_id :: Lens.Lens' GetSolNetworkOperationResponse (Prelude.Maybe Prelude.Text)
getSolNetworkOperationResponse_id = Lens.lens (\GetSolNetworkOperationResponse' {id} -> id) (\s@GetSolNetworkOperationResponse' {} a -> s {id = a} :: GetSolNetworkOperationResponse)

-- | Type of the operation represented by this occurrence.
getSolNetworkOperationResponse_lcmOperationType :: Lens.Lens' GetSolNetworkOperationResponse (Prelude.Maybe LcmOperationType)
getSolNetworkOperationResponse_lcmOperationType = Lens.lens (\GetSolNetworkOperationResponse' {lcmOperationType} -> lcmOperationType) (\s@GetSolNetworkOperationResponse' {} a -> s {lcmOperationType = a} :: GetSolNetworkOperationResponse)

-- | Metadata of this network operation occurrence.
getSolNetworkOperationResponse_metadata :: Lens.Lens' GetSolNetworkOperationResponse (Prelude.Maybe GetSolNetworkOperationMetadata)
getSolNetworkOperationResponse_metadata = Lens.lens (\GetSolNetworkOperationResponse' {metadata} -> metadata) (\s@GetSolNetworkOperationResponse' {} a -> s {metadata = a} :: GetSolNetworkOperationResponse)

-- | ID of the network operation instance.
getSolNetworkOperationResponse_nsInstanceId :: Lens.Lens' GetSolNetworkOperationResponse (Prelude.Maybe Prelude.Text)
getSolNetworkOperationResponse_nsInstanceId = Lens.lens (\GetSolNetworkOperationResponse' {nsInstanceId} -> nsInstanceId) (\s@GetSolNetworkOperationResponse' {} a -> s {nsInstanceId = a} :: GetSolNetworkOperationResponse)

-- | The state of the network operation.
getSolNetworkOperationResponse_operationState :: Lens.Lens' GetSolNetworkOperationResponse (Prelude.Maybe NsLcmOperationState)
getSolNetworkOperationResponse_operationState = Lens.lens (\GetSolNetworkOperationResponse' {operationState} -> operationState) (\s@GetSolNetworkOperationResponse' {} a -> s {operationState = a} :: GetSolNetworkOperationResponse)

-- | A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. You can use tags to
-- search and filter your resources or track your Amazon Web Services
-- costs.
getSolNetworkOperationResponse_tags :: Lens.Lens' GetSolNetworkOperationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getSolNetworkOperationResponse_tags = Lens.lens (\GetSolNetworkOperationResponse' {tags} -> tags) (\s@GetSolNetworkOperationResponse' {} a -> s {tags = a} :: GetSolNetworkOperationResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | All tasks associated with this operation occurrence.
getSolNetworkOperationResponse_tasks :: Lens.Lens' GetSolNetworkOperationResponse (Prelude.Maybe [GetSolNetworkOperationTaskDetails])
getSolNetworkOperationResponse_tasks = Lens.lens (\GetSolNetworkOperationResponse' {tasks} -> tasks) (\s@GetSolNetworkOperationResponse' {} a -> s {tasks = a} :: GetSolNetworkOperationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getSolNetworkOperationResponse_httpStatus :: Lens.Lens' GetSolNetworkOperationResponse Prelude.Int
getSolNetworkOperationResponse_httpStatus = Lens.lens (\GetSolNetworkOperationResponse' {httpStatus} -> httpStatus) (\s@GetSolNetworkOperationResponse' {} a -> s {httpStatus = a} :: GetSolNetworkOperationResponse)

-- | Network operation ARN.
getSolNetworkOperationResponse_arn :: Lens.Lens' GetSolNetworkOperationResponse Prelude.Text
getSolNetworkOperationResponse_arn = Lens.lens (\GetSolNetworkOperationResponse' {arn} -> arn) (\s@GetSolNetworkOperationResponse' {} a -> s {arn = a} :: GetSolNetworkOperationResponse)

instance
  Prelude.NFData
    GetSolNetworkOperationResponse
  where
  rnf GetSolNetworkOperationResponse' {..} =
    Prelude.rnf error
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lcmOperationType
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf nsInstanceId
      `Prelude.seq` Prelude.rnf operationState
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf tasks
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
