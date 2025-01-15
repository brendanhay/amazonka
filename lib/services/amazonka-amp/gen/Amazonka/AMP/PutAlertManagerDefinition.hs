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
-- Module      : Amazonka.AMP.PutAlertManagerDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an alert manager definition.
module Amazonka.AMP.PutAlertManagerDefinition
  ( -- * Creating a Request
    PutAlertManagerDefinition (..),
    newPutAlertManagerDefinition,

    -- * Request Lenses
    putAlertManagerDefinition_clientToken,
    putAlertManagerDefinition_data,
    putAlertManagerDefinition_workspaceId,

    -- * Destructuring the Response
    PutAlertManagerDefinitionResponse (..),
    newPutAlertManagerDefinitionResponse,

    -- * Response Lenses
    putAlertManagerDefinitionResponse_httpStatus,
    putAlertManagerDefinitionResponse_status,
  )
where

import Amazonka.AMP.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a PutAlertManagerDefinition operation.
--
-- /See:/ 'newPutAlertManagerDefinition' smart constructor.
data PutAlertManagerDefinition = PutAlertManagerDefinition'
  { -- | Optional, unique, case-sensitive, user-provided identifier to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The alert manager definition data.
    data' :: Data.Base64,
    -- | The ID of the workspace in which to update the alert manager definition.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAlertManagerDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'putAlertManagerDefinition_clientToken' - Optional, unique, case-sensitive, user-provided identifier to ensure the
-- idempotency of the request.
--
-- 'data'', 'putAlertManagerDefinition_data' - The alert manager definition data.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'workspaceId', 'putAlertManagerDefinition_workspaceId' - The ID of the workspace in which to update the alert manager definition.
newPutAlertManagerDefinition ::
  -- | 'data''
  Prelude.ByteString ->
  -- | 'workspaceId'
  Prelude.Text ->
  PutAlertManagerDefinition
newPutAlertManagerDefinition pData_ pWorkspaceId_ =
  PutAlertManagerDefinition'
    { clientToken =
        Prelude.Nothing,
      data' = Data._Base64 Lens.# pData_,
      workspaceId = pWorkspaceId_
    }

-- | Optional, unique, case-sensitive, user-provided identifier to ensure the
-- idempotency of the request.
putAlertManagerDefinition_clientToken :: Lens.Lens' PutAlertManagerDefinition (Prelude.Maybe Prelude.Text)
putAlertManagerDefinition_clientToken = Lens.lens (\PutAlertManagerDefinition' {clientToken} -> clientToken) (\s@PutAlertManagerDefinition' {} a -> s {clientToken = a} :: PutAlertManagerDefinition)

-- | The alert manager definition data.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
putAlertManagerDefinition_data :: Lens.Lens' PutAlertManagerDefinition Prelude.ByteString
putAlertManagerDefinition_data = Lens.lens (\PutAlertManagerDefinition' {data'} -> data') (\s@PutAlertManagerDefinition' {} a -> s {data' = a} :: PutAlertManagerDefinition) Prelude.. Data._Base64

-- | The ID of the workspace in which to update the alert manager definition.
putAlertManagerDefinition_workspaceId :: Lens.Lens' PutAlertManagerDefinition Prelude.Text
putAlertManagerDefinition_workspaceId = Lens.lens (\PutAlertManagerDefinition' {workspaceId} -> workspaceId) (\s@PutAlertManagerDefinition' {} a -> s {workspaceId = a} :: PutAlertManagerDefinition)

instance Core.AWSRequest PutAlertManagerDefinition where
  type
    AWSResponse PutAlertManagerDefinition =
      PutAlertManagerDefinitionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutAlertManagerDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "status")
      )

instance Prelude.Hashable PutAlertManagerDefinition where
  hashWithSalt _salt PutAlertManagerDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` data'
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData PutAlertManagerDefinition where
  rnf PutAlertManagerDefinition' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf data' `Prelude.seq`
        Prelude.rnf workspaceId

instance Data.ToHeaders PutAlertManagerDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutAlertManagerDefinition where
  toJSON PutAlertManagerDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("data" Data..= data')
          ]
      )

instance Data.ToPath PutAlertManagerDefinition where
  toPath PutAlertManagerDefinition' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/alertmanager/definition"
      ]

instance Data.ToQuery PutAlertManagerDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a PutAlertManagerDefinition operation.
--
-- /See:/ 'newPutAlertManagerDefinitionResponse' smart constructor.
data PutAlertManagerDefinitionResponse = PutAlertManagerDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of alert manager definition.
    status :: AlertManagerDefinitionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAlertManagerDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putAlertManagerDefinitionResponse_httpStatus' - The response's http status code.
--
-- 'status', 'putAlertManagerDefinitionResponse_status' - The status of alert manager definition.
newPutAlertManagerDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'status'
  AlertManagerDefinitionStatus ->
  PutAlertManagerDefinitionResponse
newPutAlertManagerDefinitionResponse
  pHttpStatus_
  pStatus_ =
    PutAlertManagerDefinitionResponse'
      { httpStatus =
          pHttpStatus_,
        status = pStatus_
      }

-- | The response's http status code.
putAlertManagerDefinitionResponse_httpStatus :: Lens.Lens' PutAlertManagerDefinitionResponse Prelude.Int
putAlertManagerDefinitionResponse_httpStatus = Lens.lens (\PutAlertManagerDefinitionResponse' {httpStatus} -> httpStatus) (\s@PutAlertManagerDefinitionResponse' {} a -> s {httpStatus = a} :: PutAlertManagerDefinitionResponse)

-- | The status of alert manager definition.
putAlertManagerDefinitionResponse_status :: Lens.Lens' PutAlertManagerDefinitionResponse AlertManagerDefinitionStatus
putAlertManagerDefinitionResponse_status = Lens.lens (\PutAlertManagerDefinitionResponse' {status} -> status) (\s@PutAlertManagerDefinitionResponse' {} a -> s {status = a} :: PutAlertManagerDefinitionResponse)

instance
  Prelude.NFData
    PutAlertManagerDefinitionResponse
  where
  rnf PutAlertManagerDefinitionResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf status
