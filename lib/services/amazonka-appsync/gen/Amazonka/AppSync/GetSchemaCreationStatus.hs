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
-- Module      : Amazonka.AppSync.GetSchemaCreationStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current status of a schema creation operation.
module Amazonka.AppSync.GetSchemaCreationStatus
  ( -- * Creating a Request
    GetSchemaCreationStatus (..),
    newGetSchemaCreationStatus,

    -- * Request Lenses
    getSchemaCreationStatus_apiId,

    -- * Destructuring the Response
    GetSchemaCreationStatusResponse (..),
    newGetSchemaCreationStatusResponse,

    -- * Response Lenses
    getSchemaCreationStatusResponse_status,
    getSchemaCreationStatusResponse_details,
    getSchemaCreationStatusResponse_httpStatus,
  )
where

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSchemaCreationStatus' smart constructor.
data GetSchemaCreationStatus = GetSchemaCreationStatus'
  { -- | The API ID.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSchemaCreationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'getSchemaCreationStatus_apiId' - The API ID.
newGetSchemaCreationStatus ::
  -- | 'apiId'
  Prelude.Text ->
  GetSchemaCreationStatus
newGetSchemaCreationStatus pApiId_ =
  GetSchemaCreationStatus' {apiId = pApiId_}

-- | The API ID.
getSchemaCreationStatus_apiId :: Lens.Lens' GetSchemaCreationStatus Prelude.Text
getSchemaCreationStatus_apiId = Lens.lens (\GetSchemaCreationStatus' {apiId} -> apiId) (\s@GetSchemaCreationStatus' {} a -> s {apiId = a} :: GetSchemaCreationStatus)

instance Core.AWSRequest GetSchemaCreationStatus where
  type
    AWSResponse GetSchemaCreationStatus =
      GetSchemaCreationStatusResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSchemaCreationStatusResponse'
            Prelude.<$> (x Data..?> "status")
            Prelude.<*> (x Data..?> "details")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSchemaCreationStatus where
  hashWithSalt _salt GetSchemaCreationStatus' {..} =
    _salt `Prelude.hashWithSalt` apiId

instance Prelude.NFData GetSchemaCreationStatus where
  rnf GetSchemaCreationStatus' {..} = Prelude.rnf apiId

instance Data.ToHeaders GetSchemaCreationStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetSchemaCreationStatus where
  toPath GetSchemaCreationStatus' {..} =
    Prelude.mconcat
      ["/v1/apis/", Data.toBS apiId, "/schemacreation"]

instance Data.ToQuery GetSchemaCreationStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSchemaCreationStatusResponse' smart constructor.
data GetSchemaCreationStatusResponse = GetSchemaCreationStatusResponse'
  { -- | The current state of the schema (PROCESSING, FAILED, SUCCESS, or
    -- NOT_APPLICABLE). When the schema is in the ACTIVE state, you can add
    -- data.
    status :: Prelude.Maybe SchemaStatus,
    -- | Detailed information about the status of the schema creation operation.
    details :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSchemaCreationStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'getSchemaCreationStatusResponse_status' - The current state of the schema (PROCESSING, FAILED, SUCCESS, or
-- NOT_APPLICABLE). When the schema is in the ACTIVE state, you can add
-- data.
--
-- 'details', 'getSchemaCreationStatusResponse_details' - Detailed information about the status of the schema creation operation.
--
-- 'httpStatus', 'getSchemaCreationStatusResponse_httpStatus' - The response's http status code.
newGetSchemaCreationStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSchemaCreationStatusResponse
newGetSchemaCreationStatusResponse pHttpStatus_ =
  GetSchemaCreationStatusResponse'
    { status =
        Prelude.Nothing,
      details = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current state of the schema (PROCESSING, FAILED, SUCCESS, or
-- NOT_APPLICABLE). When the schema is in the ACTIVE state, you can add
-- data.
getSchemaCreationStatusResponse_status :: Lens.Lens' GetSchemaCreationStatusResponse (Prelude.Maybe SchemaStatus)
getSchemaCreationStatusResponse_status = Lens.lens (\GetSchemaCreationStatusResponse' {status} -> status) (\s@GetSchemaCreationStatusResponse' {} a -> s {status = a} :: GetSchemaCreationStatusResponse)

-- | Detailed information about the status of the schema creation operation.
getSchemaCreationStatusResponse_details :: Lens.Lens' GetSchemaCreationStatusResponse (Prelude.Maybe Prelude.Text)
getSchemaCreationStatusResponse_details = Lens.lens (\GetSchemaCreationStatusResponse' {details} -> details) (\s@GetSchemaCreationStatusResponse' {} a -> s {details = a} :: GetSchemaCreationStatusResponse)

-- | The response's http status code.
getSchemaCreationStatusResponse_httpStatus :: Lens.Lens' GetSchemaCreationStatusResponse Prelude.Int
getSchemaCreationStatusResponse_httpStatus = Lens.lens (\GetSchemaCreationStatusResponse' {httpStatus} -> httpStatus) (\s@GetSchemaCreationStatusResponse' {} a -> s {httpStatus = a} :: GetSchemaCreationStatusResponse)

instance
  Prelude.NFData
    GetSchemaCreationStatusResponse
  where
  rnf GetSchemaCreationStatusResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf details
      `Prelude.seq` Prelude.rnf httpStatus
