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
-- Module      : Network.AWS.CloudDirectory.GetSchemaAsJson
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a JSON representation of the schema. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_jsonformat.html#schemas_json JSON Schema Format>
-- for more information.
module Network.AWS.CloudDirectory.GetSchemaAsJson
  ( -- * Creating a Request
    GetSchemaAsJson (..),
    newGetSchemaAsJson,

    -- * Request Lenses
    getSchemaAsJson_schemaArn,

    -- * Destructuring the Response
    GetSchemaAsJsonResponse (..),
    newGetSchemaAsJsonResponse,

    -- * Response Lenses
    getSchemaAsJsonResponse_name,
    getSchemaAsJsonResponse_document,
    getSchemaAsJsonResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSchemaAsJson' smart constructor.
data GetSchemaAsJson = GetSchemaAsJson'
  { -- | The ARN of the schema to retrieve.
    schemaArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSchemaAsJson' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'getSchemaAsJson_schemaArn' - The ARN of the schema to retrieve.
newGetSchemaAsJson ::
  -- | 'schemaArn'
  Core.Text ->
  GetSchemaAsJson
newGetSchemaAsJson pSchemaArn_ =
  GetSchemaAsJson' {schemaArn = pSchemaArn_}

-- | The ARN of the schema to retrieve.
getSchemaAsJson_schemaArn :: Lens.Lens' GetSchemaAsJson Core.Text
getSchemaAsJson_schemaArn = Lens.lens (\GetSchemaAsJson' {schemaArn} -> schemaArn) (\s@GetSchemaAsJson' {} a -> s {schemaArn = a} :: GetSchemaAsJson)

instance Core.AWSRequest GetSchemaAsJson where
  type
    AWSResponse GetSchemaAsJson =
      GetSchemaAsJsonResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSchemaAsJsonResponse'
            Core.<$> (x Core..?> "Name")
            Core.<*> (x Core..?> "Document")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetSchemaAsJson

instance Core.NFData GetSchemaAsJson

instance Core.ToHeaders GetSchemaAsJson where
  toHeaders GetSchemaAsJson' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# schemaArn]

instance Core.ToJSON GetSchemaAsJson where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath GetSchemaAsJson where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/schema/json"

instance Core.ToQuery GetSchemaAsJson where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetSchemaAsJsonResponse' smart constructor.
data GetSchemaAsJsonResponse = GetSchemaAsJsonResponse'
  { -- | The name of the retrieved schema.
    name :: Core.Maybe Core.Text,
    -- | The JSON representation of the schema document.
    document :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSchemaAsJsonResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getSchemaAsJsonResponse_name' - The name of the retrieved schema.
--
-- 'document', 'getSchemaAsJsonResponse_document' - The JSON representation of the schema document.
--
-- 'httpStatus', 'getSchemaAsJsonResponse_httpStatus' - The response's http status code.
newGetSchemaAsJsonResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetSchemaAsJsonResponse
newGetSchemaAsJsonResponse pHttpStatus_ =
  GetSchemaAsJsonResponse'
    { name = Core.Nothing,
      document = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the retrieved schema.
getSchemaAsJsonResponse_name :: Lens.Lens' GetSchemaAsJsonResponse (Core.Maybe Core.Text)
getSchemaAsJsonResponse_name = Lens.lens (\GetSchemaAsJsonResponse' {name} -> name) (\s@GetSchemaAsJsonResponse' {} a -> s {name = a} :: GetSchemaAsJsonResponse)

-- | The JSON representation of the schema document.
getSchemaAsJsonResponse_document :: Lens.Lens' GetSchemaAsJsonResponse (Core.Maybe Core.Text)
getSchemaAsJsonResponse_document = Lens.lens (\GetSchemaAsJsonResponse' {document} -> document) (\s@GetSchemaAsJsonResponse' {} a -> s {document = a} :: GetSchemaAsJsonResponse)

-- | The response's http status code.
getSchemaAsJsonResponse_httpStatus :: Lens.Lens' GetSchemaAsJsonResponse Core.Int
getSchemaAsJsonResponse_httpStatus = Lens.lens (\GetSchemaAsJsonResponse' {httpStatus} -> httpStatus) (\s@GetSchemaAsJsonResponse' {} a -> s {httpStatus = a} :: GetSchemaAsJsonResponse)

instance Core.NFData GetSchemaAsJsonResponse
