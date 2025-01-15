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
-- Module      : Amazonka.CloudDirectory.GetSchemaAsJson
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a JSON representation of the schema. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_jsonformat.html#schemas_json JSON Schema Format>
-- for more information.
module Amazonka.CloudDirectory.GetSchemaAsJson
  ( -- * Creating a Request
    GetSchemaAsJson (..),
    newGetSchemaAsJson,

    -- * Request Lenses
    getSchemaAsJson_schemaArn,

    -- * Destructuring the Response
    GetSchemaAsJsonResponse (..),
    newGetSchemaAsJsonResponse,

    -- * Response Lenses
    getSchemaAsJsonResponse_document,
    getSchemaAsJsonResponse_name,
    getSchemaAsJsonResponse_httpStatus,
  )
where

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSchemaAsJson' smart constructor.
data GetSchemaAsJson = GetSchemaAsJson'
  { -- | The ARN of the schema to retrieve.
    schemaArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetSchemaAsJson
newGetSchemaAsJson pSchemaArn_ =
  GetSchemaAsJson' {schemaArn = pSchemaArn_}

-- | The ARN of the schema to retrieve.
getSchemaAsJson_schemaArn :: Lens.Lens' GetSchemaAsJson Prelude.Text
getSchemaAsJson_schemaArn = Lens.lens (\GetSchemaAsJson' {schemaArn} -> schemaArn) (\s@GetSchemaAsJson' {} a -> s {schemaArn = a} :: GetSchemaAsJson)

instance Core.AWSRequest GetSchemaAsJson where
  type
    AWSResponse GetSchemaAsJson =
      GetSchemaAsJsonResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSchemaAsJsonResponse'
            Prelude.<$> (x Data..?> "Document")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSchemaAsJson where
  hashWithSalt _salt GetSchemaAsJson' {..} =
    _salt `Prelude.hashWithSalt` schemaArn

instance Prelude.NFData GetSchemaAsJson where
  rnf GetSchemaAsJson' {..} = Prelude.rnf schemaArn

instance Data.ToHeaders GetSchemaAsJson where
  toHeaders GetSchemaAsJson' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Data.=# schemaArn]

instance Data.ToJSON GetSchemaAsJson where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetSchemaAsJson where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/schema/json"

instance Data.ToQuery GetSchemaAsJson where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSchemaAsJsonResponse' smart constructor.
data GetSchemaAsJsonResponse = GetSchemaAsJsonResponse'
  { -- | The JSON representation of the schema document.
    document :: Prelude.Maybe Prelude.Text,
    -- | The name of the retrieved schema.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSchemaAsJsonResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'document', 'getSchemaAsJsonResponse_document' - The JSON representation of the schema document.
--
-- 'name', 'getSchemaAsJsonResponse_name' - The name of the retrieved schema.
--
-- 'httpStatus', 'getSchemaAsJsonResponse_httpStatus' - The response's http status code.
newGetSchemaAsJsonResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSchemaAsJsonResponse
newGetSchemaAsJsonResponse pHttpStatus_ =
  GetSchemaAsJsonResponse'
    { document =
        Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The JSON representation of the schema document.
getSchemaAsJsonResponse_document :: Lens.Lens' GetSchemaAsJsonResponse (Prelude.Maybe Prelude.Text)
getSchemaAsJsonResponse_document = Lens.lens (\GetSchemaAsJsonResponse' {document} -> document) (\s@GetSchemaAsJsonResponse' {} a -> s {document = a} :: GetSchemaAsJsonResponse)

-- | The name of the retrieved schema.
getSchemaAsJsonResponse_name :: Lens.Lens' GetSchemaAsJsonResponse (Prelude.Maybe Prelude.Text)
getSchemaAsJsonResponse_name = Lens.lens (\GetSchemaAsJsonResponse' {name} -> name) (\s@GetSchemaAsJsonResponse' {} a -> s {name = a} :: GetSchemaAsJsonResponse)

-- | The response's http status code.
getSchemaAsJsonResponse_httpStatus :: Lens.Lens' GetSchemaAsJsonResponse Prelude.Int
getSchemaAsJsonResponse_httpStatus = Lens.lens (\GetSchemaAsJsonResponse' {httpStatus} -> httpStatus) (\s@GetSchemaAsJsonResponse' {} a -> s {httpStatus = a} :: GetSchemaAsJsonResponse)

instance Prelude.NFData GetSchemaAsJsonResponse where
  rnf GetSchemaAsJsonResponse' {..} =
    Prelude.rnf document `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf httpStatus
