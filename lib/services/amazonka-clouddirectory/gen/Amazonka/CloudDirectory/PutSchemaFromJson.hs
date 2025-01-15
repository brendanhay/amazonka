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
-- Module      : Amazonka.CloudDirectory.PutSchemaFromJson
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a schema to be updated using JSON upload. Only available for
-- development schemas. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_jsonformat.html#schemas_json JSON Schema Format>
-- for more information.
module Amazonka.CloudDirectory.PutSchemaFromJson
  ( -- * Creating a Request
    PutSchemaFromJson (..),
    newPutSchemaFromJson,

    -- * Request Lenses
    putSchemaFromJson_schemaArn,
    putSchemaFromJson_document,

    -- * Destructuring the Response
    PutSchemaFromJsonResponse (..),
    newPutSchemaFromJsonResponse,

    -- * Response Lenses
    putSchemaFromJsonResponse_arn,
    putSchemaFromJsonResponse_httpStatus,
  )
where

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutSchemaFromJson' smart constructor.
data PutSchemaFromJson = PutSchemaFromJson'
  { -- | The ARN of the schema to update.
    schemaArn :: Prelude.Text,
    -- | The replacement JSON schema.
    document :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSchemaFromJson' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'putSchemaFromJson_schemaArn' - The ARN of the schema to update.
--
-- 'document', 'putSchemaFromJson_document' - The replacement JSON schema.
newPutSchemaFromJson ::
  -- | 'schemaArn'
  Prelude.Text ->
  -- | 'document'
  Prelude.Text ->
  PutSchemaFromJson
newPutSchemaFromJson pSchemaArn_ pDocument_ =
  PutSchemaFromJson'
    { schemaArn = pSchemaArn_,
      document = pDocument_
    }

-- | The ARN of the schema to update.
putSchemaFromJson_schemaArn :: Lens.Lens' PutSchemaFromJson Prelude.Text
putSchemaFromJson_schemaArn = Lens.lens (\PutSchemaFromJson' {schemaArn} -> schemaArn) (\s@PutSchemaFromJson' {} a -> s {schemaArn = a} :: PutSchemaFromJson)

-- | The replacement JSON schema.
putSchemaFromJson_document :: Lens.Lens' PutSchemaFromJson Prelude.Text
putSchemaFromJson_document = Lens.lens (\PutSchemaFromJson' {document} -> document) (\s@PutSchemaFromJson' {} a -> s {document = a} :: PutSchemaFromJson)

instance Core.AWSRequest PutSchemaFromJson where
  type
    AWSResponse PutSchemaFromJson =
      PutSchemaFromJsonResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutSchemaFromJsonResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutSchemaFromJson where
  hashWithSalt _salt PutSchemaFromJson' {..} =
    _salt
      `Prelude.hashWithSalt` schemaArn
      `Prelude.hashWithSalt` document

instance Prelude.NFData PutSchemaFromJson where
  rnf PutSchemaFromJson' {..} =
    Prelude.rnf schemaArn `Prelude.seq`
      Prelude.rnf document

instance Data.ToHeaders PutSchemaFromJson where
  toHeaders PutSchemaFromJson' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Data.=# schemaArn]

instance Data.ToJSON PutSchemaFromJson where
  toJSON PutSchemaFromJson' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Document" Data..= document)]
      )

instance Data.ToPath PutSchemaFromJson where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/schema/json"

instance Data.ToQuery PutSchemaFromJson where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutSchemaFromJsonResponse' smart constructor.
data PutSchemaFromJsonResponse = PutSchemaFromJsonResponse'
  { -- | The ARN of the schema to update.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSchemaFromJsonResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'putSchemaFromJsonResponse_arn' - The ARN of the schema to update.
--
-- 'httpStatus', 'putSchemaFromJsonResponse_httpStatus' - The response's http status code.
newPutSchemaFromJsonResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutSchemaFromJsonResponse
newPutSchemaFromJsonResponse pHttpStatus_ =
  PutSchemaFromJsonResponse'
    { arn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the schema to update.
putSchemaFromJsonResponse_arn :: Lens.Lens' PutSchemaFromJsonResponse (Prelude.Maybe Prelude.Text)
putSchemaFromJsonResponse_arn = Lens.lens (\PutSchemaFromJsonResponse' {arn} -> arn) (\s@PutSchemaFromJsonResponse' {} a -> s {arn = a} :: PutSchemaFromJsonResponse)

-- | The response's http status code.
putSchemaFromJsonResponse_httpStatus :: Lens.Lens' PutSchemaFromJsonResponse Prelude.Int
putSchemaFromJsonResponse_httpStatus = Lens.lens (\PutSchemaFromJsonResponse' {httpStatus} -> httpStatus) (\s@PutSchemaFromJsonResponse' {} a -> s {httpStatus = a} :: PutSchemaFromJsonResponse)

instance Prelude.NFData PutSchemaFromJsonResponse where
  rnf PutSchemaFromJsonResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf httpStatus
