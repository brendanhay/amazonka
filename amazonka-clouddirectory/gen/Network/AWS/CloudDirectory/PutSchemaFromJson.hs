{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudDirectory.PutSchemaFromJson
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a schema to be updated using JSON upload. Only available for
-- development schemas. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_jsonformat.html#schemas_json JSON Schema Format>
-- for more information.
module Network.AWS.CloudDirectory.PutSchemaFromJson
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

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutSchemaFromJson' smart constructor.
data PutSchemaFromJson = PutSchemaFromJson'
  { -- | The ARN of the schema to update.
    schemaArn :: Prelude.Text,
    -- | The replacement JSON schema.
    document :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest PutSchemaFromJson where
  type Rs PutSchemaFromJson = PutSchemaFromJsonResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutSchemaFromJsonResponse'
            Prelude.<$> (x Prelude..?> "Arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutSchemaFromJson

instance Prelude.NFData PutSchemaFromJson

instance Prelude.ToHeaders PutSchemaFromJson where
  toHeaders PutSchemaFromJson' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Prelude.=# schemaArn]

instance Prelude.ToJSON PutSchemaFromJson where
  toJSON PutSchemaFromJson' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Document" Prelude..= document)]
      )

instance Prelude.ToPath PutSchemaFromJson where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/schema/json"

instance Prelude.ToQuery PutSchemaFromJson where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutSchemaFromJsonResponse' smart constructor.
data PutSchemaFromJsonResponse = PutSchemaFromJsonResponse'
  { -- | The ARN of the schema to update.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData PutSchemaFromJsonResponse
