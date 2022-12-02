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
-- Module      : Amazonka.AppSync.GetIntrospectionSchema
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the introspection schema for a GraphQL API.
module Amazonka.AppSync.GetIntrospectionSchema
  ( -- * Creating a Request
    GetIntrospectionSchema (..),
    newGetIntrospectionSchema,

    -- * Request Lenses
    getIntrospectionSchema_includeDirectives,
    getIntrospectionSchema_apiId,
    getIntrospectionSchema_format,

    -- * Destructuring the Response
    GetIntrospectionSchemaResponse (..),
    newGetIntrospectionSchemaResponse,

    -- * Response Lenses
    getIntrospectionSchemaResponse_schema,
    getIntrospectionSchemaResponse_httpStatus,
  )
where

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetIntrospectionSchema' smart constructor.
data GetIntrospectionSchema = GetIntrospectionSchema'
  { -- | A flag that specifies whether the schema introspection should contain
    -- directives.
    includeDirectives :: Prelude.Maybe Prelude.Bool,
    -- | The API ID.
    apiId :: Prelude.Text,
    -- | The schema format: SDL or JSON.
    format :: OutputType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIntrospectionSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeDirectives', 'getIntrospectionSchema_includeDirectives' - A flag that specifies whether the schema introspection should contain
-- directives.
--
-- 'apiId', 'getIntrospectionSchema_apiId' - The API ID.
--
-- 'format', 'getIntrospectionSchema_format' - The schema format: SDL or JSON.
newGetIntrospectionSchema ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'format'
  OutputType ->
  GetIntrospectionSchema
newGetIntrospectionSchema pApiId_ pFormat_ =
  GetIntrospectionSchema'
    { includeDirectives =
        Prelude.Nothing,
      apiId = pApiId_,
      format = pFormat_
    }

-- | A flag that specifies whether the schema introspection should contain
-- directives.
getIntrospectionSchema_includeDirectives :: Lens.Lens' GetIntrospectionSchema (Prelude.Maybe Prelude.Bool)
getIntrospectionSchema_includeDirectives = Lens.lens (\GetIntrospectionSchema' {includeDirectives} -> includeDirectives) (\s@GetIntrospectionSchema' {} a -> s {includeDirectives = a} :: GetIntrospectionSchema)

-- | The API ID.
getIntrospectionSchema_apiId :: Lens.Lens' GetIntrospectionSchema Prelude.Text
getIntrospectionSchema_apiId = Lens.lens (\GetIntrospectionSchema' {apiId} -> apiId) (\s@GetIntrospectionSchema' {} a -> s {apiId = a} :: GetIntrospectionSchema)

-- | The schema format: SDL or JSON.
getIntrospectionSchema_format :: Lens.Lens' GetIntrospectionSchema OutputType
getIntrospectionSchema_format = Lens.lens (\GetIntrospectionSchema' {format} -> format) (\s@GetIntrospectionSchema' {} a -> s {format = a} :: GetIntrospectionSchema)

instance Core.AWSRequest GetIntrospectionSchema where
  type
    AWSResponse GetIntrospectionSchema =
      GetIntrospectionSchemaResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveBytes
      ( \s h x ->
          GetIntrospectionSchemaResponse'
            Prelude.<$> (Prelude.pure (Prelude.Just (Prelude.coerce x)))
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetIntrospectionSchema where
  hashWithSalt _salt GetIntrospectionSchema' {..} =
    _salt `Prelude.hashWithSalt` includeDirectives
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` format

instance Prelude.NFData GetIntrospectionSchema where
  rnf GetIntrospectionSchema' {..} =
    Prelude.rnf includeDirectives
      `Prelude.seq` Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf format

instance Data.ToHeaders GetIntrospectionSchema where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetIntrospectionSchema where
  toPath GetIntrospectionSchema' {..} =
    Prelude.mconcat
      ["/v1/apis/", Data.toBS apiId, "/schema"]

instance Data.ToQuery GetIntrospectionSchema where
  toQuery GetIntrospectionSchema' {..} =
    Prelude.mconcat
      [ "includeDirectives" Data.=: includeDirectives,
        "format" Data.=: format
      ]

-- | /See:/ 'newGetIntrospectionSchemaResponse' smart constructor.
data GetIntrospectionSchemaResponse = GetIntrospectionSchemaResponse'
  { -- | The schema, in GraphQL Schema Definition Language (SDL) format.
    --
    -- For more information, see the
    -- <http://graphql.org/learn/schema/ GraphQL SDL documentation>.
    schema :: Prelude.Maybe Prelude.ByteString,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIntrospectionSchemaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schema', 'getIntrospectionSchemaResponse_schema' - The schema, in GraphQL Schema Definition Language (SDL) format.
--
-- For more information, see the
-- <http://graphql.org/learn/schema/ GraphQL SDL documentation>.
--
-- 'httpStatus', 'getIntrospectionSchemaResponse_httpStatus' - The response's http status code.
newGetIntrospectionSchemaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIntrospectionSchemaResponse
newGetIntrospectionSchemaResponse pHttpStatus_ =
  GetIntrospectionSchemaResponse'
    { schema =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The schema, in GraphQL Schema Definition Language (SDL) format.
--
-- For more information, see the
-- <http://graphql.org/learn/schema/ GraphQL SDL documentation>.
getIntrospectionSchemaResponse_schema :: Lens.Lens' GetIntrospectionSchemaResponse (Prelude.Maybe Prelude.ByteString)
getIntrospectionSchemaResponse_schema = Lens.lens (\GetIntrospectionSchemaResponse' {schema} -> schema) (\s@GetIntrospectionSchemaResponse' {} a -> s {schema = a} :: GetIntrospectionSchemaResponse)

-- | The response's http status code.
getIntrospectionSchemaResponse_httpStatus :: Lens.Lens' GetIntrospectionSchemaResponse Prelude.Int
getIntrospectionSchemaResponse_httpStatus = Lens.lens (\GetIntrospectionSchemaResponse' {httpStatus} -> httpStatus) (\s@GetIntrospectionSchemaResponse' {} a -> s {httpStatus = a} :: GetIntrospectionSchemaResponse)

instance
  Prelude.NFData
    GetIntrospectionSchemaResponse
  where
  rnf GetIntrospectionSchemaResponse' {..} =
    Prelude.rnf schema
      `Prelude.seq` Prelude.rnf httpStatus
