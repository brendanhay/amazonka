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
-- Module      : Network.AWS.AppSync.GetIntrospectionSchema
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the introspection schema for a GraphQL API.
module Network.AWS.AppSync.GetIntrospectionSchema
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

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetIntrospectionSchema' smart constructor.
data GetIntrospectionSchema = GetIntrospectionSchema'
  { -- | A flag that specifies whether the schema introspection should contain
    -- directives.
    includeDirectives :: Core.Maybe Core.Bool,
    -- | The API ID.
    apiId :: Core.Text,
    -- | The schema format: SDL or JSON.
    format :: OutputType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'format'
  OutputType ->
  GetIntrospectionSchema
newGetIntrospectionSchema pApiId_ pFormat_ =
  GetIntrospectionSchema'
    { includeDirectives =
        Core.Nothing,
      apiId = pApiId_,
      format = pFormat_
    }

-- | A flag that specifies whether the schema introspection should contain
-- directives.
getIntrospectionSchema_includeDirectives :: Lens.Lens' GetIntrospectionSchema (Core.Maybe Core.Bool)
getIntrospectionSchema_includeDirectives = Lens.lens (\GetIntrospectionSchema' {includeDirectives} -> includeDirectives) (\s@GetIntrospectionSchema' {} a -> s {includeDirectives = a} :: GetIntrospectionSchema)

-- | The API ID.
getIntrospectionSchema_apiId :: Lens.Lens' GetIntrospectionSchema Core.Text
getIntrospectionSchema_apiId = Lens.lens (\GetIntrospectionSchema' {apiId} -> apiId) (\s@GetIntrospectionSchema' {} a -> s {apiId = a} :: GetIntrospectionSchema)

-- | The schema format: SDL or JSON.
getIntrospectionSchema_format :: Lens.Lens' GetIntrospectionSchema OutputType
getIntrospectionSchema_format = Lens.lens (\GetIntrospectionSchema' {format} -> format) (\s@GetIntrospectionSchema' {} a -> s {format = a} :: GetIntrospectionSchema)

instance Core.AWSRequest GetIntrospectionSchema where
  type
    AWSResponse GetIntrospectionSchema =
      GetIntrospectionSchemaResponse
  request = Request.get defaultService
  response =
    Response.receiveBytes
      ( \s h x ->
          GetIntrospectionSchemaResponse'
            Core.<$> (Core.pure (Core.Just x))
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetIntrospectionSchema

instance Core.NFData GetIntrospectionSchema

instance Core.ToHeaders GetIntrospectionSchema where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetIntrospectionSchema where
  toPath GetIntrospectionSchema' {..} =
    Core.mconcat
      ["/v1/apis/", Core.toBS apiId, "/schema"]

instance Core.ToQuery GetIntrospectionSchema where
  toQuery GetIntrospectionSchema' {..} =
    Core.mconcat
      [ "includeDirectives" Core.=: includeDirectives,
        "format" Core.=: format
      ]

-- | /See:/ 'newGetIntrospectionSchemaResponse' smart constructor.
data GetIntrospectionSchemaResponse = GetIntrospectionSchemaResponse'
  { -- | The schema, in GraphQL Schema Definition Language (SDL) format.
    --
    -- For more information, see the
    -- <http://graphql.org/learn/schema/ GraphQL SDL documentation>.
    schema :: Core.Maybe Core.ByteString,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetIntrospectionSchemaResponse
newGetIntrospectionSchemaResponse pHttpStatus_ =
  GetIntrospectionSchemaResponse'
    { schema =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The schema, in GraphQL Schema Definition Language (SDL) format.
--
-- For more information, see the
-- <http://graphql.org/learn/schema/ GraphQL SDL documentation>.
getIntrospectionSchemaResponse_schema :: Lens.Lens' GetIntrospectionSchemaResponse (Core.Maybe Core.ByteString)
getIntrospectionSchemaResponse_schema = Lens.lens (\GetIntrospectionSchemaResponse' {schema} -> schema) (\s@GetIntrospectionSchemaResponse' {} a -> s {schema = a} :: GetIntrospectionSchemaResponse)

-- | The response's http status code.
getIntrospectionSchemaResponse_httpStatus :: Lens.Lens' GetIntrospectionSchemaResponse Core.Int
getIntrospectionSchemaResponse_httpStatus = Lens.lens (\GetIntrospectionSchemaResponse' {httpStatus} -> httpStatus) (\s@GetIntrospectionSchemaResponse' {} a -> s {httpStatus = a} :: GetIntrospectionSchemaResponse)

instance Core.NFData GetIntrospectionSchemaResponse
