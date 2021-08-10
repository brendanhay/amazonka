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
-- Module      : Network.AWS.AppSync.CreateType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @Type@ object.
module Network.AWS.AppSync.CreateType
  ( -- * Creating a Request
    CreateType (..),
    newCreateType,

    -- * Request Lenses
    createType_apiId,
    createType_definition,
    createType_format,

    -- * Destructuring the Response
    CreateTypeResponse (..),
    newCreateTypeResponse,

    -- * Response Lenses
    createTypeResponse_type,
    createTypeResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateType' smart constructor.
data CreateType = CreateType'
  { -- | The API ID.
    apiId :: Prelude.Text,
    -- | The type definition, in GraphQL Schema Definition Language (SDL) format.
    --
    -- For more information, see the
    -- <http://graphql.org/learn/schema/ GraphQL SDL documentation>.
    definition :: Prelude.Text,
    -- | The type format: SDL or JSON.
    format :: TypeDefinitionFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'createType_apiId' - The API ID.
--
-- 'definition', 'createType_definition' - The type definition, in GraphQL Schema Definition Language (SDL) format.
--
-- For more information, see the
-- <http://graphql.org/learn/schema/ GraphQL SDL documentation>.
--
-- 'format', 'createType_format' - The type format: SDL or JSON.
newCreateType ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'definition'
  Prelude.Text ->
  -- | 'format'
  TypeDefinitionFormat ->
  CreateType
newCreateType pApiId_ pDefinition_ pFormat_ =
  CreateType'
    { apiId = pApiId_,
      definition = pDefinition_,
      format = pFormat_
    }

-- | The API ID.
createType_apiId :: Lens.Lens' CreateType Prelude.Text
createType_apiId = Lens.lens (\CreateType' {apiId} -> apiId) (\s@CreateType' {} a -> s {apiId = a} :: CreateType)

-- | The type definition, in GraphQL Schema Definition Language (SDL) format.
--
-- For more information, see the
-- <http://graphql.org/learn/schema/ GraphQL SDL documentation>.
createType_definition :: Lens.Lens' CreateType Prelude.Text
createType_definition = Lens.lens (\CreateType' {definition} -> definition) (\s@CreateType' {} a -> s {definition = a} :: CreateType)

-- | The type format: SDL or JSON.
createType_format :: Lens.Lens' CreateType TypeDefinitionFormat
createType_format = Lens.lens (\CreateType' {format} -> format) (\s@CreateType' {} a -> s {format = a} :: CreateType)

instance Core.AWSRequest CreateType where
  type AWSResponse CreateType = CreateTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTypeResponse'
            Prelude.<$> (x Core..?> "type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateType

instance Prelude.NFData CreateType

instance Core.ToHeaders CreateType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateType where
  toJSON CreateType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("definition" Core..= definition),
            Prelude.Just ("format" Core..= format)
          ]
      )

instance Core.ToPath CreateType where
  toPath CreateType' {..} =
    Prelude.mconcat
      ["/v1/apis/", Core.toBS apiId, "/types"]

instance Core.ToQuery CreateType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTypeResponse' smart constructor.
data CreateTypeResponse = CreateTypeResponse'
  { -- | The @Type@ object.
    type' :: Prelude.Maybe Type,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'createTypeResponse_type' - The @Type@ object.
--
-- 'httpStatus', 'createTypeResponse_httpStatus' - The response's http status code.
newCreateTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTypeResponse
newCreateTypeResponse pHttpStatus_ =
  CreateTypeResponse'
    { type' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @Type@ object.
createTypeResponse_type :: Lens.Lens' CreateTypeResponse (Prelude.Maybe Type)
createTypeResponse_type = Lens.lens (\CreateTypeResponse' {type'} -> type') (\s@CreateTypeResponse' {} a -> s {type' = a} :: CreateTypeResponse)

-- | The response's http status code.
createTypeResponse_httpStatus :: Lens.Lens' CreateTypeResponse Prelude.Int
createTypeResponse_httpStatus = Lens.lens (\CreateTypeResponse' {httpStatus} -> httpStatus) (\s@CreateTypeResponse' {} a -> s {httpStatus = a} :: CreateTypeResponse)

instance Prelude.NFData CreateTypeResponse
