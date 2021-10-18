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
-- Module      : Network.AWS.AppSync.GetType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a @Type@ object.
module Network.AWS.AppSync.GetType
  ( -- * Creating a Request
    GetType (..),
    newGetType,

    -- * Request Lenses
    getType_apiId,
    getType_typeName,
    getType_format,

    -- * Destructuring the Response
    GetTypeResponse (..),
    newGetTypeResponse,

    -- * Response Lenses
    getTypeResponse_type,
    getTypeResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetType' smart constructor.
data GetType = GetType'
  { -- | The API ID.
    apiId :: Prelude.Text,
    -- | The type name.
    typeName :: Prelude.Text,
    -- | The type format: SDL or JSON.
    format :: TypeDefinitionFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'getType_apiId' - The API ID.
--
-- 'typeName', 'getType_typeName' - The type name.
--
-- 'format', 'getType_format' - The type format: SDL or JSON.
newGetType ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'typeName'
  Prelude.Text ->
  -- | 'format'
  TypeDefinitionFormat ->
  GetType
newGetType pApiId_ pTypeName_ pFormat_ =
  GetType'
    { apiId = pApiId_,
      typeName = pTypeName_,
      format = pFormat_
    }

-- | The API ID.
getType_apiId :: Lens.Lens' GetType Prelude.Text
getType_apiId = Lens.lens (\GetType' {apiId} -> apiId) (\s@GetType' {} a -> s {apiId = a} :: GetType)

-- | The type name.
getType_typeName :: Lens.Lens' GetType Prelude.Text
getType_typeName = Lens.lens (\GetType' {typeName} -> typeName) (\s@GetType' {} a -> s {typeName = a} :: GetType)

-- | The type format: SDL or JSON.
getType_format :: Lens.Lens' GetType TypeDefinitionFormat
getType_format = Lens.lens (\GetType' {format} -> format) (\s@GetType' {} a -> s {format = a} :: GetType)

instance Core.AWSRequest GetType where
  type AWSResponse GetType = GetTypeResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTypeResponse'
            Prelude.<$> (x Core..?> "type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetType

instance Prelude.NFData GetType

instance Core.ToHeaders GetType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetType where
  toPath GetType' {..} =
    Prelude.mconcat
      [ "/v1/apis/",
        Core.toBS apiId,
        "/types/",
        Core.toBS typeName
      ]

instance Core.ToQuery GetType where
  toQuery GetType' {..} =
    Prelude.mconcat ["format" Core.=: format]

-- | /See:/ 'newGetTypeResponse' smart constructor.
data GetTypeResponse = GetTypeResponse'
  { -- | The @Type@ object.
    type' :: Prelude.Maybe Type,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'getTypeResponse_type' - The @Type@ object.
--
-- 'httpStatus', 'getTypeResponse_httpStatus' - The response's http status code.
newGetTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTypeResponse
newGetTypeResponse pHttpStatus_ =
  GetTypeResponse'
    { type' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @Type@ object.
getTypeResponse_type :: Lens.Lens' GetTypeResponse (Prelude.Maybe Type)
getTypeResponse_type = Lens.lens (\GetTypeResponse' {type'} -> type') (\s@GetTypeResponse' {} a -> s {type' = a} :: GetTypeResponse)

-- | The response's http status code.
getTypeResponse_httpStatus :: Lens.Lens' GetTypeResponse Prelude.Int
getTypeResponse_httpStatus = Lens.lens (\GetTypeResponse' {httpStatus} -> httpStatus) (\s@GetTypeResponse' {} a -> s {httpStatus = a} :: GetTypeResponse)

instance Prelude.NFData GetTypeResponse
