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
-- Module      : Amazonka.AppSync.GetType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a @Type@ object.
module Amazonka.AppSync.GetType
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

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTypeResponse'
            Prelude.<$> (x Data..?> "type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetType where
  hashWithSalt _salt GetType' {..} =
    _salt `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` typeName
      `Prelude.hashWithSalt` format

instance Prelude.NFData GetType where
  rnf GetType' {..} =
    Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf format

instance Data.ToHeaders GetType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetType where
  toPath GetType' {..} =
    Prelude.mconcat
      [ "/v1/apis/",
        Data.toBS apiId,
        "/types/",
        Data.toBS typeName
      ]

instance Data.ToQuery GetType where
  toQuery GetType' {..} =
    Prelude.mconcat ["format" Data.=: format]

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

instance Prelude.NFData GetTypeResponse where
  rnf GetTypeResponse' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf httpStatus
