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
-- Module      : Network.AWS.AppSync.GetResolver
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a @Resolver@ object.
module Network.AWS.AppSync.GetResolver
  ( -- * Creating a Request
    GetResolver (..),
    newGetResolver,

    -- * Request Lenses
    getResolver_apiId,
    getResolver_typeName,
    getResolver_fieldName,

    -- * Destructuring the Response
    GetResolverResponse (..),
    newGetResolverResponse,

    -- * Response Lenses
    getResolverResponse_resolver,
    getResolverResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetResolver' smart constructor.
data GetResolver = GetResolver'
  { -- | The API ID.
    apiId :: Prelude.Text,
    -- | The resolver type name.
    typeName :: Prelude.Text,
    -- | The resolver field name.
    fieldName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResolver' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'getResolver_apiId' - The API ID.
--
-- 'typeName', 'getResolver_typeName' - The resolver type name.
--
-- 'fieldName', 'getResolver_fieldName' - The resolver field name.
newGetResolver ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'typeName'
  Prelude.Text ->
  -- | 'fieldName'
  Prelude.Text ->
  GetResolver
newGetResolver pApiId_ pTypeName_ pFieldName_ =
  GetResolver'
    { apiId = pApiId_,
      typeName = pTypeName_,
      fieldName = pFieldName_
    }

-- | The API ID.
getResolver_apiId :: Lens.Lens' GetResolver Prelude.Text
getResolver_apiId = Lens.lens (\GetResolver' {apiId} -> apiId) (\s@GetResolver' {} a -> s {apiId = a} :: GetResolver)

-- | The resolver type name.
getResolver_typeName :: Lens.Lens' GetResolver Prelude.Text
getResolver_typeName = Lens.lens (\GetResolver' {typeName} -> typeName) (\s@GetResolver' {} a -> s {typeName = a} :: GetResolver)

-- | The resolver field name.
getResolver_fieldName :: Lens.Lens' GetResolver Prelude.Text
getResolver_fieldName = Lens.lens (\GetResolver' {fieldName} -> fieldName) (\s@GetResolver' {} a -> s {fieldName = a} :: GetResolver)

instance Core.AWSRequest GetResolver where
  type AWSResponse GetResolver = GetResolverResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResolverResponse'
            Prelude.<$> (x Core..?> "resolver")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResolver

instance Prelude.NFData GetResolver

instance Core.ToHeaders GetResolver where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetResolver where
  toPath GetResolver' {..} =
    Prelude.mconcat
      [ "/v1/apis/",
        Core.toBS apiId,
        "/types/",
        Core.toBS typeName,
        "/resolvers/",
        Core.toBS fieldName
      ]

instance Core.ToQuery GetResolver where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResolverResponse' smart constructor.
data GetResolverResponse = GetResolverResponse'
  { -- | The @Resolver@ object.
    resolver :: Prelude.Maybe Resolver,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResolverResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolver', 'getResolverResponse_resolver' - The @Resolver@ object.
--
-- 'httpStatus', 'getResolverResponse_httpStatus' - The response's http status code.
newGetResolverResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResolverResponse
newGetResolverResponse pHttpStatus_ =
  GetResolverResponse'
    { resolver = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @Resolver@ object.
getResolverResponse_resolver :: Lens.Lens' GetResolverResponse (Prelude.Maybe Resolver)
getResolverResponse_resolver = Lens.lens (\GetResolverResponse' {resolver} -> resolver) (\s@GetResolverResponse' {} a -> s {resolver = a} :: GetResolverResponse)

-- | The response's http status code.
getResolverResponse_httpStatus :: Lens.Lens' GetResolverResponse Prelude.Int
getResolverResponse_httpStatus = Lens.lens (\GetResolverResponse' {httpStatus} -> httpStatus) (\s@GetResolverResponse' {} a -> s {httpStatus = a} :: GetResolverResponse)

instance Prelude.NFData GetResolverResponse
