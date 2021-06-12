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
-- Module      : Network.AWS.AppSync.DeleteResolver
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a @Resolver@ object.
module Network.AWS.AppSync.DeleteResolver
  ( -- * Creating a Request
    DeleteResolver (..),
    newDeleteResolver,

    -- * Request Lenses
    deleteResolver_apiId,
    deleteResolver_typeName,
    deleteResolver_fieldName,

    -- * Destructuring the Response
    DeleteResolverResponse (..),
    newDeleteResolverResponse,

    -- * Response Lenses
    deleteResolverResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteResolver' smart constructor.
data DeleteResolver = DeleteResolver'
  { -- | The API ID.
    apiId :: Core.Text,
    -- | The name of the resolver type.
    typeName :: Core.Text,
    -- | The resolver field name.
    fieldName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteResolver' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'deleteResolver_apiId' - The API ID.
--
-- 'typeName', 'deleteResolver_typeName' - The name of the resolver type.
--
-- 'fieldName', 'deleteResolver_fieldName' - The resolver field name.
newDeleteResolver ::
  -- | 'apiId'
  Core.Text ->
  -- | 'typeName'
  Core.Text ->
  -- | 'fieldName'
  Core.Text ->
  DeleteResolver
newDeleteResolver pApiId_ pTypeName_ pFieldName_ =
  DeleteResolver'
    { apiId = pApiId_,
      typeName = pTypeName_,
      fieldName = pFieldName_
    }

-- | The API ID.
deleteResolver_apiId :: Lens.Lens' DeleteResolver Core.Text
deleteResolver_apiId = Lens.lens (\DeleteResolver' {apiId} -> apiId) (\s@DeleteResolver' {} a -> s {apiId = a} :: DeleteResolver)

-- | The name of the resolver type.
deleteResolver_typeName :: Lens.Lens' DeleteResolver Core.Text
deleteResolver_typeName = Lens.lens (\DeleteResolver' {typeName} -> typeName) (\s@DeleteResolver' {} a -> s {typeName = a} :: DeleteResolver)

-- | The resolver field name.
deleteResolver_fieldName :: Lens.Lens' DeleteResolver Core.Text
deleteResolver_fieldName = Lens.lens (\DeleteResolver' {fieldName} -> fieldName) (\s@DeleteResolver' {} a -> s {fieldName = a} :: DeleteResolver)

instance Core.AWSRequest DeleteResolver where
  type
    AWSResponse DeleteResolver =
      DeleteResolverResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteResolverResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteResolver

instance Core.NFData DeleteResolver

instance Core.ToHeaders DeleteResolver where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteResolver where
  toPath DeleteResolver' {..} =
    Core.mconcat
      [ "/v1/apis/",
        Core.toBS apiId,
        "/types/",
        Core.toBS typeName,
        "/resolvers/",
        Core.toBS fieldName
      ]

instance Core.ToQuery DeleteResolver where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteResolverResponse' smart constructor.
data DeleteResolverResponse = DeleteResolverResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteResolverResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteResolverResponse_httpStatus' - The response's http status code.
newDeleteResolverResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteResolverResponse
newDeleteResolverResponse pHttpStatus_ =
  DeleteResolverResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteResolverResponse_httpStatus :: Lens.Lens' DeleteResolverResponse Core.Int
deleteResolverResponse_httpStatus = Lens.lens (\DeleteResolverResponse' {httpStatus} -> httpStatus) (\s@DeleteResolverResponse' {} a -> s {httpStatus = a} :: DeleteResolverResponse)

instance Core.NFData DeleteResolverResponse
