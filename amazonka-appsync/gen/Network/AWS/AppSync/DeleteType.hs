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
-- Module      : Network.AWS.AppSync.DeleteType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a @Type@ object.
module Network.AWS.AppSync.DeleteType
  ( -- * Creating a Request
    DeleteType (..),
    newDeleteType,

    -- * Request Lenses
    deleteType_apiId,
    deleteType_typeName,

    -- * Destructuring the Response
    DeleteTypeResponse (..),
    newDeleteTypeResponse,

    -- * Response Lenses
    deleteTypeResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteType' smart constructor.
data DeleteType = DeleteType'
  { -- | The API ID.
    apiId :: Core.Text,
    -- | The type name.
    typeName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'deleteType_apiId' - The API ID.
--
-- 'typeName', 'deleteType_typeName' - The type name.
newDeleteType ::
  -- | 'apiId'
  Core.Text ->
  -- | 'typeName'
  Core.Text ->
  DeleteType
newDeleteType pApiId_ pTypeName_ =
  DeleteType' {apiId = pApiId_, typeName = pTypeName_}

-- | The API ID.
deleteType_apiId :: Lens.Lens' DeleteType Core.Text
deleteType_apiId = Lens.lens (\DeleteType' {apiId} -> apiId) (\s@DeleteType' {} a -> s {apiId = a} :: DeleteType)

-- | The type name.
deleteType_typeName :: Lens.Lens' DeleteType Core.Text
deleteType_typeName = Lens.lens (\DeleteType' {typeName} -> typeName) (\s@DeleteType' {} a -> s {typeName = a} :: DeleteType)

instance Core.AWSRequest DeleteType where
  type AWSResponse DeleteType = DeleteTypeResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTypeResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteType

instance Core.NFData DeleteType

instance Core.ToHeaders DeleteType where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteType where
  toPath DeleteType' {..} =
    Core.mconcat
      [ "/v1/apis/",
        Core.toBS apiId,
        "/types/",
        Core.toBS typeName
      ]

instance Core.ToQuery DeleteType where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteTypeResponse' smart constructor.
data DeleteTypeResponse = DeleteTypeResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteTypeResponse_httpStatus' - The response's http status code.
newDeleteTypeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteTypeResponse
newDeleteTypeResponse pHttpStatus_ =
  DeleteTypeResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteTypeResponse_httpStatus :: Lens.Lens' DeleteTypeResponse Core.Int
deleteTypeResponse_httpStatus = Lens.lens (\DeleteTypeResponse' {httpStatus} -> httpStatus) (\s@DeleteTypeResponse' {} a -> s {httpStatus = a} :: DeleteTypeResponse)

instance Core.NFData DeleteTypeResponse
