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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteType' smart constructor.
data DeleteType = DeleteType'
  { -- | The API ID.
    apiId :: Prelude.Text,
    -- | The type name.
    typeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'typeName'
  Prelude.Text ->
  DeleteType
newDeleteType pApiId_ pTypeName_ =
  DeleteType' {apiId = pApiId_, typeName = pTypeName_}

-- | The API ID.
deleteType_apiId :: Lens.Lens' DeleteType Prelude.Text
deleteType_apiId = Lens.lens (\DeleteType' {apiId} -> apiId) (\s@DeleteType' {} a -> s {apiId = a} :: DeleteType)

-- | The type name.
deleteType_typeName :: Lens.Lens' DeleteType Prelude.Text
deleteType_typeName = Lens.lens (\DeleteType' {typeName} -> typeName) (\s@DeleteType' {} a -> s {typeName = a} :: DeleteType)

instance Prelude.AWSRequest DeleteType where
  type Rs DeleteType = DeleteTypeResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTypeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteType

instance Prelude.NFData DeleteType

instance Prelude.ToHeaders DeleteType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DeleteType where
  toPath DeleteType' {..} =
    Prelude.mconcat
      [ "/v1/apis/",
        Prelude.toBS apiId,
        "/types/",
        Prelude.toBS typeName
      ]

instance Prelude.ToQuery DeleteType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTypeResponse' smart constructor.
data DeleteTypeResponse = DeleteTypeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteTypeResponse
newDeleteTypeResponse pHttpStatus_ =
  DeleteTypeResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteTypeResponse_httpStatus :: Lens.Lens' DeleteTypeResponse Prelude.Int
deleteTypeResponse_httpStatus = Lens.lens (\DeleteTypeResponse' {httpStatus} -> httpStatus) (\s@DeleteTypeResponse' {} a -> s {httpStatus = a} :: DeleteTypeResponse)

instance Prelude.NFData DeleteTypeResponse
