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
-- Module      : Amazonka.AppSync.DeleteType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a @Type@ object.
module Amazonka.AppSync.DeleteType
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

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteType' smart constructor.
data DeleteType = DeleteType'
  { -- | The API ID.
    apiId :: Prelude.Text,
    -- | The type name.
    typeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteType where
  type AWSResponse DeleteType = DeleteTypeResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTypeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteType where
  hashWithSalt _salt DeleteType' {..} =
    _salt
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` typeName

instance Prelude.NFData DeleteType where
  rnf DeleteType' {..} =
    Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf typeName

instance Data.ToHeaders DeleteType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteType where
  toPath DeleteType' {..} =
    Prelude.mconcat
      [ "/v1/apis/",
        Data.toBS apiId,
        "/types/",
        Data.toBS typeName
      ]

instance Data.ToQuery DeleteType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTypeResponse' smart constructor.
data DeleteTypeResponse = DeleteTypeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData DeleteTypeResponse where
  rnf DeleteTypeResponse' {..} = Prelude.rnf httpStatus
