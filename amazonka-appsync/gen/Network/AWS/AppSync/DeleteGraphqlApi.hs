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
-- Module      : Network.AWS.AppSync.DeleteGraphqlApi
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a @GraphqlApi@ object.
module Network.AWS.AppSync.DeleteGraphqlApi
  ( -- * Creating a Request
    DeleteGraphqlApi (..),
    newDeleteGraphqlApi,

    -- * Request Lenses
    deleteGraphqlApi_apiId,

    -- * Destructuring the Response
    DeleteGraphqlApiResponse (..),
    newDeleteGraphqlApiResponse,

    -- * Response Lenses
    deleteGraphqlApiResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteGraphqlApi' smart constructor.
data DeleteGraphqlApi = DeleteGraphqlApi'
  { -- | The API ID.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteGraphqlApi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'deleteGraphqlApi_apiId' - The API ID.
newDeleteGraphqlApi ::
  -- | 'apiId'
  Prelude.Text ->
  DeleteGraphqlApi
newDeleteGraphqlApi pApiId_ =
  DeleteGraphqlApi' {apiId = pApiId_}

-- | The API ID.
deleteGraphqlApi_apiId :: Lens.Lens' DeleteGraphqlApi Prelude.Text
deleteGraphqlApi_apiId = Lens.lens (\DeleteGraphqlApi' {apiId} -> apiId) (\s@DeleteGraphqlApi' {} a -> s {apiId = a} :: DeleteGraphqlApi)

instance Prelude.AWSRequest DeleteGraphqlApi where
  type Rs DeleteGraphqlApi = DeleteGraphqlApiResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteGraphqlApiResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteGraphqlApi

instance Prelude.NFData DeleteGraphqlApi

instance Prelude.ToHeaders DeleteGraphqlApi where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DeleteGraphqlApi where
  toPath DeleteGraphqlApi' {..} =
    Prelude.mconcat ["/v1/apis/", Prelude.toBS apiId]

instance Prelude.ToQuery DeleteGraphqlApi where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteGraphqlApiResponse' smart constructor.
data DeleteGraphqlApiResponse = DeleteGraphqlApiResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteGraphqlApiResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteGraphqlApiResponse_httpStatus' - The response's http status code.
newDeleteGraphqlApiResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteGraphqlApiResponse
newDeleteGraphqlApiResponse pHttpStatus_ =
  DeleteGraphqlApiResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteGraphqlApiResponse_httpStatus :: Lens.Lens' DeleteGraphqlApiResponse Prelude.Int
deleteGraphqlApiResponse_httpStatus = Lens.lens (\DeleteGraphqlApiResponse' {httpStatus} -> httpStatus) (\s@DeleteGraphqlApiResponse' {} a -> s {httpStatus = a} :: DeleteGraphqlApiResponse)

instance Prelude.NFData DeleteGraphqlApiResponse
