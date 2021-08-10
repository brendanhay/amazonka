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
-- Module      : Network.AWS.AppSync.GetGraphqlApi
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a @GraphqlApi@ object.
module Network.AWS.AppSync.GetGraphqlApi
  ( -- * Creating a Request
    GetGraphqlApi (..),
    newGetGraphqlApi,

    -- * Request Lenses
    getGraphqlApi_apiId,

    -- * Destructuring the Response
    GetGraphqlApiResponse (..),
    newGetGraphqlApiResponse,

    -- * Response Lenses
    getGraphqlApiResponse_graphqlApi,
    getGraphqlApiResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetGraphqlApi' smart constructor.
data GetGraphqlApi = GetGraphqlApi'
  { -- | The API ID for the GraphQL API.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGraphqlApi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'getGraphqlApi_apiId' - The API ID for the GraphQL API.
newGetGraphqlApi ::
  -- | 'apiId'
  Prelude.Text ->
  GetGraphqlApi
newGetGraphqlApi pApiId_ =
  GetGraphqlApi' {apiId = pApiId_}

-- | The API ID for the GraphQL API.
getGraphqlApi_apiId :: Lens.Lens' GetGraphqlApi Prelude.Text
getGraphqlApi_apiId = Lens.lens (\GetGraphqlApi' {apiId} -> apiId) (\s@GetGraphqlApi' {} a -> s {apiId = a} :: GetGraphqlApi)

instance Core.AWSRequest GetGraphqlApi where
  type
    AWSResponse GetGraphqlApi =
      GetGraphqlApiResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGraphqlApiResponse'
            Prelude.<$> (x Core..?> "graphqlApi")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetGraphqlApi

instance Prelude.NFData GetGraphqlApi

instance Core.ToHeaders GetGraphqlApi where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetGraphqlApi where
  toPath GetGraphqlApi' {..} =
    Prelude.mconcat ["/v1/apis/", Core.toBS apiId]

instance Core.ToQuery GetGraphqlApi where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetGraphqlApiResponse' smart constructor.
data GetGraphqlApiResponse = GetGraphqlApiResponse'
  { -- | The @GraphqlApi@ object.
    graphqlApi :: Prelude.Maybe GraphqlApi,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGraphqlApiResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'graphqlApi', 'getGraphqlApiResponse_graphqlApi' - The @GraphqlApi@ object.
--
-- 'httpStatus', 'getGraphqlApiResponse_httpStatus' - The response's http status code.
newGetGraphqlApiResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetGraphqlApiResponse
newGetGraphqlApiResponse pHttpStatus_ =
  GetGraphqlApiResponse'
    { graphqlApi =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @GraphqlApi@ object.
getGraphqlApiResponse_graphqlApi :: Lens.Lens' GetGraphqlApiResponse (Prelude.Maybe GraphqlApi)
getGraphqlApiResponse_graphqlApi = Lens.lens (\GetGraphqlApiResponse' {graphqlApi} -> graphqlApi) (\s@GetGraphqlApiResponse' {} a -> s {graphqlApi = a} :: GetGraphqlApiResponse)

-- | The response's http status code.
getGraphqlApiResponse_httpStatus :: Lens.Lens' GetGraphqlApiResponse Prelude.Int
getGraphqlApiResponse_httpStatus = Lens.lens (\GetGraphqlApiResponse' {httpStatus} -> httpStatus) (\s@GetGraphqlApiResponse' {} a -> s {httpStatus = a} :: GetGraphqlApiResponse)

instance Prelude.NFData GetGraphqlApiResponse
