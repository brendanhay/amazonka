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
-- Module      : Amazonka.RedshiftServerLess.ListEndpointAccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @EndpointAccess@ objects and relevant information.
--
-- This operation returns paginated results.
module Amazonka.RedshiftServerLess.ListEndpointAccess
  ( -- * Creating a Request
    ListEndpointAccess (..),
    newListEndpointAccess,

    -- * Request Lenses
    listEndpointAccess_maxResults,
    listEndpointAccess_nextToken,
    listEndpointAccess_vpcId,
    listEndpointAccess_workgroupName,

    -- * Destructuring the Response
    ListEndpointAccessResponse (..),
    newListEndpointAccessResponse,

    -- * Response Lenses
    listEndpointAccessResponse_nextToken,
    listEndpointAccessResponse_httpStatus,
    listEndpointAccessResponse_endpoints,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEndpointAccess' smart constructor.
data ListEndpointAccess = ListEndpointAccess'
  { -- | An optional parameter that specifies the maximum number of results to
    -- return. You can use @nextToken@ to display the next page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If your initial @ListEndpointAccess@ operation returns a @nextToken@,
    -- you can include the returned @nextToken@ in following
    -- @ListEndpointAccess@ operations, which returns results in the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the virtual private cloud with access to Amazon
    -- Redshift Serverless.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The name of the workgroup associated with the VPC endpoint to return.
    workgroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEndpointAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listEndpointAccess_maxResults' - An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to display the next page of results.
--
-- 'nextToken', 'listEndpointAccess_nextToken' - If your initial @ListEndpointAccess@ operation returns a @nextToken@,
-- you can include the returned @nextToken@ in following
-- @ListEndpointAccess@ operations, which returns results in the next page.
--
-- 'vpcId', 'listEndpointAccess_vpcId' - The unique identifier of the virtual private cloud with access to Amazon
-- Redshift Serverless.
--
-- 'workgroupName', 'listEndpointAccess_workgroupName' - The name of the workgroup associated with the VPC endpoint to return.
newListEndpointAccess ::
  ListEndpointAccess
newListEndpointAccess =
  ListEndpointAccess'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      workgroupName = Prelude.Nothing
    }

-- | An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to display the next page of results.
listEndpointAccess_maxResults :: Lens.Lens' ListEndpointAccess (Prelude.Maybe Prelude.Natural)
listEndpointAccess_maxResults = Lens.lens (\ListEndpointAccess' {maxResults} -> maxResults) (\s@ListEndpointAccess' {} a -> s {maxResults = a} :: ListEndpointAccess)

-- | If your initial @ListEndpointAccess@ operation returns a @nextToken@,
-- you can include the returned @nextToken@ in following
-- @ListEndpointAccess@ operations, which returns results in the next page.
listEndpointAccess_nextToken :: Lens.Lens' ListEndpointAccess (Prelude.Maybe Prelude.Text)
listEndpointAccess_nextToken = Lens.lens (\ListEndpointAccess' {nextToken} -> nextToken) (\s@ListEndpointAccess' {} a -> s {nextToken = a} :: ListEndpointAccess)

-- | The unique identifier of the virtual private cloud with access to Amazon
-- Redshift Serverless.
listEndpointAccess_vpcId :: Lens.Lens' ListEndpointAccess (Prelude.Maybe Prelude.Text)
listEndpointAccess_vpcId = Lens.lens (\ListEndpointAccess' {vpcId} -> vpcId) (\s@ListEndpointAccess' {} a -> s {vpcId = a} :: ListEndpointAccess)

-- | The name of the workgroup associated with the VPC endpoint to return.
listEndpointAccess_workgroupName :: Lens.Lens' ListEndpointAccess (Prelude.Maybe Prelude.Text)
listEndpointAccess_workgroupName = Lens.lens (\ListEndpointAccess' {workgroupName} -> workgroupName) (\s@ListEndpointAccess' {} a -> s {workgroupName = a} :: ListEndpointAccess)

instance Core.AWSPager ListEndpointAccess where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEndpointAccessResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listEndpointAccessResponse_endpoints) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listEndpointAccess_nextToken
          Lens..~ rs
          Lens.^? listEndpointAccessResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListEndpointAccess where
  type
    AWSResponse ListEndpointAccess =
      ListEndpointAccessResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEndpointAccessResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "endpoints" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListEndpointAccess where
  hashWithSalt _salt ListEndpointAccess' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` workgroupName

instance Prelude.NFData ListEndpointAccess where
  rnf ListEndpointAccess' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf workgroupName

instance Data.ToHeaders ListEndpointAccess where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftServerless.ListEndpointAccess" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListEndpointAccess where
  toJSON ListEndpointAccess' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("vpcId" Data..=) Prelude.<$> vpcId,
            ("workgroupName" Data..=) Prelude.<$> workgroupName
          ]
      )

instance Data.ToPath ListEndpointAccess where
  toPath = Prelude.const "/"

instance Data.ToQuery ListEndpointAccess where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEndpointAccessResponse' smart constructor.
data ListEndpointAccessResponse = ListEndpointAccessResponse'
  { -- | When @nextToken@ is returned, there are more results available. The
    -- value of @nextToken@ is a unique pagination token for each page. Make
    -- the call again using the returned token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The returned VPC endpoints.
    endpoints :: [EndpointAccess]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEndpointAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEndpointAccessResponse_nextToken' - When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
--
-- 'httpStatus', 'listEndpointAccessResponse_httpStatus' - The response's http status code.
--
-- 'endpoints', 'listEndpointAccessResponse_endpoints' - The returned VPC endpoints.
newListEndpointAccessResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEndpointAccessResponse
newListEndpointAccessResponse pHttpStatus_ =
  ListEndpointAccessResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      endpoints = Prelude.mempty
    }

-- | When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
listEndpointAccessResponse_nextToken :: Lens.Lens' ListEndpointAccessResponse (Prelude.Maybe Prelude.Text)
listEndpointAccessResponse_nextToken = Lens.lens (\ListEndpointAccessResponse' {nextToken} -> nextToken) (\s@ListEndpointAccessResponse' {} a -> s {nextToken = a} :: ListEndpointAccessResponse)

-- | The response's http status code.
listEndpointAccessResponse_httpStatus :: Lens.Lens' ListEndpointAccessResponse Prelude.Int
listEndpointAccessResponse_httpStatus = Lens.lens (\ListEndpointAccessResponse' {httpStatus} -> httpStatus) (\s@ListEndpointAccessResponse' {} a -> s {httpStatus = a} :: ListEndpointAccessResponse)

-- | The returned VPC endpoints.
listEndpointAccessResponse_endpoints :: Lens.Lens' ListEndpointAccessResponse [EndpointAccess]
listEndpointAccessResponse_endpoints = Lens.lens (\ListEndpointAccessResponse' {endpoints} -> endpoints) (\s@ListEndpointAccessResponse' {} a -> s {endpoints = a} :: ListEndpointAccessResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListEndpointAccessResponse where
  rnf ListEndpointAccessResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf endpoints
