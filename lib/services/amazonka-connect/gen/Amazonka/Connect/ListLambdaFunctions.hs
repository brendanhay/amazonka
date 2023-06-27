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
-- Module      : Amazonka.Connect.ListLambdaFunctions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Returns a paginated list of all Lambda functions that display in the
-- dropdown options in the relevant flow blocks.
--
-- This operation returns paginated results.
module Amazonka.Connect.ListLambdaFunctions
  ( -- * Creating a Request
    ListLambdaFunctions (..),
    newListLambdaFunctions,

    -- * Request Lenses
    listLambdaFunctions_maxResults,
    listLambdaFunctions_nextToken,
    listLambdaFunctions_instanceId,

    -- * Destructuring the Response
    ListLambdaFunctionsResponse (..),
    newListLambdaFunctionsResponse,

    -- * Response Lenses
    listLambdaFunctionsResponse_lambdaFunctions,
    listLambdaFunctionsResponse_nextToken,
    listLambdaFunctionsResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListLambdaFunctions' smart constructor.
data ListLambdaFunctions = ListLambdaFunctions'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLambdaFunctions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listLambdaFunctions_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'listLambdaFunctions_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'instanceId', 'listLambdaFunctions_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
newListLambdaFunctions ::
  -- | 'instanceId'
  Prelude.Text ->
  ListLambdaFunctions
newListLambdaFunctions pInstanceId_ =
  ListLambdaFunctions'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The maximum number of results to return per page.
listLambdaFunctions_maxResults :: Lens.Lens' ListLambdaFunctions (Prelude.Maybe Prelude.Natural)
listLambdaFunctions_maxResults = Lens.lens (\ListLambdaFunctions' {maxResults} -> maxResults) (\s@ListLambdaFunctions' {} a -> s {maxResults = a} :: ListLambdaFunctions)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listLambdaFunctions_nextToken :: Lens.Lens' ListLambdaFunctions (Prelude.Maybe Prelude.Text)
listLambdaFunctions_nextToken = Lens.lens (\ListLambdaFunctions' {nextToken} -> nextToken) (\s@ListLambdaFunctions' {} a -> s {nextToken = a} :: ListLambdaFunctions)

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
listLambdaFunctions_instanceId :: Lens.Lens' ListLambdaFunctions Prelude.Text
listLambdaFunctions_instanceId = Lens.lens (\ListLambdaFunctions' {instanceId} -> instanceId) (\s@ListLambdaFunctions' {} a -> s {instanceId = a} :: ListLambdaFunctions)

instance Core.AWSPager ListLambdaFunctions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLambdaFunctionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listLambdaFunctionsResponse_lambdaFunctions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listLambdaFunctions_nextToken
          Lens..~ rs
          Lens.^? listLambdaFunctionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListLambdaFunctions where
  type
    AWSResponse ListLambdaFunctions =
      ListLambdaFunctionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLambdaFunctionsResponse'
            Prelude.<$> ( x
                            Data..?> "LambdaFunctions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLambdaFunctions where
  hashWithSalt _salt ListLambdaFunctions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData ListLambdaFunctions where
  rnf ListLambdaFunctions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders ListLambdaFunctions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListLambdaFunctions where
  toPath ListLambdaFunctions' {..} =
    Prelude.mconcat
      [ "/instance/",
        Data.toBS instanceId,
        "/lambda-functions"
      ]

instance Data.ToQuery ListLambdaFunctions where
  toQuery ListLambdaFunctions' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListLambdaFunctionsResponse' smart constructor.
data ListLambdaFunctionsResponse = ListLambdaFunctionsResponse'
  { -- | The Lambdafunction ARNs associated with the specified instance.
    lambdaFunctions :: Prelude.Maybe [Prelude.Text],
    -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLambdaFunctionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambdaFunctions', 'listLambdaFunctionsResponse_lambdaFunctions' - The Lambdafunction ARNs associated with the specified instance.
--
-- 'nextToken', 'listLambdaFunctionsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'httpStatus', 'listLambdaFunctionsResponse_httpStatus' - The response's http status code.
newListLambdaFunctionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLambdaFunctionsResponse
newListLambdaFunctionsResponse pHttpStatus_ =
  ListLambdaFunctionsResponse'
    { lambdaFunctions =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Lambdafunction ARNs associated with the specified instance.
listLambdaFunctionsResponse_lambdaFunctions :: Lens.Lens' ListLambdaFunctionsResponse (Prelude.Maybe [Prelude.Text])
listLambdaFunctionsResponse_lambdaFunctions = Lens.lens (\ListLambdaFunctionsResponse' {lambdaFunctions} -> lambdaFunctions) (\s@ListLambdaFunctionsResponse' {} a -> s {lambdaFunctions = a} :: ListLambdaFunctionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are additional results, this is the token for the next set of
-- results.
listLambdaFunctionsResponse_nextToken :: Lens.Lens' ListLambdaFunctionsResponse (Prelude.Maybe Prelude.Text)
listLambdaFunctionsResponse_nextToken = Lens.lens (\ListLambdaFunctionsResponse' {nextToken} -> nextToken) (\s@ListLambdaFunctionsResponse' {} a -> s {nextToken = a} :: ListLambdaFunctionsResponse)

-- | The response's http status code.
listLambdaFunctionsResponse_httpStatus :: Lens.Lens' ListLambdaFunctionsResponse Prelude.Int
listLambdaFunctionsResponse_httpStatus = Lens.lens (\ListLambdaFunctionsResponse' {httpStatus} -> httpStatus) (\s@ListLambdaFunctionsResponse' {} a -> s {httpStatus = a} :: ListLambdaFunctionsResponse)

instance Prelude.NFData ListLambdaFunctionsResponse where
  rnf ListLambdaFunctionsResponse' {..} =
    Prelude.rnf lambdaFunctions
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
