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
-- Module      : Amazonka.ECS.ListTaskDefinitions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of task definitions that are registered to your account.
-- You can filter the results by family name with the @familyPrefix@
-- parameter or by status with the @status@ parameter.
--
-- This operation returns paginated results.
module Amazonka.ECS.ListTaskDefinitions
  ( -- * Creating a Request
    ListTaskDefinitions (..),
    newListTaskDefinitions,

    -- * Request Lenses
    listTaskDefinitions_familyPrefix,
    listTaskDefinitions_maxResults,
    listTaskDefinitions_nextToken,
    listTaskDefinitions_sort,
    listTaskDefinitions_status,

    -- * Destructuring the Response
    ListTaskDefinitionsResponse (..),
    newListTaskDefinitionsResponse,

    -- * Response Lenses
    listTaskDefinitionsResponse_nextToken,
    listTaskDefinitionsResponse_taskDefinitionArns,
    listTaskDefinitionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTaskDefinitions' smart constructor.
data ListTaskDefinitions = ListTaskDefinitions'
  { -- | The full family name to filter the @ListTaskDefinitions@ results with.
    -- Specifying a @familyPrefix@ limits the listed task definitions to task
    -- definition revisions that belong to that family.
    familyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of task definition results that @ListTaskDefinitions@
    -- returned in paginated output. When this parameter is used,
    -- @ListTaskDefinitions@ only returns @maxResults@ results in a single page
    -- along with a @nextToken@ response element. The remaining results of the
    -- initial request can be seen by sending another @ListTaskDefinitions@
    -- request with the returned @nextToken@ value. This value can be between 1
    -- and 100. If this parameter isn\'t used, then @ListTaskDefinitions@
    -- returns up to 100 results and a @nextToken@ value if applicable.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The @nextToken@ value returned from a @ListTaskDefinitions@ request
    -- indicating that more results are available to fulfill the request and
    -- further calls will be needed. If @maxResults@ was provided, it is
    -- possible the number of results to be fewer than @maxResults@.
    --
    -- This token should be treated as an opaque identifier that is only used
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The order to sort the results in. Valid values are @ASC@ and @DESC@. By
    -- default, (@ASC@) task definitions are listed lexicographically by family
    -- name and in ascending numerical order by revision so that the newest
    -- task definitions in a family are listed last. Setting this parameter to
    -- @DESC@ reverses the sort order on family name and revision. This is so
    -- that the newest task definitions in a family are listed first.
    sort :: Prelude.Maybe SortOrder,
    -- | The task definition status to filter the @ListTaskDefinitions@ results
    -- with. By default, only @ACTIVE@ task definitions are listed. By setting
    -- this parameter to @INACTIVE@, you can view task definitions that are
    -- @INACTIVE@ as long as an active task or service still references them.
    -- If you paginate the resulting output, be sure to keep the @status@ value
    -- constant in each subsequent request.
    status :: Prelude.Maybe TaskDefinitionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTaskDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'familyPrefix', 'listTaskDefinitions_familyPrefix' - The full family name to filter the @ListTaskDefinitions@ results with.
-- Specifying a @familyPrefix@ limits the listed task definitions to task
-- definition revisions that belong to that family.
--
-- 'maxResults', 'listTaskDefinitions_maxResults' - The maximum number of task definition results that @ListTaskDefinitions@
-- returned in paginated output. When this parameter is used,
-- @ListTaskDefinitions@ only returns @maxResults@ results in a single page
-- along with a @nextToken@ response element. The remaining results of the
-- initial request can be seen by sending another @ListTaskDefinitions@
-- request with the returned @nextToken@ value. This value can be between 1
-- and 100. If this parameter isn\'t used, then @ListTaskDefinitions@
-- returns up to 100 results and a @nextToken@ value if applicable.
--
-- 'nextToken', 'listTaskDefinitions_nextToken' - The @nextToken@ value returned from a @ListTaskDefinitions@ request
-- indicating that more results are available to fulfill the request and
-- further calls will be needed. If @maxResults@ was provided, it is
-- possible the number of results to be fewer than @maxResults@.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
--
-- 'sort', 'listTaskDefinitions_sort' - The order to sort the results in. Valid values are @ASC@ and @DESC@. By
-- default, (@ASC@) task definitions are listed lexicographically by family
-- name and in ascending numerical order by revision so that the newest
-- task definitions in a family are listed last. Setting this parameter to
-- @DESC@ reverses the sort order on family name and revision. This is so
-- that the newest task definitions in a family are listed first.
--
-- 'status', 'listTaskDefinitions_status' - The task definition status to filter the @ListTaskDefinitions@ results
-- with. By default, only @ACTIVE@ task definitions are listed. By setting
-- this parameter to @INACTIVE@, you can view task definitions that are
-- @INACTIVE@ as long as an active task or service still references them.
-- If you paginate the resulting output, be sure to keep the @status@ value
-- constant in each subsequent request.
newListTaskDefinitions ::
  ListTaskDefinitions
newListTaskDefinitions =
  ListTaskDefinitions'
    { familyPrefix =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sort = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The full family name to filter the @ListTaskDefinitions@ results with.
-- Specifying a @familyPrefix@ limits the listed task definitions to task
-- definition revisions that belong to that family.
listTaskDefinitions_familyPrefix :: Lens.Lens' ListTaskDefinitions (Prelude.Maybe Prelude.Text)
listTaskDefinitions_familyPrefix = Lens.lens (\ListTaskDefinitions' {familyPrefix} -> familyPrefix) (\s@ListTaskDefinitions' {} a -> s {familyPrefix = a} :: ListTaskDefinitions)

-- | The maximum number of task definition results that @ListTaskDefinitions@
-- returned in paginated output. When this parameter is used,
-- @ListTaskDefinitions@ only returns @maxResults@ results in a single page
-- along with a @nextToken@ response element. The remaining results of the
-- initial request can be seen by sending another @ListTaskDefinitions@
-- request with the returned @nextToken@ value. This value can be between 1
-- and 100. If this parameter isn\'t used, then @ListTaskDefinitions@
-- returns up to 100 results and a @nextToken@ value if applicable.
listTaskDefinitions_maxResults :: Lens.Lens' ListTaskDefinitions (Prelude.Maybe Prelude.Int)
listTaskDefinitions_maxResults = Lens.lens (\ListTaskDefinitions' {maxResults} -> maxResults) (\s@ListTaskDefinitions' {} a -> s {maxResults = a} :: ListTaskDefinitions)

-- | The @nextToken@ value returned from a @ListTaskDefinitions@ request
-- indicating that more results are available to fulfill the request and
-- further calls will be needed. If @maxResults@ was provided, it is
-- possible the number of results to be fewer than @maxResults@.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
listTaskDefinitions_nextToken :: Lens.Lens' ListTaskDefinitions (Prelude.Maybe Prelude.Text)
listTaskDefinitions_nextToken = Lens.lens (\ListTaskDefinitions' {nextToken} -> nextToken) (\s@ListTaskDefinitions' {} a -> s {nextToken = a} :: ListTaskDefinitions)

-- | The order to sort the results in. Valid values are @ASC@ and @DESC@. By
-- default, (@ASC@) task definitions are listed lexicographically by family
-- name and in ascending numerical order by revision so that the newest
-- task definitions in a family are listed last. Setting this parameter to
-- @DESC@ reverses the sort order on family name and revision. This is so
-- that the newest task definitions in a family are listed first.
listTaskDefinitions_sort :: Lens.Lens' ListTaskDefinitions (Prelude.Maybe SortOrder)
listTaskDefinitions_sort = Lens.lens (\ListTaskDefinitions' {sort} -> sort) (\s@ListTaskDefinitions' {} a -> s {sort = a} :: ListTaskDefinitions)

-- | The task definition status to filter the @ListTaskDefinitions@ results
-- with. By default, only @ACTIVE@ task definitions are listed. By setting
-- this parameter to @INACTIVE@, you can view task definitions that are
-- @INACTIVE@ as long as an active task or service still references them.
-- If you paginate the resulting output, be sure to keep the @status@ value
-- constant in each subsequent request.
listTaskDefinitions_status :: Lens.Lens' ListTaskDefinitions (Prelude.Maybe TaskDefinitionStatus)
listTaskDefinitions_status = Lens.lens (\ListTaskDefinitions' {status} -> status) (\s@ListTaskDefinitions' {} a -> s {status = a} :: ListTaskDefinitions)

instance Core.AWSPager ListTaskDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTaskDefinitionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTaskDefinitionsResponse_taskDefinitionArns
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listTaskDefinitions_nextToken
          Lens..~ rs
          Lens.^? listTaskDefinitionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListTaskDefinitions where
  type
    AWSResponse ListTaskDefinitions =
      ListTaskDefinitionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTaskDefinitionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "taskDefinitionArns"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTaskDefinitions where
  hashWithSalt _salt ListTaskDefinitions' {..} =
    _salt
      `Prelude.hashWithSalt` familyPrefix
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sort
      `Prelude.hashWithSalt` status

instance Prelude.NFData ListTaskDefinitions where
  rnf ListTaskDefinitions' {..} =
    Prelude.rnf familyPrefix
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sort
      `Prelude.seq` Prelude.rnf status

instance Data.ToHeaders ListTaskDefinitions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.ListTaskDefinitions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTaskDefinitions where
  toJSON ListTaskDefinitions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("familyPrefix" Data..=) Prelude.<$> familyPrefix,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("sort" Data..=) Prelude.<$> sort,
            ("status" Data..=) Prelude.<$> status
          ]
      )

instance Data.ToPath ListTaskDefinitions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTaskDefinitions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTaskDefinitionsResponse' smart constructor.
data ListTaskDefinitionsResponse = ListTaskDefinitionsResponse'
  { -- | The @nextToken@ value to include in a future @ListTaskDefinitions@
    -- request. When the results of a @ListTaskDefinitions@ request exceed
    -- @maxResults@, this value can be used to retrieve the next page of
    -- results. This value is @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of task definition Amazon Resource Name (ARN) entries for the
    -- @ListTaskDefinitions@ request.
    taskDefinitionArns :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTaskDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTaskDefinitionsResponse_nextToken' - The @nextToken@ value to include in a future @ListTaskDefinitions@
-- request. When the results of a @ListTaskDefinitions@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
--
-- 'taskDefinitionArns', 'listTaskDefinitionsResponse_taskDefinitionArns' - The list of task definition Amazon Resource Name (ARN) entries for the
-- @ListTaskDefinitions@ request.
--
-- 'httpStatus', 'listTaskDefinitionsResponse_httpStatus' - The response's http status code.
newListTaskDefinitionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTaskDefinitionsResponse
newListTaskDefinitionsResponse pHttpStatus_ =
  ListTaskDefinitionsResponse'
    { nextToken =
        Prelude.Nothing,
      taskDefinitionArns = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future @ListTaskDefinitions@
-- request. When the results of a @ListTaskDefinitions@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
listTaskDefinitionsResponse_nextToken :: Lens.Lens' ListTaskDefinitionsResponse (Prelude.Maybe Prelude.Text)
listTaskDefinitionsResponse_nextToken = Lens.lens (\ListTaskDefinitionsResponse' {nextToken} -> nextToken) (\s@ListTaskDefinitionsResponse' {} a -> s {nextToken = a} :: ListTaskDefinitionsResponse)

-- | The list of task definition Amazon Resource Name (ARN) entries for the
-- @ListTaskDefinitions@ request.
listTaskDefinitionsResponse_taskDefinitionArns :: Lens.Lens' ListTaskDefinitionsResponse (Prelude.Maybe [Prelude.Text])
listTaskDefinitionsResponse_taskDefinitionArns = Lens.lens (\ListTaskDefinitionsResponse' {taskDefinitionArns} -> taskDefinitionArns) (\s@ListTaskDefinitionsResponse' {} a -> s {taskDefinitionArns = a} :: ListTaskDefinitionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTaskDefinitionsResponse_httpStatus :: Lens.Lens' ListTaskDefinitionsResponse Prelude.Int
listTaskDefinitionsResponse_httpStatus = Lens.lens (\ListTaskDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListTaskDefinitionsResponse' {} a -> s {httpStatus = a} :: ListTaskDefinitionsResponse)

instance Prelude.NFData ListTaskDefinitionsResponse where
  rnf ListTaskDefinitionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf taskDefinitionArns
      `Prelude.seq` Prelude.rnf httpStatus
