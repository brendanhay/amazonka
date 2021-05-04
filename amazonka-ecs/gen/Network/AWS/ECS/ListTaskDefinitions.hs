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
-- Module      : Network.AWS.ECS.ListTaskDefinitions
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.ECS.ListTaskDefinitions
  ( -- * Creating a Request
    ListTaskDefinitions (..),
    newListTaskDefinitions,

    -- * Request Lenses
    listTaskDefinitions_nextToken,
    listTaskDefinitions_status,
    listTaskDefinitions_maxResults,
    listTaskDefinitions_familyPrefix,
    listTaskDefinitions_sort,

    -- * Destructuring the Response
    ListTaskDefinitionsResponse (..),
    newListTaskDefinitionsResponse,

    -- * Response Lenses
    listTaskDefinitionsResponse_nextToken,
    listTaskDefinitionsResponse_taskDefinitionArns,
    listTaskDefinitionsResponse_httpStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTaskDefinitions' smart constructor.
data ListTaskDefinitions = ListTaskDefinitions'
  { -- | The @nextToken@ value returned from a @ListTaskDefinitions@ request
    -- indicating that more results are available to fulfill the request and
    -- further calls will be needed. If @maxResults@ was provided, it is
    -- possible the number of results to be fewer than @maxResults@.
    --
    -- This token should be treated as an opaque identifier that is only used
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The task definition status with which to filter the
    -- @ListTaskDefinitions@ results. By default, only @ACTIVE@ task
    -- definitions are listed. By setting this parameter to @INACTIVE@, you can
    -- view task definitions that are @INACTIVE@ as long as an active task or
    -- service still references them. If you paginate the resulting output, be
    -- sure to keep the @status@ value constant in each subsequent request.
    status :: Prelude.Maybe TaskDefinitionStatus,
    -- | The maximum number of task definition results returned by
    -- @ListTaskDefinitions@ in paginated output. When this parameter is used,
    -- @ListTaskDefinitions@ only returns @maxResults@ results in a single page
    -- along with a @nextToken@ response element. The remaining results of the
    -- initial request can be seen by sending another @ListTaskDefinitions@
    -- request with the returned @nextToken@ value. This value can be between 1
    -- and 100. If this parameter is not used, then @ListTaskDefinitions@
    -- returns up to 100 results and a @nextToken@ value if applicable.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The full family name with which to filter the @ListTaskDefinitions@
    -- results. Specifying a @familyPrefix@ limits the listed task definitions
    -- to task definition revisions that belong to that family.
    familyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The order in which to sort the results. Valid values are @ASC@ and
    -- @DESC@. By default (@ASC@), task definitions are listed
    -- lexicographically by family name and in ascending numerical order by
    -- revision so that the newest task definitions in a family are listed
    -- last. Setting this parameter to @DESC@ reverses the sort order on family
    -- name and revision so that the newest task definitions in a family are
    -- listed first.
    sort :: Prelude.Maybe SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListTaskDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'status', 'listTaskDefinitions_status' - The task definition status with which to filter the
-- @ListTaskDefinitions@ results. By default, only @ACTIVE@ task
-- definitions are listed. By setting this parameter to @INACTIVE@, you can
-- view task definitions that are @INACTIVE@ as long as an active task or
-- service still references them. If you paginate the resulting output, be
-- sure to keep the @status@ value constant in each subsequent request.
--
-- 'maxResults', 'listTaskDefinitions_maxResults' - The maximum number of task definition results returned by
-- @ListTaskDefinitions@ in paginated output. When this parameter is used,
-- @ListTaskDefinitions@ only returns @maxResults@ results in a single page
-- along with a @nextToken@ response element. The remaining results of the
-- initial request can be seen by sending another @ListTaskDefinitions@
-- request with the returned @nextToken@ value. This value can be between 1
-- and 100. If this parameter is not used, then @ListTaskDefinitions@
-- returns up to 100 results and a @nextToken@ value if applicable.
--
-- 'familyPrefix', 'listTaskDefinitions_familyPrefix' - The full family name with which to filter the @ListTaskDefinitions@
-- results. Specifying a @familyPrefix@ limits the listed task definitions
-- to task definition revisions that belong to that family.
--
-- 'sort', 'listTaskDefinitions_sort' - The order in which to sort the results. Valid values are @ASC@ and
-- @DESC@. By default (@ASC@), task definitions are listed
-- lexicographically by family name and in ascending numerical order by
-- revision so that the newest task definitions in a family are listed
-- last. Setting this parameter to @DESC@ reverses the sort order on family
-- name and revision so that the newest task definitions in a family are
-- listed first.
newListTaskDefinitions ::
  ListTaskDefinitions
newListTaskDefinitions =
  ListTaskDefinitions'
    { nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      familyPrefix = Prelude.Nothing,
      sort = Prelude.Nothing
    }

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

-- | The task definition status with which to filter the
-- @ListTaskDefinitions@ results. By default, only @ACTIVE@ task
-- definitions are listed. By setting this parameter to @INACTIVE@, you can
-- view task definitions that are @INACTIVE@ as long as an active task or
-- service still references them. If you paginate the resulting output, be
-- sure to keep the @status@ value constant in each subsequent request.
listTaskDefinitions_status :: Lens.Lens' ListTaskDefinitions (Prelude.Maybe TaskDefinitionStatus)
listTaskDefinitions_status = Lens.lens (\ListTaskDefinitions' {status} -> status) (\s@ListTaskDefinitions' {} a -> s {status = a} :: ListTaskDefinitions)

-- | The maximum number of task definition results returned by
-- @ListTaskDefinitions@ in paginated output. When this parameter is used,
-- @ListTaskDefinitions@ only returns @maxResults@ results in a single page
-- along with a @nextToken@ response element. The remaining results of the
-- initial request can be seen by sending another @ListTaskDefinitions@
-- request with the returned @nextToken@ value. This value can be between 1
-- and 100. If this parameter is not used, then @ListTaskDefinitions@
-- returns up to 100 results and a @nextToken@ value if applicable.
listTaskDefinitions_maxResults :: Lens.Lens' ListTaskDefinitions (Prelude.Maybe Prelude.Int)
listTaskDefinitions_maxResults = Lens.lens (\ListTaskDefinitions' {maxResults} -> maxResults) (\s@ListTaskDefinitions' {} a -> s {maxResults = a} :: ListTaskDefinitions)

-- | The full family name with which to filter the @ListTaskDefinitions@
-- results. Specifying a @familyPrefix@ limits the listed task definitions
-- to task definition revisions that belong to that family.
listTaskDefinitions_familyPrefix :: Lens.Lens' ListTaskDefinitions (Prelude.Maybe Prelude.Text)
listTaskDefinitions_familyPrefix = Lens.lens (\ListTaskDefinitions' {familyPrefix} -> familyPrefix) (\s@ListTaskDefinitions' {} a -> s {familyPrefix = a} :: ListTaskDefinitions)

-- | The order in which to sort the results. Valid values are @ASC@ and
-- @DESC@. By default (@ASC@), task definitions are listed
-- lexicographically by family name and in ascending numerical order by
-- revision so that the newest task definitions in a family are listed
-- last. Setting this parameter to @DESC@ reverses the sort order on family
-- name and revision so that the newest task definitions in a family are
-- listed first.
listTaskDefinitions_sort :: Lens.Lens' ListTaskDefinitions (Prelude.Maybe SortOrder)
listTaskDefinitions_sort = Lens.lens (\ListTaskDefinitions' {sort} -> sort) (\s@ListTaskDefinitions' {} a -> s {sort = a} :: ListTaskDefinitions)

instance Pager.AWSPager ListTaskDefinitions where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listTaskDefinitionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listTaskDefinitionsResponse_taskDefinitionArns
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listTaskDefinitions_nextToken
          Lens..~ rs
          Lens.^? listTaskDefinitionsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListTaskDefinitions where
  type
    Rs ListTaskDefinitions =
      ListTaskDefinitionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTaskDefinitionsResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> ( x Prelude..?> "taskDefinitionArns"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTaskDefinitions

instance Prelude.NFData ListTaskDefinitions

instance Prelude.ToHeaders ListTaskDefinitions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonEC2ContainerServiceV20141113.ListTaskDefinitions" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListTaskDefinitions where
  toJSON ListTaskDefinitions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("nextToken" Prelude..=) Prelude.<$> nextToken,
            ("status" Prelude..=) Prelude.<$> status,
            ("maxResults" Prelude..=) Prelude.<$> maxResults,
            ("familyPrefix" Prelude..=) Prelude.<$> familyPrefix,
            ("sort" Prelude..=) Prelude.<$> sort
          ]
      )

instance Prelude.ToPath ListTaskDefinitions where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListTaskDefinitions where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
listTaskDefinitionsResponse_taskDefinitionArns = Lens.lens (\ListTaskDefinitionsResponse' {taskDefinitionArns} -> taskDefinitionArns) (\s@ListTaskDefinitionsResponse' {} a -> s {taskDefinitionArns = a} :: ListTaskDefinitionsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listTaskDefinitionsResponse_httpStatus :: Lens.Lens' ListTaskDefinitionsResponse Prelude.Int
listTaskDefinitionsResponse_httpStatus = Lens.lens (\ListTaskDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListTaskDefinitionsResponse' {} a -> s {httpStatus = a} :: ListTaskDefinitionsResponse)

instance Prelude.NFData ListTaskDefinitionsResponse
