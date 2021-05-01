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
-- Module      : Network.AWS.SSM.DescribeAssociationExecutions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this API action to view all executions for a specific association
-- ID.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeAssociationExecutions
  ( -- * Creating a Request
    DescribeAssociationExecutions (..),
    newDescribeAssociationExecutions,

    -- * Request Lenses
    describeAssociationExecutions_nextToken,
    describeAssociationExecutions_maxResults,
    describeAssociationExecutions_filters,
    describeAssociationExecutions_associationId,

    -- * Destructuring the Response
    DescribeAssociationExecutionsResponse (..),
    newDescribeAssociationExecutionsResponse,

    -- * Response Lenses
    describeAssociationExecutionsResponse_nextToken,
    describeAssociationExecutionsResponse_associationExecutions,
    describeAssociationExecutionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeAssociationExecutions' smart constructor.
data DescribeAssociationExecutions = DescribeAssociationExecutions'
  { -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Filters for the request. You can specify the following filters and
    -- values.
    --
    -- ExecutionId (EQUAL)
    --
    -- Status (EQUAL)
    --
    -- CreatedTime (EQUAL, GREATER_THAN, LESS_THAN)
    filters :: Prelude.Maybe (Prelude.NonEmpty AssociationExecutionFilter),
    -- | The association ID for which you want to view execution history details.
    associationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeAssociationExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAssociationExecutions_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'maxResults', 'describeAssociationExecutions_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'filters', 'describeAssociationExecutions_filters' - Filters for the request. You can specify the following filters and
-- values.
--
-- ExecutionId (EQUAL)
--
-- Status (EQUAL)
--
-- CreatedTime (EQUAL, GREATER_THAN, LESS_THAN)
--
-- 'associationId', 'describeAssociationExecutions_associationId' - The association ID for which you want to view execution history details.
newDescribeAssociationExecutions ::
  -- | 'associationId'
  Prelude.Text ->
  DescribeAssociationExecutions
newDescribeAssociationExecutions pAssociationId_ =
  DescribeAssociationExecutions'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing,
      associationId = pAssociationId_
    }

-- | A token to start the list. Use this token to get the next set of
-- results.
describeAssociationExecutions_nextToken :: Lens.Lens' DescribeAssociationExecutions (Prelude.Maybe Prelude.Text)
describeAssociationExecutions_nextToken = Lens.lens (\DescribeAssociationExecutions' {nextToken} -> nextToken) (\s@DescribeAssociationExecutions' {} a -> s {nextToken = a} :: DescribeAssociationExecutions)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeAssociationExecutions_maxResults :: Lens.Lens' DescribeAssociationExecutions (Prelude.Maybe Prelude.Natural)
describeAssociationExecutions_maxResults = Lens.lens (\DescribeAssociationExecutions' {maxResults} -> maxResults) (\s@DescribeAssociationExecutions' {} a -> s {maxResults = a} :: DescribeAssociationExecutions)

-- | Filters for the request. You can specify the following filters and
-- values.
--
-- ExecutionId (EQUAL)
--
-- Status (EQUAL)
--
-- CreatedTime (EQUAL, GREATER_THAN, LESS_THAN)
describeAssociationExecutions_filters :: Lens.Lens' DescribeAssociationExecutions (Prelude.Maybe (Prelude.NonEmpty AssociationExecutionFilter))
describeAssociationExecutions_filters = Lens.lens (\DescribeAssociationExecutions' {filters} -> filters) (\s@DescribeAssociationExecutions' {} a -> s {filters = a} :: DescribeAssociationExecutions) Prelude.. Lens.mapping Prelude._Coerce

-- | The association ID for which you want to view execution history details.
describeAssociationExecutions_associationId :: Lens.Lens' DescribeAssociationExecutions Prelude.Text
describeAssociationExecutions_associationId = Lens.lens (\DescribeAssociationExecutions' {associationId} -> associationId) (\s@DescribeAssociationExecutions' {} a -> s {associationId = a} :: DescribeAssociationExecutions)

instance Pager.AWSPager DescribeAssociationExecutions where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeAssociationExecutionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeAssociationExecutionsResponse_associationExecutions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeAssociationExecutions_nextToken
          Lens..~ rs
          Lens.^? describeAssociationExecutionsResponse_nextToken
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    DescribeAssociationExecutions
  where
  type
    Rs DescribeAssociationExecutions =
      DescribeAssociationExecutionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAssociationExecutionsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "AssociationExecutions"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeAssociationExecutions

instance Prelude.NFData DescribeAssociationExecutions

instance
  Prelude.ToHeaders
    DescribeAssociationExecutions
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.DescribeAssociationExecutions" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeAssociationExecutions where
  toJSON DescribeAssociationExecutions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("Filters" Prelude..=) Prelude.<$> filters,
            Prelude.Just
              ("AssociationId" Prelude..= associationId)
          ]
      )

instance Prelude.ToPath DescribeAssociationExecutions where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DescribeAssociationExecutions
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAssociationExecutionsResponse' smart constructor.
data DescribeAssociationExecutionsResponse = DescribeAssociationExecutionsResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the executions for the specified association ID.
    associationExecutions :: Prelude.Maybe [AssociationExecution],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeAssociationExecutionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAssociationExecutionsResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'associationExecutions', 'describeAssociationExecutionsResponse_associationExecutions' - A list of the executions for the specified association ID.
--
-- 'httpStatus', 'describeAssociationExecutionsResponse_httpStatus' - The response's http status code.
newDescribeAssociationExecutionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAssociationExecutionsResponse
newDescribeAssociationExecutionsResponse pHttpStatus_ =
  DescribeAssociationExecutionsResponse'
    { nextToken =
        Prelude.Nothing,
      associationExecutions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
describeAssociationExecutionsResponse_nextToken :: Lens.Lens' DescribeAssociationExecutionsResponse (Prelude.Maybe Prelude.Text)
describeAssociationExecutionsResponse_nextToken = Lens.lens (\DescribeAssociationExecutionsResponse' {nextToken} -> nextToken) (\s@DescribeAssociationExecutionsResponse' {} a -> s {nextToken = a} :: DescribeAssociationExecutionsResponse)

-- | A list of the executions for the specified association ID.
describeAssociationExecutionsResponse_associationExecutions :: Lens.Lens' DescribeAssociationExecutionsResponse (Prelude.Maybe [AssociationExecution])
describeAssociationExecutionsResponse_associationExecutions = Lens.lens (\DescribeAssociationExecutionsResponse' {associationExecutions} -> associationExecutions) (\s@DescribeAssociationExecutionsResponse' {} a -> s {associationExecutions = a} :: DescribeAssociationExecutionsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeAssociationExecutionsResponse_httpStatus :: Lens.Lens' DescribeAssociationExecutionsResponse Prelude.Int
describeAssociationExecutionsResponse_httpStatus = Lens.lens (\DescribeAssociationExecutionsResponse' {httpStatus} -> httpStatus) (\s@DescribeAssociationExecutionsResponse' {} a -> s {httpStatus = a} :: DescribeAssociationExecutionsResponse)

instance
  Prelude.NFData
    DescribeAssociationExecutionsResponse
