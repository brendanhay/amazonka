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
-- Module      : Amazonka.SecurityHub.DescribeActionTargets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the custom action targets in Security Hub in your
-- account.
--
-- This operation returns paginated results.
module Amazonka.SecurityHub.DescribeActionTargets
  ( -- * Creating a Request
    DescribeActionTargets (..),
    newDescribeActionTargets,

    -- * Request Lenses
    describeActionTargets_actionTargetArns,
    describeActionTargets_maxResults,
    describeActionTargets_nextToken,

    -- * Destructuring the Response
    DescribeActionTargetsResponse (..),
    newDescribeActionTargetsResponse,

    -- * Response Lenses
    describeActionTargetsResponse_nextToken,
    describeActionTargetsResponse_httpStatus,
    describeActionTargetsResponse_actionTargets,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newDescribeActionTargets' smart constructor.
data DescribeActionTargets = DescribeActionTargets'
  { -- | A list of custom action target ARNs for the custom action targets to
    -- retrieve.
    actionTargetArns :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that is required for pagination. On your first call to the
    -- @DescribeActionTargets@ operation, set the value of this parameter to
    -- @NULL@.
    --
    -- For subsequent calls to the operation, to continue listing data, set the
    -- value of this parameter to the value returned from the previous
    -- response.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeActionTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionTargetArns', 'describeActionTargets_actionTargetArns' - A list of custom action target ARNs for the custom action targets to
-- retrieve.
--
-- 'maxResults', 'describeActionTargets_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'describeActionTargets_nextToken' - The token that is required for pagination. On your first call to the
-- @DescribeActionTargets@ operation, set the value of this parameter to
-- @NULL@.
--
-- For subsequent calls to the operation, to continue listing data, set the
-- value of this parameter to the value returned from the previous
-- response.
newDescribeActionTargets ::
  DescribeActionTargets
newDescribeActionTargets =
  DescribeActionTargets'
    { actionTargetArns =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | A list of custom action target ARNs for the custom action targets to
-- retrieve.
describeActionTargets_actionTargetArns :: Lens.Lens' DescribeActionTargets (Prelude.Maybe [Prelude.Text])
describeActionTargets_actionTargetArns = Lens.lens (\DescribeActionTargets' {actionTargetArns} -> actionTargetArns) (\s@DescribeActionTargets' {} a -> s {actionTargetArns = a} :: DescribeActionTargets) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return.
describeActionTargets_maxResults :: Lens.Lens' DescribeActionTargets (Prelude.Maybe Prelude.Natural)
describeActionTargets_maxResults = Lens.lens (\DescribeActionTargets' {maxResults} -> maxResults) (\s@DescribeActionTargets' {} a -> s {maxResults = a} :: DescribeActionTargets)

-- | The token that is required for pagination. On your first call to the
-- @DescribeActionTargets@ operation, set the value of this parameter to
-- @NULL@.
--
-- For subsequent calls to the operation, to continue listing data, set the
-- value of this parameter to the value returned from the previous
-- response.
describeActionTargets_nextToken :: Lens.Lens' DescribeActionTargets (Prelude.Maybe Prelude.Text)
describeActionTargets_nextToken = Lens.lens (\DescribeActionTargets' {nextToken} -> nextToken) (\s@DescribeActionTargets' {} a -> s {nextToken = a} :: DescribeActionTargets)

instance Core.AWSPager DescribeActionTargets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeActionTargetsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. describeActionTargetsResponse_actionTargets
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeActionTargets_nextToken
              Lens..~ rs
              Lens.^? describeActionTargetsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest DescribeActionTargets where
  type
    AWSResponse DescribeActionTargets =
      DescribeActionTargetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeActionTargetsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "ActionTargets" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable DescribeActionTargets where
  hashWithSalt _salt DescribeActionTargets' {..} =
    _salt
      `Prelude.hashWithSalt` actionTargetArns
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeActionTargets where
  rnf DescribeActionTargets' {..} =
    Prelude.rnf actionTargetArns `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken

instance Data.ToHeaders DescribeActionTargets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeActionTargets where
  toJSON DescribeActionTargets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ActionTargetArns" Data..=)
              Prelude.<$> actionTargetArns,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeActionTargets where
  toPath = Prelude.const "/actionTargets/get"

instance Data.ToQuery DescribeActionTargets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeActionTargetsResponse' smart constructor.
data DescribeActionTargetsResponse = DescribeActionTargetsResponse'
  { -- | The pagination token to use to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of @ActionTarget@ objects. Each object includes the
    -- @ActionTargetArn@, @Description@, and @Name@ of a custom action target
    -- available in Security Hub.
    actionTargets :: [ActionTarget]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeActionTargetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeActionTargetsResponse_nextToken' - The pagination token to use to request the next page of results.
--
-- 'httpStatus', 'describeActionTargetsResponse_httpStatus' - The response's http status code.
--
-- 'actionTargets', 'describeActionTargetsResponse_actionTargets' - A list of @ActionTarget@ objects. Each object includes the
-- @ActionTargetArn@, @Description@, and @Name@ of a custom action target
-- available in Security Hub.
newDescribeActionTargetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeActionTargetsResponse
newDescribeActionTargetsResponse pHttpStatus_ =
  DescribeActionTargetsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      actionTargets = Prelude.mempty
    }

-- | The pagination token to use to request the next page of results.
describeActionTargetsResponse_nextToken :: Lens.Lens' DescribeActionTargetsResponse (Prelude.Maybe Prelude.Text)
describeActionTargetsResponse_nextToken = Lens.lens (\DescribeActionTargetsResponse' {nextToken} -> nextToken) (\s@DescribeActionTargetsResponse' {} a -> s {nextToken = a} :: DescribeActionTargetsResponse)

-- | The response's http status code.
describeActionTargetsResponse_httpStatus :: Lens.Lens' DescribeActionTargetsResponse Prelude.Int
describeActionTargetsResponse_httpStatus = Lens.lens (\DescribeActionTargetsResponse' {httpStatus} -> httpStatus) (\s@DescribeActionTargetsResponse' {} a -> s {httpStatus = a} :: DescribeActionTargetsResponse)

-- | A list of @ActionTarget@ objects. Each object includes the
-- @ActionTargetArn@, @Description@, and @Name@ of a custom action target
-- available in Security Hub.
describeActionTargetsResponse_actionTargets :: Lens.Lens' DescribeActionTargetsResponse [ActionTarget]
describeActionTargetsResponse_actionTargets = Lens.lens (\DescribeActionTargetsResponse' {actionTargets} -> actionTargets) (\s@DescribeActionTargetsResponse' {} a -> s {actionTargets = a} :: DescribeActionTargetsResponse) Prelude.. Lens.coerced

instance Prelude.NFData DescribeActionTargetsResponse where
  rnf DescribeActionTargetsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf actionTargets
