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
-- Module      : Amazonka.GameLift.DescribeMatchmakingRuleSets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details for FlexMatch matchmaking rule sets. You can
-- request all existing rule sets for the Region, or provide a list of one
-- or more rule set names. When requesting multiple items, use the
-- pagination parameters to retrieve results as a set of sequential pages.
-- If successful, a rule set is returned for each requested name.
--
-- __Learn more__
--
-- -   <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-rulesets.html Build a rule set>
--
-- This operation returns paginated results.
module Amazonka.GameLift.DescribeMatchmakingRuleSets
  ( -- * Creating a Request
    DescribeMatchmakingRuleSets (..),
    newDescribeMatchmakingRuleSets,

    -- * Request Lenses
    describeMatchmakingRuleSets_limit,
    describeMatchmakingRuleSets_names,
    describeMatchmakingRuleSets_nextToken,

    -- * Destructuring the Response
    DescribeMatchmakingRuleSetsResponse (..),
    newDescribeMatchmakingRuleSetsResponse,

    -- * Response Lenses
    describeMatchmakingRuleSetsResponse_nextToken,
    describeMatchmakingRuleSetsResponse_httpStatus,
    describeMatchmakingRuleSetsResponse_ruleSets,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeMatchmakingRuleSets' smart constructor.
data DescribeMatchmakingRuleSets = DescribeMatchmakingRuleSets'
  { -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | A list of one or more matchmaking rule set names to retrieve details
    -- for. (Note: The rule set name is different from the optional \"name\"
    -- field in the rule set body.) You can use either the rule set name or ARN
    -- value.
    names :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMatchmakingRuleSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'describeMatchmakingRuleSets_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
--
-- 'names', 'describeMatchmakingRuleSets_names' - A list of one or more matchmaking rule set names to retrieve details
-- for. (Note: The rule set name is different from the optional \"name\"
-- field in the rule set body.) You can use either the rule set name or ARN
-- value.
--
-- 'nextToken', 'describeMatchmakingRuleSets_nextToken' - A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
newDescribeMatchmakingRuleSets ::
  DescribeMatchmakingRuleSets
newDescribeMatchmakingRuleSets =
  DescribeMatchmakingRuleSets'
    { limit =
        Prelude.Nothing,
      names = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
describeMatchmakingRuleSets_limit :: Lens.Lens' DescribeMatchmakingRuleSets (Prelude.Maybe Prelude.Natural)
describeMatchmakingRuleSets_limit = Lens.lens (\DescribeMatchmakingRuleSets' {limit} -> limit) (\s@DescribeMatchmakingRuleSets' {} a -> s {limit = a} :: DescribeMatchmakingRuleSets)

-- | A list of one or more matchmaking rule set names to retrieve details
-- for. (Note: The rule set name is different from the optional \"name\"
-- field in the rule set body.) You can use either the rule set name or ARN
-- value.
describeMatchmakingRuleSets_names :: Lens.Lens' DescribeMatchmakingRuleSets (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeMatchmakingRuleSets_names = Lens.lens (\DescribeMatchmakingRuleSets' {names} -> names) (\s@DescribeMatchmakingRuleSets' {} a -> s {names = a} :: DescribeMatchmakingRuleSets) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
describeMatchmakingRuleSets_nextToken :: Lens.Lens' DescribeMatchmakingRuleSets (Prelude.Maybe Prelude.Text)
describeMatchmakingRuleSets_nextToken = Lens.lens (\DescribeMatchmakingRuleSets' {nextToken} -> nextToken) (\s@DescribeMatchmakingRuleSets' {} a -> s {nextToken = a} :: DescribeMatchmakingRuleSets)

instance Core.AWSPager DescribeMatchmakingRuleSets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeMatchmakingRuleSetsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. describeMatchmakingRuleSetsResponse_ruleSets
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeMatchmakingRuleSets_nextToken
              Lens..~ rs
              Lens.^? describeMatchmakingRuleSetsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest DescribeMatchmakingRuleSets where
  type
    AWSResponse DescribeMatchmakingRuleSets =
      DescribeMatchmakingRuleSetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMatchmakingRuleSetsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "RuleSets" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable DescribeMatchmakingRuleSets where
  hashWithSalt _salt DescribeMatchmakingRuleSets' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` names
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeMatchmakingRuleSets where
  rnf DescribeMatchmakingRuleSets' {..} =
    Prelude.rnf limit `Prelude.seq`
      Prelude.rnf names `Prelude.seq`
        Prelude.rnf nextToken

instance Data.ToHeaders DescribeMatchmakingRuleSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.DescribeMatchmakingRuleSets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeMatchmakingRuleSets where
  toJSON DescribeMatchmakingRuleSets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("Names" Data..=) Prelude.<$> names,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeMatchmakingRuleSets where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeMatchmakingRuleSets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMatchmakingRuleSetsResponse' smart constructor.
data DescribeMatchmakingRuleSetsResponse = DescribeMatchmakingRuleSetsResponse'
  { -- | A token that indicates where to resume retrieving results on the next
    -- call to this operation. If no token is returned, these results represent
    -- the end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A collection of requested matchmaking rule set objects.
    ruleSets :: [MatchmakingRuleSet]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMatchmakingRuleSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeMatchmakingRuleSetsResponse_nextToken' - A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
--
-- 'httpStatus', 'describeMatchmakingRuleSetsResponse_httpStatus' - The response's http status code.
--
-- 'ruleSets', 'describeMatchmakingRuleSetsResponse_ruleSets' - A collection of requested matchmaking rule set objects.
newDescribeMatchmakingRuleSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeMatchmakingRuleSetsResponse
newDescribeMatchmakingRuleSetsResponse pHttpStatus_ =
  DescribeMatchmakingRuleSetsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      ruleSets = Prelude.mempty
    }

-- | A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
describeMatchmakingRuleSetsResponse_nextToken :: Lens.Lens' DescribeMatchmakingRuleSetsResponse (Prelude.Maybe Prelude.Text)
describeMatchmakingRuleSetsResponse_nextToken = Lens.lens (\DescribeMatchmakingRuleSetsResponse' {nextToken} -> nextToken) (\s@DescribeMatchmakingRuleSetsResponse' {} a -> s {nextToken = a} :: DescribeMatchmakingRuleSetsResponse)

-- | The response's http status code.
describeMatchmakingRuleSetsResponse_httpStatus :: Lens.Lens' DescribeMatchmakingRuleSetsResponse Prelude.Int
describeMatchmakingRuleSetsResponse_httpStatus = Lens.lens (\DescribeMatchmakingRuleSetsResponse' {httpStatus} -> httpStatus) (\s@DescribeMatchmakingRuleSetsResponse' {} a -> s {httpStatus = a} :: DescribeMatchmakingRuleSetsResponse)

-- | A collection of requested matchmaking rule set objects.
describeMatchmakingRuleSetsResponse_ruleSets :: Lens.Lens' DescribeMatchmakingRuleSetsResponse [MatchmakingRuleSet]
describeMatchmakingRuleSetsResponse_ruleSets = Lens.lens (\DescribeMatchmakingRuleSetsResponse' {ruleSets} -> ruleSets) (\s@DescribeMatchmakingRuleSetsResponse' {} a -> s {ruleSets = a} :: DescribeMatchmakingRuleSetsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    DescribeMatchmakingRuleSetsResponse
  where
  rnf DescribeMatchmakingRuleSetsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf ruleSets
