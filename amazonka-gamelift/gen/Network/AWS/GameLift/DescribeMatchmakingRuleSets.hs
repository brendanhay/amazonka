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
-- Module      : Network.AWS.GameLift.DescribeMatchmakingRuleSets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
-- -   <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-rulesets.html Build a Rule Set>
--
-- __Related operations__
--
-- -   CreateMatchmakingConfiguration
--
-- -   DescribeMatchmakingConfigurations
--
-- -   UpdateMatchmakingConfiguration
--
-- -   DeleteMatchmakingConfiguration
--
-- -   CreateMatchmakingRuleSet
--
-- -   DescribeMatchmakingRuleSets
--
-- -   ValidateMatchmakingRuleSet
--
-- -   DeleteMatchmakingRuleSet
--
-- This operation returns paginated results.
module Network.AWS.GameLift.DescribeMatchmakingRuleSets
  ( -- * Creating a Request
    DescribeMatchmakingRuleSets (..),
    newDescribeMatchmakingRuleSets,

    -- * Request Lenses
    describeMatchmakingRuleSets_names,
    describeMatchmakingRuleSets_nextToken,
    describeMatchmakingRuleSets_limit,

    -- * Destructuring the Response
    DescribeMatchmakingRuleSetsResponse (..),
    newDescribeMatchmakingRuleSetsResponse,

    -- * Response Lenses
    describeMatchmakingRuleSetsResponse_nextToken,
    describeMatchmakingRuleSetsResponse_httpStatus,
    describeMatchmakingRuleSetsResponse_ruleSets,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDescribeMatchmakingRuleSets' smart constructor.
data DescribeMatchmakingRuleSets = DescribeMatchmakingRuleSets'
  { -- | A list of one or more matchmaking rule set names to retrieve details
    -- for. (Note: The rule set name is different from the optional \"name\"
    -- field in the rule set body.) You can use either the rule set name or ARN
    -- value.
    names :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages.
    limit :: Prelude.Maybe Prelude.Natural
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
-- 'names', 'describeMatchmakingRuleSets_names' - A list of one or more matchmaking rule set names to retrieve details
-- for. (Note: The rule set name is different from the optional \"name\"
-- field in the rule set body.) You can use either the rule set name or ARN
-- value.
--
-- 'nextToken', 'describeMatchmakingRuleSets_nextToken' - A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
--
-- 'limit', 'describeMatchmakingRuleSets_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
newDescribeMatchmakingRuleSets ::
  DescribeMatchmakingRuleSets
newDescribeMatchmakingRuleSets =
  DescribeMatchmakingRuleSets'
    { names =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | A list of one or more matchmaking rule set names to retrieve details
-- for. (Note: The rule set name is different from the optional \"name\"
-- field in the rule set body.) You can use either the rule set name or ARN
-- value.
describeMatchmakingRuleSets_names :: Lens.Lens' DescribeMatchmakingRuleSets (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeMatchmakingRuleSets_names = Lens.lens (\DescribeMatchmakingRuleSets' {names} -> names) (\s@DescribeMatchmakingRuleSets' {} a -> s {names = a} :: DescribeMatchmakingRuleSets) Prelude.. Lens.mapping Lens._Coerce

-- | A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
describeMatchmakingRuleSets_nextToken :: Lens.Lens' DescribeMatchmakingRuleSets (Prelude.Maybe Prelude.Text)
describeMatchmakingRuleSets_nextToken = Lens.lens (\DescribeMatchmakingRuleSets' {nextToken} -> nextToken) (\s@DescribeMatchmakingRuleSets' {} a -> s {nextToken = a} :: DescribeMatchmakingRuleSets)

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages.
describeMatchmakingRuleSets_limit :: Lens.Lens' DescribeMatchmakingRuleSets (Prelude.Maybe Prelude.Natural)
describeMatchmakingRuleSets_limit = Lens.lens (\DescribeMatchmakingRuleSets' {limit} -> limit) (\s@DescribeMatchmakingRuleSets' {} a -> s {limit = a} :: DescribeMatchmakingRuleSets)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMatchmakingRuleSetsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "RuleSets" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable DescribeMatchmakingRuleSets

instance Prelude.NFData DescribeMatchmakingRuleSets

instance Core.ToHeaders DescribeMatchmakingRuleSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.DescribeMatchmakingRuleSets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeMatchmakingRuleSets where
  toJSON DescribeMatchmakingRuleSets' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Names" Core..=) Prelude.<$> names,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Limit" Core..=) Prelude.<$> limit
          ]
      )

instance Core.ToPath DescribeMatchmakingRuleSets where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeMatchmakingRuleSets where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeMatchmakingRuleSetsResponse' smart constructor.
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
describeMatchmakingRuleSetsResponse_ruleSets = Lens.lens (\DescribeMatchmakingRuleSetsResponse' {ruleSets} -> ruleSets) (\s@DescribeMatchmakingRuleSetsResponse' {} a -> s {ruleSets = a} :: DescribeMatchmakingRuleSetsResponse) Prelude.. Lens._Coerce

instance
  Prelude.NFData
    DescribeMatchmakingRuleSetsResponse
