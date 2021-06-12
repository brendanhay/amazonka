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
-- Module      : Network.AWS.GameLift.DescribeMatchmakingConfigurations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of FlexMatch matchmaking configurations.
--
-- This operation offers the following options: (1) retrieve all
-- matchmaking configurations, (2) retrieve configurations for a specified
-- list, or (3) retrieve all configurations that use a specified rule set
-- name. When requesting multiple items, use the pagination parameters to
-- retrieve results as a set of sequential pages.
--
-- If successful, a configuration is returned for each requested name. When
-- specifying a list of names, only configurations that currently exist are
-- returned.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/matchmaker-build.html Setting Up FlexMatch Matchmakers>
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
module Network.AWS.GameLift.DescribeMatchmakingConfigurations
  ( -- * Creating a Request
    DescribeMatchmakingConfigurations (..),
    newDescribeMatchmakingConfigurations,

    -- * Request Lenses
    describeMatchmakingConfigurations_names,
    describeMatchmakingConfigurations_nextToken,
    describeMatchmakingConfigurations_ruleSetName,
    describeMatchmakingConfigurations_limit,

    -- * Destructuring the Response
    DescribeMatchmakingConfigurationsResponse (..),
    newDescribeMatchmakingConfigurationsResponse,

    -- * Response Lenses
    describeMatchmakingConfigurationsResponse_nextToken,
    describeMatchmakingConfigurationsResponse_configurations,
    describeMatchmakingConfigurationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDescribeMatchmakingConfigurations' smart constructor.
data DescribeMatchmakingConfigurations = DescribeMatchmakingConfigurations'
  { -- | A unique identifier for a matchmaking configuration(s) to retrieve. You
    -- can use either the configuration name or ARN value. To request all
    -- existing configurations, leave this parameter empty.
    names :: Core.Maybe [Core.Text],
    -- | A token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Core.Maybe Core.Text,
    -- | A unique identifier for a matchmaking rule set. You can use either the
    -- rule set name or ARN value. Use this parameter to retrieve all
    -- matchmaking configurations that use this rule set.
    ruleSetName :: Core.Maybe Core.Text,
    -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages. This parameter
    -- is limited to 10.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMatchmakingConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'names', 'describeMatchmakingConfigurations_names' - A unique identifier for a matchmaking configuration(s) to retrieve. You
-- can use either the configuration name or ARN value. To request all
-- existing configurations, leave this parameter empty.
--
-- 'nextToken', 'describeMatchmakingConfigurations_nextToken' - A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
--
-- 'ruleSetName', 'describeMatchmakingConfigurations_ruleSetName' - A unique identifier for a matchmaking rule set. You can use either the
-- rule set name or ARN value. Use this parameter to retrieve all
-- matchmaking configurations that use this rule set.
--
-- 'limit', 'describeMatchmakingConfigurations_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages. This parameter
-- is limited to 10.
newDescribeMatchmakingConfigurations ::
  DescribeMatchmakingConfigurations
newDescribeMatchmakingConfigurations =
  DescribeMatchmakingConfigurations'
    { names =
        Core.Nothing,
      nextToken = Core.Nothing,
      ruleSetName = Core.Nothing,
      limit = Core.Nothing
    }

-- | A unique identifier for a matchmaking configuration(s) to retrieve. You
-- can use either the configuration name or ARN value. To request all
-- existing configurations, leave this parameter empty.
describeMatchmakingConfigurations_names :: Lens.Lens' DescribeMatchmakingConfigurations (Core.Maybe [Core.Text])
describeMatchmakingConfigurations_names = Lens.lens (\DescribeMatchmakingConfigurations' {names} -> names) (\s@DescribeMatchmakingConfigurations' {} a -> s {names = a} :: DescribeMatchmakingConfigurations) Core.. Lens.mapping Lens._Coerce

-- | A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
describeMatchmakingConfigurations_nextToken :: Lens.Lens' DescribeMatchmakingConfigurations (Core.Maybe Core.Text)
describeMatchmakingConfigurations_nextToken = Lens.lens (\DescribeMatchmakingConfigurations' {nextToken} -> nextToken) (\s@DescribeMatchmakingConfigurations' {} a -> s {nextToken = a} :: DescribeMatchmakingConfigurations)

-- | A unique identifier for a matchmaking rule set. You can use either the
-- rule set name or ARN value. Use this parameter to retrieve all
-- matchmaking configurations that use this rule set.
describeMatchmakingConfigurations_ruleSetName :: Lens.Lens' DescribeMatchmakingConfigurations (Core.Maybe Core.Text)
describeMatchmakingConfigurations_ruleSetName = Lens.lens (\DescribeMatchmakingConfigurations' {ruleSetName} -> ruleSetName) (\s@DescribeMatchmakingConfigurations' {} a -> s {ruleSetName = a} :: DescribeMatchmakingConfigurations)

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages. This parameter
-- is limited to 10.
describeMatchmakingConfigurations_limit :: Lens.Lens' DescribeMatchmakingConfigurations (Core.Maybe Core.Natural)
describeMatchmakingConfigurations_limit = Lens.lens (\DescribeMatchmakingConfigurations' {limit} -> limit) (\s@DescribeMatchmakingConfigurations' {} a -> s {limit = a} :: DescribeMatchmakingConfigurations)

instance
  Core.AWSPager
    DescribeMatchmakingConfigurations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeMatchmakingConfigurationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeMatchmakingConfigurationsResponse_configurations
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeMatchmakingConfigurations_nextToken
          Lens..~ rs
          Lens.^? describeMatchmakingConfigurationsResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeMatchmakingConfigurations
  where
  type
    AWSResponse DescribeMatchmakingConfigurations =
      DescribeMatchmakingConfigurationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMatchmakingConfigurationsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Configurations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeMatchmakingConfigurations

instance
  Core.NFData
    DescribeMatchmakingConfigurations

instance
  Core.ToHeaders
    DescribeMatchmakingConfigurations
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.DescribeMatchmakingConfigurations" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeMatchmakingConfigurations
  where
  toJSON DescribeMatchmakingConfigurations' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Names" Core..=) Core.<$> names,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("RuleSetName" Core..=) Core.<$> ruleSetName,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance
  Core.ToPath
    DescribeMatchmakingConfigurations
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeMatchmakingConfigurations
  where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeMatchmakingConfigurationsResponse' smart constructor.
data DescribeMatchmakingConfigurationsResponse = DescribeMatchmakingConfigurationsResponse'
  { -- | A token that indicates where to resume retrieving results on the next
    -- call to this operation. If no token is returned, these results represent
    -- the end of the list.
    nextToken :: Core.Maybe Core.Text,
    -- | A collection of requested matchmaking configurations.
    configurations :: Core.Maybe [MatchmakingConfiguration],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMatchmakingConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeMatchmakingConfigurationsResponse_nextToken' - A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
--
-- 'configurations', 'describeMatchmakingConfigurationsResponse_configurations' - A collection of requested matchmaking configurations.
--
-- 'httpStatus', 'describeMatchmakingConfigurationsResponse_httpStatus' - The response's http status code.
newDescribeMatchmakingConfigurationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeMatchmakingConfigurationsResponse
newDescribeMatchmakingConfigurationsResponse
  pHttpStatus_ =
    DescribeMatchmakingConfigurationsResponse'
      { nextToken =
          Core.Nothing,
        configurations = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
describeMatchmakingConfigurationsResponse_nextToken :: Lens.Lens' DescribeMatchmakingConfigurationsResponse (Core.Maybe Core.Text)
describeMatchmakingConfigurationsResponse_nextToken = Lens.lens (\DescribeMatchmakingConfigurationsResponse' {nextToken} -> nextToken) (\s@DescribeMatchmakingConfigurationsResponse' {} a -> s {nextToken = a} :: DescribeMatchmakingConfigurationsResponse)

-- | A collection of requested matchmaking configurations.
describeMatchmakingConfigurationsResponse_configurations :: Lens.Lens' DescribeMatchmakingConfigurationsResponse (Core.Maybe [MatchmakingConfiguration])
describeMatchmakingConfigurationsResponse_configurations = Lens.lens (\DescribeMatchmakingConfigurationsResponse' {configurations} -> configurations) (\s@DescribeMatchmakingConfigurationsResponse' {} a -> s {configurations = a} :: DescribeMatchmakingConfigurationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeMatchmakingConfigurationsResponse_httpStatus :: Lens.Lens' DescribeMatchmakingConfigurationsResponse Core.Int
describeMatchmakingConfigurationsResponse_httpStatus = Lens.lens (\DescribeMatchmakingConfigurationsResponse' {httpStatus} -> httpStatus) (\s@DescribeMatchmakingConfigurationsResponse' {} a -> s {httpStatus = a} :: DescribeMatchmakingConfigurationsResponse)

instance
  Core.NFData
    DescribeMatchmakingConfigurationsResponse
