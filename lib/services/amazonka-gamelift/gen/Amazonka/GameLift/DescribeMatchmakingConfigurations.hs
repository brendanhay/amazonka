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
-- Module      : Amazonka.GameLift.DescribeMatchmakingConfigurations
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/matchmaker-build.html Setting up FlexMatch matchmakers>
--
-- This operation returns paginated results.
module Amazonka.GameLift.DescribeMatchmakingConfigurations
  ( -- * Creating a Request
    DescribeMatchmakingConfigurations (..),
    newDescribeMatchmakingConfigurations,

    -- * Request Lenses
    describeMatchmakingConfigurations_limit,
    describeMatchmakingConfigurations_names,
    describeMatchmakingConfigurations_nextToken,
    describeMatchmakingConfigurations_ruleSetName,

    -- * Destructuring the Response
    DescribeMatchmakingConfigurationsResponse (..),
    newDescribeMatchmakingConfigurationsResponse,

    -- * Response Lenses
    describeMatchmakingConfigurationsResponse_configurations,
    describeMatchmakingConfigurationsResponse_nextToken,
    describeMatchmakingConfigurationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeMatchmakingConfigurations' smart constructor.
data DescribeMatchmakingConfigurations = DescribeMatchmakingConfigurations'
  { -- | The maximum number of results to return. Use this parameter with
    -- @NextToken@ to get results as a set of sequential pages. This parameter
    -- is limited to 10.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | A unique identifier for the matchmaking configuration(s) to retrieve.
    -- You can use either the configuration name or ARN value. To request all
    -- existing configurations, leave this parameter empty.
    names :: Prelude.Maybe [Prelude.Text],
    -- | A token that indicates the start of the next sequential page of results.
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the matchmaking rule set. You can use either the
    -- rule set name or ARN value. Use this parameter to retrieve all
    -- matchmaking configurations that use this rule set.
    ruleSetName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMatchmakingConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'describeMatchmakingConfigurations_limit' - The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages. This parameter
-- is limited to 10.
--
-- 'names', 'describeMatchmakingConfigurations_names' - A unique identifier for the matchmaking configuration(s) to retrieve.
-- You can use either the configuration name or ARN value. To request all
-- existing configurations, leave this parameter empty.
--
-- 'nextToken', 'describeMatchmakingConfigurations_nextToken' - A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
--
-- 'ruleSetName', 'describeMatchmakingConfigurations_ruleSetName' - A unique identifier for the matchmaking rule set. You can use either the
-- rule set name or ARN value. Use this parameter to retrieve all
-- matchmaking configurations that use this rule set.
newDescribeMatchmakingConfigurations ::
  DescribeMatchmakingConfigurations
newDescribeMatchmakingConfigurations =
  DescribeMatchmakingConfigurations'
    { limit =
        Prelude.Nothing,
      names = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      ruleSetName = Prelude.Nothing
    }

-- | The maximum number of results to return. Use this parameter with
-- @NextToken@ to get results as a set of sequential pages. This parameter
-- is limited to 10.
describeMatchmakingConfigurations_limit :: Lens.Lens' DescribeMatchmakingConfigurations (Prelude.Maybe Prelude.Natural)
describeMatchmakingConfigurations_limit = Lens.lens (\DescribeMatchmakingConfigurations' {limit} -> limit) (\s@DescribeMatchmakingConfigurations' {} a -> s {limit = a} :: DescribeMatchmakingConfigurations)

-- | A unique identifier for the matchmaking configuration(s) to retrieve.
-- You can use either the configuration name or ARN value. To request all
-- existing configurations, leave this parameter empty.
describeMatchmakingConfigurations_names :: Lens.Lens' DescribeMatchmakingConfigurations (Prelude.Maybe [Prelude.Text])
describeMatchmakingConfigurations_names = Lens.lens (\DescribeMatchmakingConfigurations' {names} -> names) (\s@DescribeMatchmakingConfigurations' {} a -> s {names = a} :: DescribeMatchmakingConfigurations) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates the start of the next sequential page of results.
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
describeMatchmakingConfigurations_nextToken :: Lens.Lens' DescribeMatchmakingConfigurations (Prelude.Maybe Prelude.Text)
describeMatchmakingConfigurations_nextToken = Lens.lens (\DescribeMatchmakingConfigurations' {nextToken} -> nextToken) (\s@DescribeMatchmakingConfigurations' {} a -> s {nextToken = a} :: DescribeMatchmakingConfigurations)

-- | A unique identifier for the matchmaking rule set. You can use either the
-- rule set name or ARN value. Use this parameter to retrieve all
-- matchmaking configurations that use this rule set.
describeMatchmakingConfigurations_ruleSetName :: Lens.Lens' DescribeMatchmakingConfigurations (Prelude.Maybe Prelude.Text)
describeMatchmakingConfigurations_ruleSetName = Lens.lens (\DescribeMatchmakingConfigurations' {ruleSetName} -> ruleSetName) (\s@DescribeMatchmakingConfigurations' {} a -> s {ruleSetName = a} :: DescribeMatchmakingConfigurations)

instance
  Core.AWSPager
    DescribeMatchmakingConfigurations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeMatchmakingConfigurationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeMatchmakingConfigurationsResponse_configurations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeMatchmakingConfigurations_nextToken
          Lens..~ rs
          Lens.^? describeMatchmakingConfigurationsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeMatchmakingConfigurations
  where
  type
    AWSResponse DescribeMatchmakingConfigurations =
      DescribeMatchmakingConfigurationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMatchmakingConfigurationsResponse'
            Prelude.<$> (x Data..?> "Configurations" Core..!@ Prelude.mempty)
              Prelude.<*> (x Data..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeMatchmakingConfigurations
  where
  hashWithSalt
    _salt
    DescribeMatchmakingConfigurations' {..} =
      _salt `Prelude.hashWithSalt` limit
        `Prelude.hashWithSalt` names
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` ruleSetName

instance
  Prelude.NFData
    DescribeMatchmakingConfigurations
  where
  rnf DescribeMatchmakingConfigurations' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf names
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf ruleSetName

instance
  Data.ToHeaders
    DescribeMatchmakingConfigurations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.DescribeMatchmakingConfigurations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeMatchmakingConfigurations
  where
  toJSON DescribeMatchmakingConfigurations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("Names" Data..=) Prelude.<$> names,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("RuleSetName" Data..=) Prelude.<$> ruleSetName
          ]
      )

instance
  Data.ToPath
    DescribeMatchmakingConfigurations
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeMatchmakingConfigurations
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMatchmakingConfigurationsResponse' smart constructor.
data DescribeMatchmakingConfigurationsResponse = DescribeMatchmakingConfigurationsResponse'
  { -- | A collection of requested matchmaking configurations.
    configurations :: Prelude.Maybe [MatchmakingConfiguration],
    -- | A token that indicates where to resume retrieving results on the next
    -- call to this operation. If no token is returned, these results represent
    -- the end of the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMatchmakingConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurations', 'describeMatchmakingConfigurationsResponse_configurations' - A collection of requested matchmaking configurations.
--
-- 'nextToken', 'describeMatchmakingConfigurationsResponse_nextToken' - A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
--
-- 'httpStatus', 'describeMatchmakingConfigurationsResponse_httpStatus' - The response's http status code.
newDescribeMatchmakingConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeMatchmakingConfigurationsResponse
newDescribeMatchmakingConfigurationsResponse
  pHttpStatus_ =
    DescribeMatchmakingConfigurationsResponse'
      { configurations =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A collection of requested matchmaking configurations.
describeMatchmakingConfigurationsResponse_configurations :: Lens.Lens' DescribeMatchmakingConfigurationsResponse (Prelude.Maybe [MatchmakingConfiguration])
describeMatchmakingConfigurationsResponse_configurations = Lens.lens (\DescribeMatchmakingConfigurationsResponse' {configurations} -> configurations) (\s@DescribeMatchmakingConfigurationsResponse' {} a -> s {configurations = a} :: DescribeMatchmakingConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates where to resume retrieving results on the next
-- call to this operation. If no token is returned, these results represent
-- the end of the list.
describeMatchmakingConfigurationsResponse_nextToken :: Lens.Lens' DescribeMatchmakingConfigurationsResponse (Prelude.Maybe Prelude.Text)
describeMatchmakingConfigurationsResponse_nextToken = Lens.lens (\DescribeMatchmakingConfigurationsResponse' {nextToken} -> nextToken) (\s@DescribeMatchmakingConfigurationsResponse' {} a -> s {nextToken = a} :: DescribeMatchmakingConfigurationsResponse)

-- | The response's http status code.
describeMatchmakingConfigurationsResponse_httpStatus :: Lens.Lens' DescribeMatchmakingConfigurationsResponse Prelude.Int
describeMatchmakingConfigurationsResponse_httpStatus = Lens.lens (\DescribeMatchmakingConfigurationsResponse' {httpStatus} -> httpStatus) (\s@DescribeMatchmakingConfigurationsResponse' {} a -> s {httpStatus = a} :: DescribeMatchmakingConfigurationsResponse)

instance
  Prelude.NFData
    DescribeMatchmakingConfigurationsResponse
  where
  rnf DescribeMatchmakingConfigurationsResponse' {..} =
    Prelude.rnf configurations
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
