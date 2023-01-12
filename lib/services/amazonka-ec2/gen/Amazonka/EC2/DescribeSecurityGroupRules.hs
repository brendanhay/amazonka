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
-- Module      : Amazonka.EC2.DescribeSecurityGroupRules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your security group rules.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeSecurityGroupRules
  ( -- * Creating a Request
    DescribeSecurityGroupRules (..),
    newDescribeSecurityGroupRules,

    -- * Request Lenses
    describeSecurityGroupRules_dryRun,
    describeSecurityGroupRules_filters,
    describeSecurityGroupRules_maxResults,
    describeSecurityGroupRules_nextToken,
    describeSecurityGroupRules_securityGroupRuleIds,

    -- * Destructuring the Response
    DescribeSecurityGroupRulesResponse (..),
    newDescribeSecurityGroupRulesResponse,

    -- * Response Lenses
    describeSecurityGroupRulesResponse_nextToken,
    describeSecurityGroupRulesResponse_securityGroupRules,
    describeSecurityGroupRulesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeSecurityGroupRules' smart constructor.
data DescribeSecurityGroupRules = DescribeSecurityGroupRules'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters.
    --
    -- -   @group-id@ - The ID of the security group.
    --
    -- -   @security-group-rule-id@ - The ID of the security group rule.
    --
    -- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
    --     resource. Use the tag key in the filter name and the tag value as
    --     the filter value. For example, to find all resources that have a tag
    --     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
    --     the filter name and @TeamA@ for the filter value.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another request with the returned
    -- @NextToken@ value. This value can be between 5 and 1000. If this
    -- parameter is not specified, then all results are returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the security group rules.
    securityGroupRuleIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSecurityGroupRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeSecurityGroupRules_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeSecurityGroupRules_filters' - One or more filters.
--
-- -   @group-id@ - The ID of the security group.
--
-- -   @security-group-rule-id@ - The ID of the security group rule.
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- 'maxResults', 'describeSecurityGroupRules_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another request with the returned
-- @NextToken@ value. This value can be between 5 and 1000. If this
-- parameter is not specified, then all results are returned.
--
-- 'nextToken', 'describeSecurityGroupRules_nextToken' - The token for the next page of results.
--
-- 'securityGroupRuleIds', 'describeSecurityGroupRules_securityGroupRuleIds' - The IDs of the security group rules.
newDescribeSecurityGroupRules ::
  DescribeSecurityGroupRules
newDescribeSecurityGroupRules =
  DescribeSecurityGroupRules'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      securityGroupRuleIds = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeSecurityGroupRules_dryRun :: Lens.Lens' DescribeSecurityGroupRules (Prelude.Maybe Prelude.Bool)
describeSecurityGroupRules_dryRun = Lens.lens (\DescribeSecurityGroupRules' {dryRun} -> dryRun) (\s@DescribeSecurityGroupRules' {} a -> s {dryRun = a} :: DescribeSecurityGroupRules)

-- | One or more filters.
--
-- -   @group-id@ - The ID of the security group.
--
-- -   @security-group-rule-id@ - The ID of the security group rule.
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
describeSecurityGroupRules_filters :: Lens.Lens' DescribeSecurityGroupRules (Prelude.Maybe [Filter])
describeSecurityGroupRules_filters = Lens.lens (\DescribeSecurityGroupRules' {filters} -> filters) (\s@DescribeSecurityGroupRules' {} a -> s {filters = a} :: DescribeSecurityGroupRules) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another request with the returned
-- @NextToken@ value. This value can be between 5 and 1000. If this
-- parameter is not specified, then all results are returned.
describeSecurityGroupRules_maxResults :: Lens.Lens' DescribeSecurityGroupRules (Prelude.Maybe Prelude.Natural)
describeSecurityGroupRules_maxResults = Lens.lens (\DescribeSecurityGroupRules' {maxResults} -> maxResults) (\s@DescribeSecurityGroupRules' {} a -> s {maxResults = a} :: DescribeSecurityGroupRules)

-- | The token for the next page of results.
describeSecurityGroupRules_nextToken :: Lens.Lens' DescribeSecurityGroupRules (Prelude.Maybe Prelude.Text)
describeSecurityGroupRules_nextToken = Lens.lens (\DescribeSecurityGroupRules' {nextToken} -> nextToken) (\s@DescribeSecurityGroupRules' {} a -> s {nextToken = a} :: DescribeSecurityGroupRules)

-- | The IDs of the security group rules.
describeSecurityGroupRules_securityGroupRuleIds :: Lens.Lens' DescribeSecurityGroupRules (Prelude.Maybe [Prelude.Text])
describeSecurityGroupRules_securityGroupRuleIds = Lens.lens (\DescribeSecurityGroupRules' {securityGroupRuleIds} -> securityGroupRuleIds) (\s@DescribeSecurityGroupRules' {} a -> s {securityGroupRuleIds = a} :: DescribeSecurityGroupRules) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeSecurityGroupRules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSecurityGroupRulesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSecurityGroupRulesResponse_securityGroupRules
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeSecurityGroupRules_nextToken
          Lens..~ rs
          Lens.^? describeSecurityGroupRulesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeSecurityGroupRules where
  type
    AWSResponse DescribeSecurityGroupRules =
      DescribeSecurityGroupRulesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSecurityGroupRulesResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x Data..@? "securityGroupRuleSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSecurityGroupRules where
  hashWithSalt _salt DescribeSecurityGroupRules' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` securityGroupRuleIds

instance Prelude.NFData DescribeSecurityGroupRules where
  rnf DescribeSecurityGroupRules' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf securityGroupRuleIds

instance Data.ToHeaders DescribeSecurityGroupRules where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeSecurityGroupRules where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSecurityGroupRules where
  toQuery DescribeSecurityGroupRules' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeSecurityGroupRules" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        Data.toQuery
          ( Data.toQueryList "SecurityGroupRuleId"
              Prelude.<$> securityGroupRuleIds
          )
      ]

-- | /See:/ 'newDescribeSecurityGroupRulesResponse' smart constructor.
data DescribeSecurityGroupRulesResponse = DescribeSecurityGroupRulesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about security group rules.
    securityGroupRules :: Prelude.Maybe [SecurityGroupRule],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSecurityGroupRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSecurityGroupRulesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'securityGroupRules', 'describeSecurityGroupRulesResponse_securityGroupRules' - Information about security group rules.
--
-- 'httpStatus', 'describeSecurityGroupRulesResponse_httpStatus' - The response's http status code.
newDescribeSecurityGroupRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSecurityGroupRulesResponse
newDescribeSecurityGroupRulesResponse pHttpStatus_ =
  DescribeSecurityGroupRulesResponse'
    { nextToken =
        Prelude.Nothing,
      securityGroupRules = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeSecurityGroupRulesResponse_nextToken :: Lens.Lens' DescribeSecurityGroupRulesResponse (Prelude.Maybe Prelude.Text)
describeSecurityGroupRulesResponse_nextToken = Lens.lens (\DescribeSecurityGroupRulesResponse' {nextToken} -> nextToken) (\s@DescribeSecurityGroupRulesResponse' {} a -> s {nextToken = a} :: DescribeSecurityGroupRulesResponse)

-- | Information about security group rules.
describeSecurityGroupRulesResponse_securityGroupRules :: Lens.Lens' DescribeSecurityGroupRulesResponse (Prelude.Maybe [SecurityGroupRule])
describeSecurityGroupRulesResponse_securityGroupRules = Lens.lens (\DescribeSecurityGroupRulesResponse' {securityGroupRules} -> securityGroupRules) (\s@DescribeSecurityGroupRulesResponse' {} a -> s {securityGroupRules = a} :: DescribeSecurityGroupRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSecurityGroupRulesResponse_httpStatus :: Lens.Lens' DescribeSecurityGroupRulesResponse Prelude.Int
describeSecurityGroupRulesResponse_httpStatus = Lens.lens (\DescribeSecurityGroupRulesResponse' {httpStatus} -> httpStatus) (\s@DescribeSecurityGroupRulesResponse' {} a -> s {httpStatus = a} :: DescribeSecurityGroupRulesResponse)

instance
  Prelude.NFData
    DescribeSecurityGroupRulesResponse
  where
  rnf DescribeSecurityGroupRulesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf securityGroupRules
      `Prelude.seq` Prelude.rnf httpStatus
