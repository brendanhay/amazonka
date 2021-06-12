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
-- Module      : Network.AWS.CloudWatch.DescribeInsightRules
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all the Contributor Insights rules in your account.
--
-- For more information about Contributor Insights, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights.html Using Contributor Insights to Analyze High-Cardinality Data>.
module Network.AWS.CloudWatch.DescribeInsightRules
  ( -- * Creating a Request
    DescribeInsightRules (..),
    newDescribeInsightRules,

    -- * Request Lenses
    describeInsightRules_nextToken,
    describeInsightRules_maxResults,

    -- * Destructuring the Response
    DescribeInsightRulesResponse (..),
    newDescribeInsightRulesResponse,

    -- * Response Lenses
    describeInsightRulesResponse_nextToken,
    describeInsightRulesResponse_insightRules,
    describeInsightRulesResponse_httpStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeInsightRules' smart constructor.
data DescribeInsightRules = DescribeInsightRules'
  { -- | Include this value, if it was returned by the previous operation, to get
    -- the next set of rules.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in one operation. If you omit
    -- this parameter, the default of 500 is used.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInsightRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInsightRules_nextToken' - Include this value, if it was returned by the previous operation, to get
-- the next set of rules.
--
-- 'maxResults', 'describeInsightRules_maxResults' - The maximum number of results to return in one operation. If you omit
-- this parameter, the default of 500 is used.
newDescribeInsightRules ::
  DescribeInsightRules
newDescribeInsightRules =
  DescribeInsightRules'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | Include this value, if it was returned by the previous operation, to get
-- the next set of rules.
describeInsightRules_nextToken :: Lens.Lens' DescribeInsightRules (Core.Maybe Core.Text)
describeInsightRules_nextToken = Lens.lens (\DescribeInsightRules' {nextToken} -> nextToken) (\s@DescribeInsightRules' {} a -> s {nextToken = a} :: DescribeInsightRules)

-- | The maximum number of results to return in one operation. If you omit
-- this parameter, the default of 500 is used.
describeInsightRules_maxResults :: Lens.Lens' DescribeInsightRules (Core.Maybe Core.Natural)
describeInsightRules_maxResults = Lens.lens (\DescribeInsightRules' {maxResults} -> maxResults) (\s@DescribeInsightRules' {} a -> s {maxResults = a} :: DescribeInsightRules)

instance Core.AWSRequest DescribeInsightRules where
  type
    AWSResponse DescribeInsightRules =
      DescribeInsightRulesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeInsightRulesResult"
      ( \s h x ->
          DescribeInsightRulesResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> ( x Core..@? "InsightRules" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeInsightRules

instance Core.NFData DescribeInsightRules

instance Core.ToHeaders DescribeInsightRules where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeInsightRules where
  toPath = Core.const "/"

instance Core.ToQuery DescribeInsightRules where
  toQuery DescribeInsightRules' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeInsightRules" :: Core.ByteString),
        "Version" Core.=: ("2010-08-01" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newDescribeInsightRulesResponse' smart constructor.
data DescribeInsightRulesResponse = DescribeInsightRulesResponse'
  { -- | If this parameter is present, it is a token that marks the start of the
    -- next batch of returned results.
    nextToken :: Core.Maybe Core.Text,
    -- | The rules returned by the operation.
    insightRules :: Core.Maybe [InsightRule],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInsightRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInsightRulesResponse_nextToken' - If this parameter is present, it is a token that marks the start of the
-- next batch of returned results.
--
-- 'insightRules', 'describeInsightRulesResponse_insightRules' - The rules returned by the operation.
--
-- 'httpStatus', 'describeInsightRulesResponse_httpStatus' - The response's http status code.
newDescribeInsightRulesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeInsightRulesResponse
newDescribeInsightRulesResponse pHttpStatus_ =
  DescribeInsightRulesResponse'
    { nextToken =
        Core.Nothing,
      insightRules = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If this parameter is present, it is a token that marks the start of the
-- next batch of returned results.
describeInsightRulesResponse_nextToken :: Lens.Lens' DescribeInsightRulesResponse (Core.Maybe Core.Text)
describeInsightRulesResponse_nextToken = Lens.lens (\DescribeInsightRulesResponse' {nextToken} -> nextToken) (\s@DescribeInsightRulesResponse' {} a -> s {nextToken = a} :: DescribeInsightRulesResponse)

-- | The rules returned by the operation.
describeInsightRulesResponse_insightRules :: Lens.Lens' DescribeInsightRulesResponse (Core.Maybe [InsightRule])
describeInsightRulesResponse_insightRules = Lens.lens (\DescribeInsightRulesResponse' {insightRules} -> insightRules) (\s@DescribeInsightRulesResponse' {} a -> s {insightRules = a} :: DescribeInsightRulesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeInsightRulesResponse_httpStatus :: Lens.Lens' DescribeInsightRulesResponse Core.Int
describeInsightRulesResponse_httpStatus = Lens.lens (\DescribeInsightRulesResponse' {httpStatus} -> httpStatus) (\s@DescribeInsightRulesResponse' {} a -> s {httpStatus = a} :: DescribeInsightRulesResponse)

instance Core.NFData DescribeInsightRulesResponse
