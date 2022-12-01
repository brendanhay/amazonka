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
-- Module      : Amazonka.CloudWatch.DescribeInsightRules
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all the Contributor Insights rules in your account.
--
-- For more information about Contributor Insights, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights.html Using Contributor Insights to Analyze High-Cardinality Data>.
module Amazonka.CloudWatch.DescribeInsightRules
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

import Amazonka.CloudWatch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeInsightRules' smart constructor.
data DescribeInsightRules = DescribeInsightRules'
  { -- | Include this value, if it was returned by the previous operation, to get
    -- the next set of rules.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in one operation. If you omit
    -- this parameter, the default of 500 is used.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Include this value, if it was returned by the previous operation, to get
-- the next set of rules.
describeInsightRules_nextToken :: Lens.Lens' DescribeInsightRules (Prelude.Maybe Prelude.Text)
describeInsightRules_nextToken = Lens.lens (\DescribeInsightRules' {nextToken} -> nextToken) (\s@DescribeInsightRules' {} a -> s {nextToken = a} :: DescribeInsightRules)

-- | The maximum number of results to return in one operation. If you omit
-- this parameter, the default of 500 is used.
describeInsightRules_maxResults :: Lens.Lens' DescribeInsightRules (Prelude.Maybe Prelude.Natural)
describeInsightRules_maxResults = Lens.lens (\DescribeInsightRules' {maxResults} -> maxResults) (\s@DescribeInsightRules' {} a -> s {maxResults = a} :: DescribeInsightRules)

instance Core.AWSRequest DescribeInsightRules where
  type
    AWSResponse DescribeInsightRules =
      DescribeInsightRulesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeInsightRulesResult"
      ( \s h x ->
          DescribeInsightRulesResponse'
            Prelude.<$> (x Core..@? "NextToken")
            Prelude.<*> ( x Core..@? "InsightRules" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInsightRules where
  hashWithSalt _salt DescribeInsightRules' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeInsightRules where
  rnf DescribeInsightRules' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribeInsightRules where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeInsightRules where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeInsightRules where
  toQuery DescribeInsightRules' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeInsightRules" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-08-01" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newDescribeInsightRulesResponse' smart constructor.
data DescribeInsightRulesResponse = DescribeInsightRulesResponse'
  { -- | If this parameter is present, it is a token that marks the start of the
    -- next batch of returned results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The rules returned by the operation.
    insightRules :: Prelude.Maybe [InsightRule],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeInsightRulesResponse
newDescribeInsightRulesResponse pHttpStatus_ =
  DescribeInsightRulesResponse'
    { nextToken =
        Prelude.Nothing,
      insightRules = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If this parameter is present, it is a token that marks the start of the
-- next batch of returned results.
describeInsightRulesResponse_nextToken :: Lens.Lens' DescribeInsightRulesResponse (Prelude.Maybe Prelude.Text)
describeInsightRulesResponse_nextToken = Lens.lens (\DescribeInsightRulesResponse' {nextToken} -> nextToken) (\s@DescribeInsightRulesResponse' {} a -> s {nextToken = a} :: DescribeInsightRulesResponse)

-- | The rules returned by the operation.
describeInsightRulesResponse_insightRules :: Lens.Lens' DescribeInsightRulesResponse (Prelude.Maybe [InsightRule])
describeInsightRulesResponse_insightRules = Lens.lens (\DescribeInsightRulesResponse' {insightRules} -> insightRules) (\s@DescribeInsightRulesResponse' {} a -> s {insightRules = a} :: DescribeInsightRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeInsightRulesResponse_httpStatus :: Lens.Lens' DescribeInsightRulesResponse Prelude.Int
describeInsightRulesResponse_httpStatus = Lens.lens (\DescribeInsightRulesResponse' {httpStatus} -> httpStatus) (\s@DescribeInsightRulesResponse' {} a -> s {httpStatus = a} :: DescribeInsightRulesResponse)

instance Prelude.NFData DescribeInsightRulesResponse where
  rnf DescribeInsightRulesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf insightRules
      `Prelude.seq` Prelude.rnf httpStatus
