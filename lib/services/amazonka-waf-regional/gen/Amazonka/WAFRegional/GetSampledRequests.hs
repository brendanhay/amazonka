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
-- Module      : Amazonka.WAFRegional.GetSampledRequests
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Gets detailed information about a specified number of requests--a
-- sample--that AWS WAF randomly selects from among the first 5,000
-- requests that your AWS resource received during a time range that you
-- choose. You can specify a sample size of up to 500 requests, and you can
-- specify any time range in the previous three hours.
--
-- @GetSampledRequests@ returns a time range, which is usually the time
-- range that you specified. However, if your resource (such as a
-- CloudFront distribution) received 5,000 requests before the specified
-- time range elapsed, @GetSampledRequests@ returns an updated time range.
-- This new time range indicates the actual period during which AWS WAF
-- selected the requests in the sample.
module Amazonka.WAFRegional.GetSampledRequests
  ( -- * Creating a Request
    GetSampledRequests (..),
    newGetSampledRequests,

    -- * Request Lenses
    getSampledRequests_webAclId,
    getSampledRequests_ruleId,
    getSampledRequests_timeWindow,
    getSampledRequests_maxItems,

    -- * Destructuring the Response
    GetSampledRequestsResponse (..),
    newGetSampledRequestsResponse,

    -- * Response Lenses
    getSampledRequestsResponse_populationSize,
    getSampledRequestsResponse_sampledRequests,
    getSampledRequestsResponse_timeWindow,
    getSampledRequestsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFRegional.Types

-- | /See:/ 'newGetSampledRequests' smart constructor.
data GetSampledRequests = GetSampledRequests'
  { -- | The @WebACLId@ of the @WebACL@ for which you want @GetSampledRequests@
    -- to return a sample of requests.
    webAclId :: Prelude.Text,
    -- | @RuleId@ is one of three values:
    --
    -- -   The @RuleId@ of the @Rule@ or the @RuleGroupId@ of the @RuleGroup@
    --     for which you want @GetSampledRequests@ to return a sample of
    --     requests.
    --
    -- -   @Default_Action@, which causes @GetSampledRequests@ to return a
    --     sample of the requests that didn\'t match any of the rules in the
    --     specified @WebACL@.
    ruleId :: Prelude.Text,
    -- | The start date and time and the end date and time of the range for which
    -- you want @GetSampledRequests@ to return a sample of requests. You must
    -- specify the times in Coordinated Universal Time (UTC) format. UTC format
    -- includes the special designator, @Z@. For example,
    -- @\"2016-09-27T14:50Z\"@. You can specify any time range in the previous
    -- three hours.
    timeWindow :: TimeWindow,
    -- | The number of requests that you want AWS WAF to return from among the
    -- first 5,000 requests that your AWS resource received during the time
    -- range. If your resource received fewer requests than the value of
    -- @MaxItems@, @GetSampledRequests@ returns information about all of them.
    maxItems :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSampledRequests' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'webAclId', 'getSampledRequests_webAclId' - The @WebACLId@ of the @WebACL@ for which you want @GetSampledRequests@
-- to return a sample of requests.
--
-- 'ruleId', 'getSampledRequests_ruleId' - @RuleId@ is one of three values:
--
-- -   The @RuleId@ of the @Rule@ or the @RuleGroupId@ of the @RuleGroup@
--     for which you want @GetSampledRequests@ to return a sample of
--     requests.
--
-- -   @Default_Action@, which causes @GetSampledRequests@ to return a
--     sample of the requests that didn\'t match any of the rules in the
--     specified @WebACL@.
--
-- 'timeWindow', 'getSampledRequests_timeWindow' - The start date and time and the end date and time of the range for which
-- you want @GetSampledRequests@ to return a sample of requests. You must
-- specify the times in Coordinated Universal Time (UTC) format. UTC format
-- includes the special designator, @Z@. For example,
-- @\"2016-09-27T14:50Z\"@. You can specify any time range in the previous
-- three hours.
--
-- 'maxItems', 'getSampledRequests_maxItems' - The number of requests that you want AWS WAF to return from among the
-- first 5,000 requests that your AWS resource received during the time
-- range. If your resource received fewer requests than the value of
-- @MaxItems@, @GetSampledRequests@ returns information about all of them.
newGetSampledRequests ::
  -- | 'webAclId'
  Prelude.Text ->
  -- | 'ruleId'
  Prelude.Text ->
  -- | 'timeWindow'
  TimeWindow ->
  -- | 'maxItems'
  Prelude.Natural ->
  GetSampledRequests
newGetSampledRequests
  pWebAclId_
  pRuleId_
  pTimeWindow_
  pMaxItems_ =
    GetSampledRequests'
      { webAclId = pWebAclId_,
        ruleId = pRuleId_,
        timeWindow = pTimeWindow_,
        maxItems = pMaxItems_
      }

-- | The @WebACLId@ of the @WebACL@ for which you want @GetSampledRequests@
-- to return a sample of requests.
getSampledRequests_webAclId :: Lens.Lens' GetSampledRequests Prelude.Text
getSampledRequests_webAclId = Lens.lens (\GetSampledRequests' {webAclId} -> webAclId) (\s@GetSampledRequests' {} a -> s {webAclId = a} :: GetSampledRequests)

-- | @RuleId@ is one of three values:
--
-- -   The @RuleId@ of the @Rule@ or the @RuleGroupId@ of the @RuleGroup@
--     for which you want @GetSampledRequests@ to return a sample of
--     requests.
--
-- -   @Default_Action@, which causes @GetSampledRequests@ to return a
--     sample of the requests that didn\'t match any of the rules in the
--     specified @WebACL@.
getSampledRequests_ruleId :: Lens.Lens' GetSampledRequests Prelude.Text
getSampledRequests_ruleId = Lens.lens (\GetSampledRequests' {ruleId} -> ruleId) (\s@GetSampledRequests' {} a -> s {ruleId = a} :: GetSampledRequests)

-- | The start date and time and the end date and time of the range for which
-- you want @GetSampledRequests@ to return a sample of requests. You must
-- specify the times in Coordinated Universal Time (UTC) format. UTC format
-- includes the special designator, @Z@. For example,
-- @\"2016-09-27T14:50Z\"@. You can specify any time range in the previous
-- three hours.
getSampledRequests_timeWindow :: Lens.Lens' GetSampledRequests TimeWindow
getSampledRequests_timeWindow = Lens.lens (\GetSampledRequests' {timeWindow} -> timeWindow) (\s@GetSampledRequests' {} a -> s {timeWindow = a} :: GetSampledRequests)

-- | The number of requests that you want AWS WAF to return from among the
-- first 5,000 requests that your AWS resource received during the time
-- range. If your resource received fewer requests than the value of
-- @MaxItems@, @GetSampledRequests@ returns information about all of them.
getSampledRequests_maxItems :: Lens.Lens' GetSampledRequests Prelude.Natural
getSampledRequests_maxItems = Lens.lens (\GetSampledRequests' {maxItems} -> maxItems) (\s@GetSampledRequests' {} a -> s {maxItems = a} :: GetSampledRequests)

instance Core.AWSRequest GetSampledRequests where
  type
    AWSResponse GetSampledRequests =
      GetSampledRequestsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSampledRequestsResponse'
            Prelude.<$> (x Data..?> "PopulationSize")
            Prelude.<*> ( x Data..?> "SampledRequests"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "TimeWindow")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSampledRequests where
  hashWithSalt _salt GetSampledRequests' {..} =
    _salt `Prelude.hashWithSalt` webAclId
      `Prelude.hashWithSalt` ruleId
      `Prelude.hashWithSalt` timeWindow
      `Prelude.hashWithSalt` maxItems

instance Prelude.NFData GetSampledRequests where
  rnf GetSampledRequests' {..} =
    Prelude.rnf webAclId
      `Prelude.seq` Prelude.rnf ruleId
      `Prelude.seq` Prelude.rnf timeWindow
      `Prelude.seq` Prelude.rnf maxItems

instance Data.ToHeaders GetSampledRequests where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_Regional_20161128.GetSampledRequests" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetSampledRequests where
  toJSON GetSampledRequests' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("WebAclId" Data..= webAclId),
            Prelude.Just ("RuleId" Data..= ruleId),
            Prelude.Just ("TimeWindow" Data..= timeWindow),
            Prelude.Just ("MaxItems" Data..= maxItems)
          ]
      )

instance Data.ToPath GetSampledRequests where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSampledRequests where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSampledRequestsResponse' smart constructor.
data GetSampledRequestsResponse = GetSampledRequestsResponse'
  { -- | The total number of requests from which @GetSampledRequests@ got a
    -- sample of @MaxItems@ requests. If @PopulationSize@ is less than
    -- @MaxItems@, the sample includes every request that your AWS resource
    -- received during the specified time range.
    populationSize :: Prelude.Maybe Prelude.Integer,
    -- | A complex type that contains detailed information about each of the
    -- requests in the sample.
    sampledRequests :: Prelude.Maybe [SampledHTTPRequest],
    -- | Usually, @TimeWindow@ is the time range that you specified in the
    -- @GetSampledRequests@ request. However, if your AWS resource received
    -- more than 5,000 requests during the time range that you specified in the
    -- request, @GetSampledRequests@ returns the time range for the first 5,000
    -- requests. Times are in Coordinated Universal Time (UTC) format.
    timeWindow :: Prelude.Maybe TimeWindow,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSampledRequestsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'populationSize', 'getSampledRequestsResponse_populationSize' - The total number of requests from which @GetSampledRequests@ got a
-- sample of @MaxItems@ requests. If @PopulationSize@ is less than
-- @MaxItems@, the sample includes every request that your AWS resource
-- received during the specified time range.
--
-- 'sampledRequests', 'getSampledRequestsResponse_sampledRequests' - A complex type that contains detailed information about each of the
-- requests in the sample.
--
-- 'timeWindow', 'getSampledRequestsResponse_timeWindow' - Usually, @TimeWindow@ is the time range that you specified in the
-- @GetSampledRequests@ request. However, if your AWS resource received
-- more than 5,000 requests during the time range that you specified in the
-- request, @GetSampledRequests@ returns the time range for the first 5,000
-- requests. Times are in Coordinated Universal Time (UTC) format.
--
-- 'httpStatus', 'getSampledRequestsResponse_httpStatus' - The response's http status code.
newGetSampledRequestsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSampledRequestsResponse
newGetSampledRequestsResponse pHttpStatus_ =
  GetSampledRequestsResponse'
    { populationSize =
        Prelude.Nothing,
      sampledRequests = Prelude.Nothing,
      timeWindow = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The total number of requests from which @GetSampledRequests@ got a
-- sample of @MaxItems@ requests. If @PopulationSize@ is less than
-- @MaxItems@, the sample includes every request that your AWS resource
-- received during the specified time range.
getSampledRequestsResponse_populationSize :: Lens.Lens' GetSampledRequestsResponse (Prelude.Maybe Prelude.Integer)
getSampledRequestsResponse_populationSize = Lens.lens (\GetSampledRequestsResponse' {populationSize} -> populationSize) (\s@GetSampledRequestsResponse' {} a -> s {populationSize = a} :: GetSampledRequestsResponse)

-- | A complex type that contains detailed information about each of the
-- requests in the sample.
getSampledRequestsResponse_sampledRequests :: Lens.Lens' GetSampledRequestsResponse (Prelude.Maybe [SampledHTTPRequest])
getSampledRequestsResponse_sampledRequests = Lens.lens (\GetSampledRequestsResponse' {sampledRequests} -> sampledRequests) (\s@GetSampledRequestsResponse' {} a -> s {sampledRequests = a} :: GetSampledRequestsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Usually, @TimeWindow@ is the time range that you specified in the
-- @GetSampledRequests@ request. However, if your AWS resource received
-- more than 5,000 requests during the time range that you specified in the
-- request, @GetSampledRequests@ returns the time range for the first 5,000
-- requests. Times are in Coordinated Universal Time (UTC) format.
getSampledRequestsResponse_timeWindow :: Lens.Lens' GetSampledRequestsResponse (Prelude.Maybe TimeWindow)
getSampledRequestsResponse_timeWindow = Lens.lens (\GetSampledRequestsResponse' {timeWindow} -> timeWindow) (\s@GetSampledRequestsResponse' {} a -> s {timeWindow = a} :: GetSampledRequestsResponse)

-- | The response's http status code.
getSampledRequestsResponse_httpStatus :: Lens.Lens' GetSampledRequestsResponse Prelude.Int
getSampledRequestsResponse_httpStatus = Lens.lens (\GetSampledRequestsResponse' {httpStatus} -> httpStatus) (\s@GetSampledRequestsResponse' {} a -> s {httpStatus = a} :: GetSampledRequestsResponse)

instance Prelude.NFData GetSampledRequestsResponse where
  rnf GetSampledRequestsResponse' {..} =
    Prelude.rnf populationSize
      `Prelude.seq` Prelude.rnf sampledRequests
      `Prelude.seq` Prelude.rnf timeWindow
      `Prelude.seq` Prelude.rnf httpStatus
