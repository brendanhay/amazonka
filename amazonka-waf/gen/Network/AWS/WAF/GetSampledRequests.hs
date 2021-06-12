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
-- Module      : Network.AWS.WAF.GetSampledRequests
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.WAF.GetSampledRequests
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
    getSampledRequestsResponse_timeWindow,
    getSampledRequestsResponse_populationSize,
    getSampledRequestsResponse_sampledRequests,
    getSampledRequestsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newGetSampledRequests' smart constructor.
data GetSampledRequests = GetSampledRequests'
  { -- | The @WebACLId@ of the @WebACL@ for which you want @GetSampledRequests@
    -- to return a sample of requests.
    webAclId :: Core.Text,
    -- | @RuleId@ is one of three values:
    --
    -- -   The @RuleId@ of the @Rule@ or the @RuleGroupId@ of the @RuleGroup@
    --     for which you want @GetSampledRequests@ to return a sample of
    --     requests.
    --
    -- -   @Default_Action@, which causes @GetSampledRequests@ to return a
    --     sample of the requests that didn\'t match any of the rules in the
    --     specified @WebACL@.
    ruleId :: Core.Text,
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
    maxItems :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'ruleId'
  Core.Text ->
  -- | 'timeWindow'
  TimeWindow ->
  -- | 'maxItems'
  Core.Natural ->
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
getSampledRequests_webAclId :: Lens.Lens' GetSampledRequests Core.Text
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
getSampledRequests_ruleId :: Lens.Lens' GetSampledRequests Core.Text
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
getSampledRequests_maxItems :: Lens.Lens' GetSampledRequests Core.Natural
getSampledRequests_maxItems = Lens.lens (\GetSampledRequests' {maxItems} -> maxItems) (\s@GetSampledRequests' {} a -> s {maxItems = a} :: GetSampledRequests)

instance Core.AWSRequest GetSampledRequests where
  type
    AWSResponse GetSampledRequests =
      GetSampledRequestsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSampledRequestsResponse'
            Core.<$> (x Core..?> "TimeWindow")
            Core.<*> (x Core..?> "PopulationSize")
            Core.<*> (x Core..?> "SampledRequests" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetSampledRequests

instance Core.NFData GetSampledRequests

instance Core.ToHeaders GetSampledRequests where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20150824.GetSampledRequests" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetSampledRequests where
  toJSON GetSampledRequests' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("WebAclId" Core..= webAclId),
            Core.Just ("RuleId" Core..= ruleId),
            Core.Just ("TimeWindow" Core..= timeWindow),
            Core.Just ("MaxItems" Core..= maxItems)
          ]
      )

instance Core.ToPath GetSampledRequests where
  toPath = Core.const "/"

instance Core.ToQuery GetSampledRequests where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetSampledRequestsResponse' smart constructor.
data GetSampledRequestsResponse = GetSampledRequestsResponse'
  { -- | Usually, @TimeWindow@ is the time range that you specified in the
    -- @GetSampledRequests@ request. However, if your AWS resource received
    -- more than 5,000 requests during the time range that you specified in the
    -- request, @GetSampledRequests@ returns the time range for the first 5,000
    -- requests. Times are in Coordinated Universal Time (UTC) format.
    timeWindow :: Core.Maybe TimeWindow,
    -- | The total number of requests from which @GetSampledRequests@ got a
    -- sample of @MaxItems@ requests. If @PopulationSize@ is less than
    -- @MaxItems@, the sample includes every request that your AWS resource
    -- received during the specified time range.
    populationSize :: Core.Maybe Core.Integer,
    -- | A complex type that contains detailed information about each of the
    -- requests in the sample.
    sampledRequests :: Core.Maybe [SampledHTTPRequest],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSampledRequestsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeWindow', 'getSampledRequestsResponse_timeWindow' - Usually, @TimeWindow@ is the time range that you specified in the
-- @GetSampledRequests@ request. However, if your AWS resource received
-- more than 5,000 requests during the time range that you specified in the
-- request, @GetSampledRequests@ returns the time range for the first 5,000
-- requests. Times are in Coordinated Universal Time (UTC) format.
--
-- 'populationSize', 'getSampledRequestsResponse_populationSize' - The total number of requests from which @GetSampledRequests@ got a
-- sample of @MaxItems@ requests. If @PopulationSize@ is less than
-- @MaxItems@, the sample includes every request that your AWS resource
-- received during the specified time range.
--
-- 'sampledRequests', 'getSampledRequestsResponse_sampledRequests' - A complex type that contains detailed information about each of the
-- requests in the sample.
--
-- 'httpStatus', 'getSampledRequestsResponse_httpStatus' - The response's http status code.
newGetSampledRequestsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetSampledRequestsResponse
newGetSampledRequestsResponse pHttpStatus_ =
  GetSampledRequestsResponse'
    { timeWindow =
        Core.Nothing,
      populationSize = Core.Nothing,
      sampledRequests = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Usually, @TimeWindow@ is the time range that you specified in the
-- @GetSampledRequests@ request. However, if your AWS resource received
-- more than 5,000 requests during the time range that you specified in the
-- request, @GetSampledRequests@ returns the time range for the first 5,000
-- requests. Times are in Coordinated Universal Time (UTC) format.
getSampledRequestsResponse_timeWindow :: Lens.Lens' GetSampledRequestsResponse (Core.Maybe TimeWindow)
getSampledRequestsResponse_timeWindow = Lens.lens (\GetSampledRequestsResponse' {timeWindow} -> timeWindow) (\s@GetSampledRequestsResponse' {} a -> s {timeWindow = a} :: GetSampledRequestsResponse)

-- | The total number of requests from which @GetSampledRequests@ got a
-- sample of @MaxItems@ requests. If @PopulationSize@ is less than
-- @MaxItems@, the sample includes every request that your AWS resource
-- received during the specified time range.
getSampledRequestsResponse_populationSize :: Lens.Lens' GetSampledRequestsResponse (Core.Maybe Core.Integer)
getSampledRequestsResponse_populationSize = Lens.lens (\GetSampledRequestsResponse' {populationSize} -> populationSize) (\s@GetSampledRequestsResponse' {} a -> s {populationSize = a} :: GetSampledRequestsResponse)

-- | A complex type that contains detailed information about each of the
-- requests in the sample.
getSampledRequestsResponse_sampledRequests :: Lens.Lens' GetSampledRequestsResponse (Core.Maybe [SampledHTTPRequest])
getSampledRequestsResponse_sampledRequests = Lens.lens (\GetSampledRequestsResponse' {sampledRequests} -> sampledRequests) (\s@GetSampledRequestsResponse' {} a -> s {sampledRequests = a} :: GetSampledRequestsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getSampledRequestsResponse_httpStatus :: Lens.Lens' GetSampledRequestsResponse Core.Int
getSampledRequestsResponse_httpStatus = Lens.lens (\GetSampledRequestsResponse' {httpStatus} -> httpStatus) (\s@GetSampledRequestsResponse' {} a -> s {httpStatus = a} :: GetSampledRequestsResponse)

instance Core.NFData GetSampledRequestsResponse
