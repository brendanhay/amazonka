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
-- Module      : Network.AWS.Config.DescribeConfigRuleEvaluationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns status information for each of your AWS managed Config rules.
-- The status includes information such as the last time AWS Config invoked
-- the rule, the last time AWS Config failed to invoke the rule, and the
-- related error for the last failure.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeConfigRuleEvaluationStatus
  ( -- * Creating a Request
    DescribeConfigRuleEvaluationStatus (..),
    newDescribeConfigRuleEvaluationStatus,

    -- * Request Lenses
    describeConfigRuleEvaluationStatus_nextToken,
    describeConfigRuleEvaluationStatus_configRuleNames,
    describeConfigRuleEvaluationStatus_limit,

    -- * Destructuring the Response
    DescribeConfigRuleEvaluationStatusResponse (..),
    newDescribeConfigRuleEvaluationStatusResponse,

    -- * Response Lenses
    describeConfigRuleEvaluationStatusResponse_nextToken,
    describeConfigRuleEvaluationStatusResponse_configRulesEvaluationStatus,
    describeConfigRuleEvaluationStatusResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeConfigRuleEvaluationStatus' smart constructor.
data DescribeConfigRuleEvaluationStatus = DescribeConfigRuleEvaluationStatus'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | The name of the AWS managed Config rules for which you want status
    -- information. If you do not specify any names, AWS Config returns status
    -- information for all AWS managed Config rules that you use.
    configRuleNames :: Core.Maybe [Core.Text],
    -- | The number of rule evaluation results that you want returned.
    --
    -- This parameter is required if the rule limit for your account is more
    -- than the default of 150 rules.
    --
    -- For information about requesting a rule limit increase, see
    -- <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_config AWS Config Limits>
    -- in the /AWS General Reference Guide/.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeConfigRuleEvaluationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeConfigRuleEvaluationStatus_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'configRuleNames', 'describeConfigRuleEvaluationStatus_configRuleNames' - The name of the AWS managed Config rules for which you want status
-- information. If you do not specify any names, AWS Config returns status
-- information for all AWS managed Config rules that you use.
--
-- 'limit', 'describeConfigRuleEvaluationStatus_limit' - The number of rule evaluation results that you want returned.
--
-- This parameter is required if the rule limit for your account is more
-- than the default of 150 rules.
--
-- For information about requesting a rule limit increase, see
-- <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_config AWS Config Limits>
-- in the /AWS General Reference Guide/.
newDescribeConfigRuleEvaluationStatus ::
  DescribeConfigRuleEvaluationStatus
newDescribeConfigRuleEvaluationStatus =
  DescribeConfigRuleEvaluationStatus'
    { nextToken =
        Core.Nothing,
      configRuleNames = Core.Nothing,
      limit = Core.Nothing
    }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeConfigRuleEvaluationStatus_nextToken :: Lens.Lens' DescribeConfigRuleEvaluationStatus (Core.Maybe Core.Text)
describeConfigRuleEvaluationStatus_nextToken = Lens.lens (\DescribeConfigRuleEvaluationStatus' {nextToken} -> nextToken) (\s@DescribeConfigRuleEvaluationStatus' {} a -> s {nextToken = a} :: DescribeConfigRuleEvaluationStatus)

-- | The name of the AWS managed Config rules for which you want status
-- information. If you do not specify any names, AWS Config returns status
-- information for all AWS managed Config rules that you use.
describeConfigRuleEvaluationStatus_configRuleNames :: Lens.Lens' DescribeConfigRuleEvaluationStatus (Core.Maybe [Core.Text])
describeConfigRuleEvaluationStatus_configRuleNames = Lens.lens (\DescribeConfigRuleEvaluationStatus' {configRuleNames} -> configRuleNames) (\s@DescribeConfigRuleEvaluationStatus' {} a -> s {configRuleNames = a} :: DescribeConfigRuleEvaluationStatus) Core.. Lens.mapping Lens._Coerce

-- | The number of rule evaluation results that you want returned.
--
-- This parameter is required if the rule limit for your account is more
-- than the default of 150 rules.
--
-- For information about requesting a rule limit increase, see
-- <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_config AWS Config Limits>
-- in the /AWS General Reference Guide/.
describeConfigRuleEvaluationStatus_limit :: Lens.Lens' DescribeConfigRuleEvaluationStatus (Core.Maybe Core.Natural)
describeConfigRuleEvaluationStatus_limit = Lens.lens (\DescribeConfigRuleEvaluationStatus' {limit} -> limit) (\s@DescribeConfigRuleEvaluationStatus' {} a -> s {limit = a} :: DescribeConfigRuleEvaluationStatus)

instance
  Core.AWSPager
    DescribeConfigRuleEvaluationStatus
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeConfigRuleEvaluationStatusResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeConfigRuleEvaluationStatusResponse_configRulesEvaluationStatus
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeConfigRuleEvaluationStatus_nextToken
          Lens..~ rs
          Lens.^? describeConfigRuleEvaluationStatusResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeConfigRuleEvaluationStatus
  where
  type
    AWSResponse DescribeConfigRuleEvaluationStatus =
      DescribeConfigRuleEvaluationStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConfigRuleEvaluationStatusResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "ConfigRulesEvaluationStatus"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeConfigRuleEvaluationStatus

instance
  Core.NFData
    DescribeConfigRuleEvaluationStatus

instance
  Core.ToHeaders
    DescribeConfigRuleEvaluationStatus
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DescribeConfigRuleEvaluationStatus" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeConfigRuleEvaluationStatus
  where
  toJSON DescribeConfigRuleEvaluationStatus' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("ConfigRuleNames" Core..=) Core.<$> configRuleNames,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance
  Core.ToPath
    DescribeConfigRuleEvaluationStatus
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeConfigRuleEvaluationStatus
  where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newDescribeConfigRuleEvaluationStatusResponse' smart constructor.
data DescribeConfigRuleEvaluationStatusResponse = DescribeConfigRuleEvaluationStatusResponse'
  { -- | The string that you use in a subsequent request to get the next page of
    -- results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | Status information about your AWS managed Config rules.
    configRulesEvaluationStatus :: Core.Maybe [ConfigRuleEvaluationStatus],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeConfigRuleEvaluationStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeConfigRuleEvaluationStatusResponse_nextToken' - The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
--
-- 'configRulesEvaluationStatus', 'describeConfigRuleEvaluationStatusResponse_configRulesEvaluationStatus' - Status information about your AWS managed Config rules.
--
-- 'httpStatus', 'describeConfigRuleEvaluationStatusResponse_httpStatus' - The response's http status code.
newDescribeConfigRuleEvaluationStatusResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeConfigRuleEvaluationStatusResponse
newDescribeConfigRuleEvaluationStatusResponse
  pHttpStatus_ =
    DescribeConfigRuleEvaluationStatusResponse'
      { nextToken =
          Core.Nothing,
        configRulesEvaluationStatus =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
describeConfigRuleEvaluationStatusResponse_nextToken :: Lens.Lens' DescribeConfigRuleEvaluationStatusResponse (Core.Maybe Core.Text)
describeConfigRuleEvaluationStatusResponse_nextToken = Lens.lens (\DescribeConfigRuleEvaluationStatusResponse' {nextToken} -> nextToken) (\s@DescribeConfigRuleEvaluationStatusResponse' {} a -> s {nextToken = a} :: DescribeConfigRuleEvaluationStatusResponse)

-- | Status information about your AWS managed Config rules.
describeConfigRuleEvaluationStatusResponse_configRulesEvaluationStatus :: Lens.Lens' DescribeConfigRuleEvaluationStatusResponse (Core.Maybe [ConfigRuleEvaluationStatus])
describeConfigRuleEvaluationStatusResponse_configRulesEvaluationStatus = Lens.lens (\DescribeConfigRuleEvaluationStatusResponse' {configRulesEvaluationStatus} -> configRulesEvaluationStatus) (\s@DescribeConfigRuleEvaluationStatusResponse' {} a -> s {configRulesEvaluationStatus = a} :: DescribeConfigRuleEvaluationStatusResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeConfigRuleEvaluationStatusResponse_httpStatus :: Lens.Lens' DescribeConfigRuleEvaluationStatusResponse Core.Int
describeConfigRuleEvaluationStatusResponse_httpStatus = Lens.lens (\DescribeConfigRuleEvaluationStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeConfigRuleEvaluationStatusResponse' {} a -> s {httpStatus = a} :: DescribeConfigRuleEvaluationStatusResponse)

instance
  Core.NFData
    DescribeConfigRuleEvaluationStatusResponse
