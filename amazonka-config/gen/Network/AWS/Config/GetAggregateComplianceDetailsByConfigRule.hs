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
-- Module      : Network.AWS.Config.GetAggregateComplianceDetailsByConfigRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the evaluation results for the specified AWS Config rule for a
-- specific resource in a rule. The results indicate which AWS resources
-- were evaluated by the rule, when each resource was last evaluated, and
-- whether each resource complies with the rule.
--
-- The results can return an empty result page. But if you have a
-- @nextToken@, the results are displayed on the next page.
--
-- This operation returns paginated results.
module Network.AWS.Config.GetAggregateComplianceDetailsByConfigRule
  ( -- * Creating a Request
    GetAggregateComplianceDetailsByConfigRule (..),
    newGetAggregateComplianceDetailsByConfigRule,

    -- * Request Lenses
    getAggregateComplianceDetailsByConfigRule_nextToken,
    getAggregateComplianceDetailsByConfigRule_complianceType,
    getAggregateComplianceDetailsByConfigRule_limit,
    getAggregateComplianceDetailsByConfigRule_configurationAggregatorName,
    getAggregateComplianceDetailsByConfigRule_configRuleName,
    getAggregateComplianceDetailsByConfigRule_accountId,
    getAggregateComplianceDetailsByConfigRule_awsRegion,

    -- * Destructuring the Response
    GetAggregateComplianceDetailsByConfigRuleResponse (..),
    newGetAggregateComplianceDetailsByConfigRuleResponse,

    -- * Response Lenses
    getAggregateComplianceDetailsByConfigRuleResponse_nextToken,
    getAggregateComplianceDetailsByConfigRuleResponse_aggregateEvaluationResults,
    getAggregateComplianceDetailsByConfigRuleResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAggregateComplianceDetailsByConfigRule' smart constructor.
data GetAggregateComplianceDetailsByConfigRule = GetAggregateComplianceDetailsByConfigRule'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | The resource compliance status.
    --
    -- For the @GetAggregateComplianceDetailsByConfigRuleRequest@ data type,
    -- AWS Config supports only the @COMPLIANT@ and @NON_COMPLIANT@. AWS Config
    -- does not support the @NOT_APPLICABLE@ and @INSUFFICIENT_DATA@ values.
    complianceType :: Core.Maybe ComplianceType,
    -- | The maximum number of evaluation results returned on each page. The
    -- default is 50. You cannot specify a number greater than 100. If you
    -- specify 0, AWS Config uses the default.
    limit :: Core.Maybe Core.Natural,
    -- | The name of the configuration aggregator.
    configurationAggregatorName :: Core.Text,
    -- | The name of the AWS Config rule for which you want compliance
    -- information.
    configRuleName :: Core.Text,
    -- | The 12-digit account ID of the source account.
    accountId :: Core.Text,
    -- | The source region from where the data is aggregated.
    awsRegion :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAggregateComplianceDetailsByConfigRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getAggregateComplianceDetailsByConfigRule_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'complianceType', 'getAggregateComplianceDetailsByConfigRule_complianceType' - The resource compliance status.
--
-- For the @GetAggregateComplianceDetailsByConfigRuleRequest@ data type,
-- AWS Config supports only the @COMPLIANT@ and @NON_COMPLIANT@. AWS Config
-- does not support the @NOT_APPLICABLE@ and @INSUFFICIENT_DATA@ values.
--
-- 'limit', 'getAggregateComplianceDetailsByConfigRule_limit' - The maximum number of evaluation results returned on each page. The
-- default is 50. You cannot specify a number greater than 100. If you
-- specify 0, AWS Config uses the default.
--
-- 'configurationAggregatorName', 'getAggregateComplianceDetailsByConfigRule_configurationAggregatorName' - The name of the configuration aggregator.
--
-- 'configRuleName', 'getAggregateComplianceDetailsByConfigRule_configRuleName' - The name of the AWS Config rule for which you want compliance
-- information.
--
-- 'accountId', 'getAggregateComplianceDetailsByConfigRule_accountId' - The 12-digit account ID of the source account.
--
-- 'awsRegion', 'getAggregateComplianceDetailsByConfigRule_awsRegion' - The source region from where the data is aggregated.
newGetAggregateComplianceDetailsByConfigRule ::
  -- | 'configurationAggregatorName'
  Core.Text ->
  -- | 'configRuleName'
  Core.Text ->
  -- | 'accountId'
  Core.Text ->
  -- | 'awsRegion'
  Core.Text ->
  GetAggregateComplianceDetailsByConfigRule
newGetAggregateComplianceDetailsByConfigRule
  pConfigurationAggregatorName_
  pConfigRuleName_
  pAccountId_
  pAwsRegion_ =
    GetAggregateComplianceDetailsByConfigRule'
      { nextToken =
          Core.Nothing,
        complianceType = Core.Nothing,
        limit = Core.Nothing,
        configurationAggregatorName =
          pConfigurationAggregatorName_,
        configRuleName =
          pConfigRuleName_,
        accountId = pAccountId_,
        awsRegion = pAwsRegion_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
getAggregateComplianceDetailsByConfigRule_nextToken :: Lens.Lens' GetAggregateComplianceDetailsByConfigRule (Core.Maybe Core.Text)
getAggregateComplianceDetailsByConfigRule_nextToken = Lens.lens (\GetAggregateComplianceDetailsByConfigRule' {nextToken} -> nextToken) (\s@GetAggregateComplianceDetailsByConfigRule' {} a -> s {nextToken = a} :: GetAggregateComplianceDetailsByConfigRule)

-- | The resource compliance status.
--
-- For the @GetAggregateComplianceDetailsByConfigRuleRequest@ data type,
-- AWS Config supports only the @COMPLIANT@ and @NON_COMPLIANT@. AWS Config
-- does not support the @NOT_APPLICABLE@ and @INSUFFICIENT_DATA@ values.
getAggregateComplianceDetailsByConfigRule_complianceType :: Lens.Lens' GetAggregateComplianceDetailsByConfigRule (Core.Maybe ComplianceType)
getAggregateComplianceDetailsByConfigRule_complianceType = Lens.lens (\GetAggregateComplianceDetailsByConfigRule' {complianceType} -> complianceType) (\s@GetAggregateComplianceDetailsByConfigRule' {} a -> s {complianceType = a} :: GetAggregateComplianceDetailsByConfigRule)

-- | The maximum number of evaluation results returned on each page. The
-- default is 50. You cannot specify a number greater than 100. If you
-- specify 0, AWS Config uses the default.
getAggregateComplianceDetailsByConfigRule_limit :: Lens.Lens' GetAggregateComplianceDetailsByConfigRule (Core.Maybe Core.Natural)
getAggregateComplianceDetailsByConfigRule_limit = Lens.lens (\GetAggregateComplianceDetailsByConfigRule' {limit} -> limit) (\s@GetAggregateComplianceDetailsByConfigRule' {} a -> s {limit = a} :: GetAggregateComplianceDetailsByConfigRule)

-- | The name of the configuration aggregator.
getAggregateComplianceDetailsByConfigRule_configurationAggregatorName :: Lens.Lens' GetAggregateComplianceDetailsByConfigRule Core.Text
getAggregateComplianceDetailsByConfigRule_configurationAggregatorName = Lens.lens (\GetAggregateComplianceDetailsByConfigRule' {configurationAggregatorName} -> configurationAggregatorName) (\s@GetAggregateComplianceDetailsByConfigRule' {} a -> s {configurationAggregatorName = a} :: GetAggregateComplianceDetailsByConfigRule)

-- | The name of the AWS Config rule for which you want compliance
-- information.
getAggregateComplianceDetailsByConfigRule_configRuleName :: Lens.Lens' GetAggregateComplianceDetailsByConfigRule Core.Text
getAggregateComplianceDetailsByConfigRule_configRuleName = Lens.lens (\GetAggregateComplianceDetailsByConfigRule' {configRuleName} -> configRuleName) (\s@GetAggregateComplianceDetailsByConfigRule' {} a -> s {configRuleName = a} :: GetAggregateComplianceDetailsByConfigRule)

-- | The 12-digit account ID of the source account.
getAggregateComplianceDetailsByConfigRule_accountId :: Lens.Lens' GetAggregateComplianceDetailsByConfigRule Core.Text
getAggregateComplianceDetailsByConfigRule_accountId = Lens.lens (\GetAggregateComplianceDetailsByConfigRule' {accountId} -> accountId) (\s@GetAggregateComplianceDetailsByConfigRule' {} a -> s {accountId = a} :: GetAggregateComplianceDetailsByConfigRule)

-- | The source region from where the data is aggregated.
getAggregateComplianceDetailsByConfigRule_awsRegion :: Lens.Lens' GetAggregateComplianceDetailsByConfigRule Core.Text
getAggregateComplianceDetailsByConfigRule_awsRegion = Lens.lens (\GetAggregateComplianceDetailsByConfigRule' {awsRegion} -> awsRegion) (\s@GetAggregateComplianceDetailsByConfigRule' {} a -> s {awsRegion = a} :: GetAggregateComplianceDetailsByConfigRule)

instance
  Core.AWSPager
    GetAggregateComplianceDetailsByConfigRule
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getAggregateComplianceDetailsByConfigRuleResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getAggregateComplianceDetailsByConfigRuleResponse_aggregateEvaluationResults
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getAggregateComplianceDetailsByConfigRule_nextToken
          Lens..~ rs
            Lens.^? getAggregateComplianceDetailsByConfigRuleResponse_nextToken
              Core.. Lens._Just

instance
  Core.AWSRequest
    GetAggregateComplianceDetailsByConfigRule
  where
  type
    AWSResponse
      GetAggregateComplianceDetailsByConfigRule =
      GetAggregateComplianceDetailsByConfigRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAggregateComplianceDetailsByConfigRuleResponse'
            Core.<$> (x Core..?> "NextToken")
              Core.<*> ( x Core..?> "AggregateEvaluationResults"
                           Core..!@ Core.mempty
                       )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetAggregateComplianceDetailsByConfigRule

instance
  Core.NFData
    GetAggregateComplianceDetailsByConfigRule

instance
  Core.ToHeaders
    GetAggregateComplianceDetailsByConfigRule
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.GetAggregateComplianceDetailsByConfigRule" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    GetAggregateComplianceDetailsByConfigRule
  where
  toJSON GetAggregateComplianceDetailsByConfigRule' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("ComplianceType" Core..=) Core.<$> complianceType,
            ("Limit" Core..=) Core.<$> limit,
            Core.Just
              ( "ConfigurationAggregatorName"
                  Core..= configurationAggregatorName
              ),
            Core.Just ("ConfigRuleName" Core..= configRuleName),
            Core.Just ("AccountId" Core..= accountId),
            Core.Just ("AwsRegion" Core..= awsRegion)
          ]
      )

instance
  Core.ToPath
    GetAggregateComplianceDetailsByConfigRule
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    GetAggregateComplianceDetailsByConfigRule
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetAggregateComplianceDetailsByConfigRuleResponse' smart constructor.
data GetAggregateComplianceDetailsByConfigRuleResponse = GetAggregateComplianceDetailsByConfigRuleResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | Returns an AggregateEvaluationResults object.
    aggregateEvaluationResults :: Core.Maybe [AggregateEvaluationResult],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAggregateComplianceDetailsByConfigRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getAggregateComplianceDetailsByConfigRuleResponse_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'aggregateEvaluationResults', 'getAggregateComplianceDetailsByConfigRuleResponse_aggregateEvaluationResults' - Returns an AggregateEvaluationResults object.
--
-- 'httpStatus', 'getAggregateComplianceDetailsByConfigRuleResponse_httpStatus' - The response's http status code.
newGetAggregateComplianceDetailsByConfigRuleResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetAggregateComplianceDetailsByConfigRuleResponse
newGetAggregateComplianceDetailsByConfigRuleResponse
  pHttpStatus_ =
    GetAggregateComplianceDetailsByConfigRuleResponse'
      { nextToken =
          Core.Nothing,
        aggregateEvaluationResults =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
getAggregateComplianceDetailsByConfigRuleResponse_nextToken :: Lens.Lens' GetAggregateComplianceDetailsByConfigRuleResponse (Core.Maybe Core.Text)
getAggregateComplianceDetailsByConfigRuleResponse_nextToken = Lens.lens (\GetAggregateComplianceDetailsByConfigRuleResponse' {nextToken} -> nextToken) (\s@GetAggregateComplianceDetailsByConfigRuleResponse' {} a -> s {nextToken = a} :: GetAggregateComplianceDetailsByConfigRuleResponse)

-- | Returns an AggregateEvaluationResults object.
getAggregateComplianceDetailsByConfigRuleResponse_aggregateEvaluationResults :: Lens.Lens' GetAggregateComplianceDetailsByConfigRuleResponse (Core.Maybe [AggregateEvaluationResult])
getAggregateComplianceDetailsByConfigRuleResponse_aggregateEvaluationResults = Lens.lens (\GetAggregateComplianceDetailsByConfigRuleResponse' {aggregateEvaluationResults} -> aggregateEvaluationResults) (\s@GetAggregateComplianceDetailsByConfigRuleResponse' {} a -> s {aggregateEvaluationResults = a} :: GetAggregateComplianceDetailsByConfigRuleResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getAggregateComplianceDetailsByConfigRuleResponse_httpStatus :: Lens.Lens' GetAggregateComplianceDetailsByConfigRuleResponse Core.Int
getAggregateComplianceDetailsByConfigRuleResponse_httpStatus = Lens.lens (\GetAggregateComplianceDetailsByConfigRuleResponse' {httpStatus} -> httpStatus) (\s@GetAggregateComplianceDetailsByConfigRuleResponse' {} a -> s {httpStatus = a} :: GetAggregateComplianceDetailsByConfigRuleResponse)

instance
  Core.NFData
    GetAggregateComplianceDetailsByConfigRuleResponse
