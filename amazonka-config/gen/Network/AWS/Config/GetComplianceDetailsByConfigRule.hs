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
-- Module      : Network.AWS.Config.GetComplianceDetailsByConfigRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the evaluation results for the specified AWS Config rule. The
-- results indicate which AWS resources were evaluated by the rule, when
-- each resource was last evaluated, and whether each resource complies
-- with the rule.
--
-- This operation returns paginated results.
module Network.AWS.Config.GetComplianceDetailsByConfigRule
  ( -- * Creating a Request
    GetComplianceDetailsByConfigRule (..),
    newGetComplianceDetailsByConfigRule,

    -- * Request Lenses
    getComplianceDetailsByConfigRule_nextToken,
    getComplianceDetailsByConfigRule_complianceTypes,
    getComplianceDetailsByConfigRule_limit,
    getComplianceDetailsByConfigRule_configRuleName,

    -- * Destructuring the Response
    GetComplianceDetailsByConfigRuleResponse (..),
    newGetComplianceDetailsByConfigRuleResponse,

    -- * Response Lenses
    getComplianceDetailsByConfigRuleResponse_nextToken,
    getComplianceDetailsByConfigRuleResponse_evaluationResults,
    getComplianceDetailsByConfigRuleResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newGetComplianceDetailsByConfigRule' smart constructor.
data GetComplianceDetailsByConfigRule = GetComplianceDetailsByConfigRule'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters the results by compliance.
    --
    -- The allowed values are @COMPLIANT@, @NON_COMPLIANT@, and
    -- @NOT_APPLICABLE@.
    complianceTypes :: Prelude.Maybe [ComplianceType],
    -- | The maximum number of evaluation results returned on each page. The
    -- default is 10. You cannot specify a number greater than 100. If you
    -- specify 0, AWS Config uses the default.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The name of the AWS Config rule for which you want compliance
    -- information.
    configRuleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetComplianceDetailsByConfigRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getComplianceDetailsByConfigRule_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'complianceTypes', 'getComplianceDetailsByConfigRule_complianceTypes' - Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@, @NON_COMPLIANT@, and
-- @NOT_APPLICABLE@.
--
-- 'limit', 'getComplianceDetailsByConfigRule_limit' - The maximum number of evaluation results returned on each page. The
-- default is 10. You cannot specify a number greater than 100. If you
-- specify 0, AWS Config uses the default.
--
-- 'configRuleName', 'getComplianceDetailsByConfigRule_configRuleName' - The name of the AWS Config rule for which you want compliance
-- information.
newGetComplianceDetailsByConfigRule ::
  -- | 'configRuleName'
  Prelude.Text ->
  GetComplianceDetailsByConfigRule
newGetComplianceDetailsByConfigRule pConfigRuleName_ =
  GetComplianceDetailsByConfigRule'
    { nextToken =
        Prelude.Nothing,
      complianceTypes = Prelude.Nothing,
      limit = Prelude.Nothing,
      configRuleName = pConfigRuleName_
    }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
getComplianceDetailsByConfigRule_nextToken :: Lens.Lens' GetComplianceDetailsByConfigRule (Prelude.Maybe Prelude.Text)
getComplianceDetailsByConfigRule_nextToken = Lens.lens (\GetComplianceDetailsByConfigRule' {nextToken} -> nextToken) (\s@GetComplianceDetailsByConfigRule' {} a -> s {nextToken = a} :: GetComplianceDetailsByConfigRule)

-- | Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@, @NON_COMPLIANT@, and
-- @NOT_APPLICABLE@.
getComplianceDetailsByConfigRule_complianceTypes :: Lens.Lens' GetComplianceDetailsByConfigRule (Prelude.Maybe [ComplianceType])
getComplianceDetailsByConfigRule_complianceTypes = Lens.lens (\GetComplianceDetailsByConfigRule' {complianceTypes} -> complianceTypes) (\s@GetComplianceDetailsByConfigRule' {} a -> s {complianceTypes = a} :: GetComplianceDetailsByConfigRule) Prelude.. Lens.mapping Lens._Coerce

-- | The maximum number of evaluation results returned on each page. The
-- default is 10. You cannot specify a number greater than 100. If you
-- specify 0, AWS Config uses the default.
getComplianceDetailsByConfigRule_limit :: Lens.Lens' GetComplianceDetailsByConfigRule (Prelude.Maybe Prelude.Natural)
getComplianceDetailsByConfigRule_limit = Lens.lens (\GetComplianceDetailsByConfigRule' {limit} -> limit) (\s@GetComplianceDetailsByConfigRule' {} a -> s {limit = a} :: GetComplianceDetailsByConfigRule)

-- | The name of the AWS Config rule for which you want compliance
-- information.
getComplianceDetailsByConfigRule_configRuleName :: Lens.Lens' GetComplianceDetailsByConfigRule Prelude.Text
getComplianceDetailsByConfigRule_configRuleName = Lens.lens (\GetComplianceDetailsByConfigRule' {configRuleName} -> configRuleName) (\s@GetComplianceDetailsByConfigRule' {} a -> s {configRuleName = a} :: GetComplianceDetailsByConfigRule)

instance
  Core.AWSPager
    GetComplianceDetailsByConfigRule
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getComplianceDetailsByConfigRuleResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getComplianceDetailsByConfigRuleResponse_evaluationResults
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getComplianceDetailsByConfigRule_nextToken
          Lens..~ rs
          Lens.^? getComplianceDetailsByConfigRuleResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    GetComplianceDetailsByConfigRule
  where
  type
    AWSResponse GetComplianceDetailsByConfigRule =
      GetComplianceDetailsByConfigRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetComplianceDetailsByConfigRuleResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "EvaluationResults"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetComplianceDetailsByConfigRule

instance
  Prelude.NFData
    GetComplianceDetailsByConfigRule

instance
  Core.ToHeaders
    GetComplianceDetailsByConfigRule
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.GetComplianceDetailsByConfigRule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetComplianceDetailsByConfigRule where
  toJSON GetComplianceDetailsByConfigRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("ComplianceTypes" Core..=)
              Prelude.<$> complianceTypes,
            ("Limit" Core..=) Prelude.<$> limit,
            Prelude.Just
              ("ConfigRuleName" Core..= configRuleName)
          ]
      )

instance Core.ToPath GetComplianceDetailsByConfigRule where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    GetComplianceDetailsByConfigRule
  where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newGetComplianceDetailsByConfigRuleResponse' smart constructor.
data GetComplianceDetailsByConfigRuleResponse = GetComplianceDetailsByConfigRuleResponse'
  { -- | The string that you use in a subsequent request to get the next page of
    -- results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the AWS resource complies with the specified AWS
    -- Config rule.
    evaluationResults :: Prelude.Maybe [EvaluationResult],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetComplianceDetailsByConfigRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getComplianceDetailsByConfigRuleResponse_nextToken' - The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
--
-- 'evaluationResults', 'getComplianceDetailsByConfigRuleResponse_evaluationResults' - Indicates whether the AWS resource complies with the specified AWS
-- Config rule.
--
-- 'httpStatus', 'getComplianceDetailsByConfigRuleResponse_httpStatus' - The response's http status code.
newGetComplianceDetailsByConfigRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetComplianceDetailsByConfigRuleResponse
newGetComplianceDetailsByConfigRuleResponse
  pHttpStatus_ =
    GetComplianceDetailsByConfigRuleResponse'
      { nextToken =
          Prelude.Nothing,
        evaluationResults =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
getComplianceDetailsByConfigRuleResponse_nextToken :: Lens.Lens' GetComplianceDetailsByConfigRuleResponse (Prelude.Maybe Prelude.Text)
getComplianceDetailsByConfigRuleResponse_nextToken = Lens.lens (\GetComplianceDetailsByConfigRuleResponse' {nextToken} -> nextToken) (\s@GetComplianceDetailsByConfigRuleResponse' {} a -> s {nextToken = a} :: GetComplianceDetailsByConfigRuleResponse)

-- | Indicates whether the AWS resource complies with the specified AWS
-- Config rule.
getComplianceDetailsByConfigRuleResponse_evaluationResults :: Lens.Lens' GetComplianceDetailsByConfigRuleResponse (Prelude.Maybe [EvaluationResult])
getComplianceDetailsByConfigRuleResponse_evaluationResults = Lens.lens (\GetComplianceDetailsByConfigRuleResponse' {evaluationResults} -> evaluationResults) (\s@GetComplianceDetailsByConfigRuleResponse' {} a -> s {evaluationResults = a} :: GetComplianceDetailsByConfigRuleResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getComplianceDetailsByConfigRuleResponse_httpStatus :: Lens.Lens' GetComplianceDetailsByConfigRuleResponse Prelude.Int
getComplianceDetailsByConfigRuleResponse_httpStatus = Lens.lens (\GetComplianceDetailsByConfigRuleResponse' {httpStatus} -> httpStatus) (\s@GetComplianceDetailsByConfigRuleResponse' {} a -> s {httpStatus = a} :: GetComplianceDetailsByConfigRuleResponse)

instance
  Prelude.NFData
    GetComplianceDetailsByConfigRuleResponse
