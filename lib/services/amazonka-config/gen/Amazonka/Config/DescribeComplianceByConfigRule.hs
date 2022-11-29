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
-- Module      : Amazonka.Config.DescribeComplianceByConfigRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Indicates whether the specified Config rules are compliant. If a rule is
-- noncompliant, this action returns the number of Amazon Web Services
-- resources that do not comply with the rule.
--
-- A rule is compliant if all of the evaluated resources comply with it. It
-- is noncompliant if any of these resources do not comply.
--
-- If Config has no current evaluation results for the rule, it returns
-- @INSUFFICIENT_DATA@. This result might indicate one of the following
-- conditions:
--
-- -   Config has never invoked an evaluation for the rule. To check
--     whether it has, use the @DescribeConfigRuleEvaluationStatus@ action
--     to get the @LastSuccessfulInvocationTime@ and
--     @LastFailedInvocationTime@.
--
-- -   The rule\'s Lambda function is failing to send evaluation results to
--     Config. Verify that the role you assigned to your configuration
--     recorder includes the @config:PutEvaluations@ permission. If the
--     rule is a custom rule, verify that the Lambda execution role
--     includes the @config:PutEvaluations@ permission.
--
-- -   The rule\'s Lambda function has returned @NOT_APPLICABLE@ for all
--     evaluation results. This can occur if the resources were deleted or
--     removed from the rule\'s scope.
--
-- This operation returns paginated results.
module Amazonka.Config.DescribeComplianceByConfigRule
  ( -- * Creating a Request
    DescribeComplianceByConfigRule (..),
    newDescribeComplianceByConfigRule,

    -- * Request Lenses
    describeComplianceByConfigRule_nextToken,
    describeComplianceByConfigRule_complianceTypes,
    describeComplianceByConfigRule_configRuleNames,

    -- * Destructuring the Response
    DescribeComplianceByConfigRuleResponse (..),
    newDescribeComplianceByConfigRuleResponse,

    -- * Response Lenses
    describeComplianceByConfigRuleResponse_nextToken,
    describeComplianceByConfigRuleResponse_complianceByConfigRules,
    describeComplianceByConfigRuleResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeComplianceByConfigRule' smart constructor.
data DescribeComplianceByConfigRule = DescribeComplianceByConfigRule'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters the results by compliance.
    --
    -- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@.
    complianceTypes :: Prelude.Maybe [ComplianceType],
    -- | Specify one or more Config rule names to filter the results by rule.
    configRuleNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeComplianceByConfigRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeComplianceByConfigRule_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'complianceTypes', 'describeComplianceByConfigRule_complianceTypes' - Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@.
--
-- 'configRuleNames', 'describeComplianceByConfigRule_configRuleNames' - Specify one or more Config rule names to filter the results by rule.
newDescribeComplianceByConfigRule ::
  DescribeComplianceByConfigRule
newDescribeComplianceByConfigRule =
  DescribeComplianceByConfigRule'
    { nextToken =
        Prelude.Nothing,
      complianceTypes = Prelude.Nothing,
      configRuleNames = Prelude.Nothing
    }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeComplianceByConfigRule_nextToken :: Lens.Lens' DescribeComplianceByConfigRule (Prelude.Maybe Prelude.Text)
describeComplianceByConfigRule_nextToken = Lens.lens (\DescribeComplianceByConfigRule' {nextToken} -> nextToken) (\s@DescribeComplianceByConfigRule' {} a -> s {nextToken = a} :: DescribeComplianceByConfigRule)

-- | Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ and @NON_COMPLIANT@.
describeComplianceByConfigRule_complianceTypes :: Lens.Lens' DescribeComplianceByConfigRule (Prelude.Maybe [ComplianceType])
describeComplianceByConfigRule_complianceTypes = Lens.lens (\DescribeComplianceByConfigRule' {complianceTypes} -> complianceTypes) (\s@DescribeComplianceByConfigRule' {} a -> s {complianceTypes = a} :: DescribeComplianceByConfigRule) Prelude.. Lens.mapping Lens.coerced

-- | Specify one or more Config rule names to filter the results by rule.
describeComplianceByConfigRule_configRuleNames :: Lens.Lens' DescribeComplianceByConfigRule (Prelude.Maybe [Prelude.Text])
describeComplianceByConfigRule_configRuleNames = Lens.lens (\DescribeComplianceByConfigRule' {configRuleNames} -> configRuleNames) (\s@DescribeComplianceByConfigRule' {} a -> s {configRuleNames = a} :: DescribeComplianceByConfigRule) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeComplianceByConfigRule where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeComplianceByConfigRuleResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeComplianceByConfigRuleResponse_complianceByConfigRules
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeComplianceByConfigRule_nextToken
          Lens..~ rs
          Lens.^? describeComplianceByConfigRuleResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeComplianceByConfigRule
  where
  type
    AWSResponse DescribeComplianceByConfigRule =
      DescribeComplianceByConfigRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeComplianceByConfigRuleResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "ComplianceByConfigRules"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeComplianceByConfigRule
  where
  hashWithSalt
    _salt
    DescribeComplianceByConfigRule' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` complianceTypes
        `Prelude.hashWithSalt` configRuleNames

instance
  Prelude.NFData
    DescribeComplianceByConfigRule
  where
  rnf DescribeComplianceByConfigRule' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf complianceTypes
      `Prelude.seq` Prelude.rnf configRuleNames

instance
  Core.ToHeaders
    DescribeComplianceByConfigRule
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DescribeComplianceByConfigRule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeComplianceByConfigRule where
  toJSON DescribeComplianceByConfigRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("ComplianceTypes" Core..=)
              Prelude.<$> complianceTypes,
            ("ConfigRuleNames" Core..=)
              Prelude.<$> configRuleNames
          ]
      )

instance Core.ToPath DescribeComplianceByConfigRule where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeComplianceByConfigRule where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeComplianceByConfigRuleResponse' smart constructor.
data DescribeComplianceByConfigRuleResponse = DescribeComplianceByConfigRuleResponse'
  { -- | The string that you use in a subsequent request to get the next page of
    -- results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether each of the specified Config rules is compliant.
    complianceByConfigRules :: Prelude.Maybe [ComplianceByConfigRule],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeComplianceByConfigRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeComplianceByConfigRuleResponse_nextToken' - The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
--
-- 'complianceByConfigRules', 'describeComplianceByConfigRuleResponse_complianceByConfigRules' - Indicates whether each of the specified Config rules is compliant.
--
-- 'httpStatus', 'describeComplianceByConfigRuleResponse_httpStatus' - The response's http status code.
newDescribeComplianceByConfigRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeComplianceByConfigRuleResponse
newDescribeComplianceByConfigRuleResponse
  pHttpStatus_ =
    DescribeComplianceByConfigRuleResponse'
      { nextToken =
          Prelude.Nothing,
        complianceByConfigRules =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
describeComplianceByConfigRuleResponse_nextToken :: Lens.Lens' DescribeComplianceByConfigRuleResponse (Prelude.Maybe Prelude.Text)
describeComplianceByConfigRuleResponse_nextToken = Lens.lens (\DescribeComplianceByConfigRuleResponse' {nextToken} -> nextToken) (\s@DescribeComplianceByConfigRuleResponse' {} a -> s {nextToken = a} :: DescribeComplianceByConfigRuleResponse)

-- | Indicates whether each of the specified Config rules is compliant.
describeComplianceByConfigRuleResponse_complianceByConfigRules :: Lens.Lens' DescribeComplianceByConfigRuleResponse (Prelude.Maybe [ComplianceByConfigRule])
describeComplianceByConfigRuleResponse_complianceByConfigRules = Lens.lens (\DescribeComplianceByConfigRuleResponse' {complianceByConfigRules} -> complianceByConfigRules) (\s@DescribeComplianceByConfigRuleResponse' {} a -> s {complianceByConfigRules = a} :: DescribeComplianceByConfigRuleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeComplianceByConfigRuleResponse_httpStatus :: Lens.Lens' DescribeComplianceByConfigRuleResponse Prelude.Int
describeComplianceByConfigRuleResponse_httpStatus = Lens.lens (\DescribeComplianceByConfigRuleResponse' {httpStatus} -> httpStatus) (\s@DescribeComplianceByConfigRuleResponse' {} a -> s {httpStatus = a} :: DescribeComplianceByConfigRuleResponse)

instance
  Prelude.NFData
    DescribeComplianceByConfigRuleResponse
  where
  rnf DescribeComplianceByConfigRuleResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf complianceByConfigRules
      `Prelude.seq` Prelude.rnf httpStatus
