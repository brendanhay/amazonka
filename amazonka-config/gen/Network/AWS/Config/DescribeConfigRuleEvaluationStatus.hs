{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeConfigRuleEvaluationStatus' smart constructor.
data DescribeConfigRuleEvaluationStatus = DescribeConfigRuleEvaluationStatus'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the AWS managed Config rules for which you want status
    -- information. If you do not specify any names, AWS Config returns status
    -- information for all AWS managed Config rules that you use.
    configRuleNames :: Prelude.Maybe [Prelude.Text],
    -- | The number of rule evaluation results that you want returned.
    --
    -- This parameter is required if the rule limit for your account is more
    -- than the default of 150 rules.
    --
    -- For information about requesting a rule limit increase, see
    -- <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_config AWS Config Limits>
    -- in the /AWS General Reference Guide/.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      configRuleNames = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeConfigRuleEvaluationStatus_nextToken :: Lens.Lens' DescribeConfigRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
describeConfigRuleEvaluationStatus_nextToken = Lens.lens (\DescribeConfigRuleEvaluationStatus' {nextToken} -> nextToken) (\s@DescribeConfigRuleEvaluationStatus' {} a -> s {nextToken = a} :: DescribeConfigRuleEvaluationStatus)

-- | The name of the AWS managed Config rules for which you want status
-- information. If you do not specify any names, AWS Config returns status
-- information for all AWS managed Config rules that you use.
describeConfigRuleEvaluationStatus_configRuleNames :: Lens.Lens' DescribeConfigRuleEvaluationStatus (Prelude.Maybe [Prelude.Text])
describeConfigRuleEvaluationStatus_configRuleNames = Lens.lens (\DescribeConfigRuleEvaluationStatus' {configRuleNames} -> configRuleNames) (\s@DescribeConfigRuleEvaluationStatus' {} a -> s {configRuleNames = a} :: DescribeConfigRuleEvaluationStatus) Prelude.. Lens.mapping Prelude._Coerce

-- | The number of rule evaluation results that you want returned.
--
-- This parameter is required if the rule limit for your account is more
-- than the default of 150 rules.
--
-- For information about requesting a rule limit increase, see
-- <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_config AWS Config Limits>
-- in the /AWS General Reference Guide/.
describeConfigRuleEvaluationStatus_limit :: Lens.Lens' DescribeConfigRuleEvaluationStatus (Prelude.Maybe Prelude.Natural)
describeConfigRuleEvaluationStatus_limit = Lens.lens (\DescribeConfigRuleEvaluationStatus' {limit} -> limit) (\s@DescribeConfigRuleEvaluationStatus' {} a -> s {limit = a} :: DescribeConfigRuleEvaluationStatus)

instance
  Pager.AWSPager
    DescribeConfigRuleEvaluationStatus
  where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeConfigRuleEvaluationStatusResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeConfigRuleEvaluationStatusResponse_configRulesEvaluationStatus
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeConfigRuleEvaluationStatus_nextToken
          Lens..~ rs
          Lens.^? describeConfigRuleEvaluationStatusResponse_nextToken
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    DescribeConfigRuleEvaluationStatus
  where
  type
    Rs DescribeConfigRuleEvaluationStatus =
      DescribeConfigRuleEvaluationStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConfigRuleEvaluationStatusResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
              Prelude.<*> ( x Prelude..?> "ConfigRulesEvaluationStatus"
                              Prelude..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeConfigRuleEvaluationStatus

instance
  Prelude.NFData
    DescribeConfigRuleEvaluationStatus

instance
  Prelude.ToHeaders
    DescribeConfigRuleEvaluationStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StarlingDoveService.DescribeConfigRuleEvaluationStatus" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    DescribeConfigRuleEvaluationStatus
  where
  toJSON DescribeConfigRuleEvaluationStatus' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("ConfigRuleNames" Prelude..=)
              Prelude.<$> configRuleNames,
            ("Limit" Prelude..=) Prelude.<$> limit
          ]
      )

instance
  Prelude.ToPath
    DescribeConfigRuleEvaluationStatus
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DescribeConfigRuleEvaluationStatus
  where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeConfigRuleEvaluationStatusResponse' smart constructor.
data DescribeConfigRuleEvaluationStatusResponse = DescribeConfigRuleEvaluationStatusResponse'
  { -- | The string that you use in a subsequent request to get the next page of
    -- results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Status information about your AWS managed Config rules.
    configRulesEvaluationStatus :: Prelude.Maybe [ConfigRuleEvaluationStatus],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeConfigRuleEvaluationStatusResponse
newDescribeConfigRuleEvaluationStatusResponse
  pHttpStatus_ =
    DescribeConfigRuleEvaluationStatusResponse'
      { nextToken =
          Prelude.Nothing,
        configRulesEvaluationStatus =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
describeConfigRuleEvaluationStatusResponse_nextToken :: Lens.Lens' DescribeConfigRuleEvaluationStatusResponse (Prelude.Maybe Prelude.Text)
describeConfigRuleEvaluationStatusResponse_nextToken = Lens.lens (\DescribeConfigRuleEvaluationStatusResponse' {nextToken} -> nextToken) (\s@DescribeConfigRuleEvaluationStatusResponse' {} a -> s {nextToken = a} :: DescribeConfigRuleEvaluationStatusResponse)

-- | Status information about your AWS managed Config rules.
describeConfigRuleEvaluationStatusResponse_configRulesEvaluationStatus :: Lens.Lens' DescribeConfigRuleEvaluationStatusResponse (Prelude.Maybe [ConfigRuleEvaluationStatus])
describeConfigRuleEvaluationStatusResponse_configRulesEvaluationStatus = Lens.lens (\DescribeConfigRuleEvaluationStatusResponse' {configRulesEvaluationStatus} -> configRulesEvaluationStatus) (\s@DescribeConfigRuleEvaluationStatusResponse' {} a -> s {configRulesEvaluationStatus = a} :: DescribeConfigRuleEvaluationStatusResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeConfigRuleEvaluationStatusResponse_httpStatus :: Lens.Lens' DescribeConfigRuleEvaluationStatusResponse Prelude.Int
describeConfigRuleEvaluationStatusResponse_httpStatus = Lens.lens (\DescribeConfigRuleEvaluationStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeConfigRuleEvaluationStatusResponse' {} a -> s {httpStatus = a} :: DescribeConfigRuleEvaluationStatusResponse)

instance
  Prelude.NFData
    DescribeConfigRuleEvaluationStatusResponse
