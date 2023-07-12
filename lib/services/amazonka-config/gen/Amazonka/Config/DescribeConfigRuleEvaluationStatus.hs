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
-- Module      : Amazonka.Config.DescribeConfigRuleEvaluationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns status information for each of your Config managed rules. The
-- status includes information such as the last time Config invoked the
-- rule, the last time Config failed to invoke the rule, and the related
-- error for the last failure.
--
-- This operation returns paginated results.
module Amazonka.Config.DescribeConfigRuleEvaluationStatus
  ( -- * Creating a Request
    DescribeConfigRuleEvaluationStatus (..),
    newDescribeConfigRuleEvaluationStatus,

    -- * Request Lenses
    describeConfigRuleEvaluationStatus_configRuleNames,
    describeConfigRuleEvaluationStatus_limit,
    describeConfigRuleEvaluationStatus_nextToken,

    -- * Destructuring the Response
    DescribeConfigRuleEvaluationStatusResponse (..),
    newDescribeConfigRuleEvaluationStatusResponse,

    -- * Response Lenses
    describeConfigRuleEvaluationStatusResponse_configRulesEvaluationStatus,
    describeConfigRuleEvaluationStatusResponse_nextToken,
    describeConfigRuleEvaluationStatusResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeConfigRuleEvaluationStatus' smart constructor.
data DescribeConfigRuleEvaluationStatus = DescribeConfigRuleEvaluationStatus'
  { -- | The name of the Config managed rules for which you want status
    -- information. If you do not specify any names, Config returns status
    -- information for all Config managed rules that you use.
    configRuleNames :: Prelude.Maybe [Prelude.Text],
    -- | The number of rule evaluation results that you want returned.
    --
    -- This parameter is required if the rule limit for your account is more
    -- than the default of 150 rules.
    --
    -- For information about requesting a rule limit increase, see
    -- <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_config Config Limits>
    -- in the /Amazon Web Services General Reference Guide/.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConfigRuleEvaluationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configRuleNames', 'describeConfigRuleEvaluationStatus_configRuleNames' - The name of the Config managed rules for which you want status
-- information. If you do not specify any names, Config returns status
-- information for all Config managed rules that you use.
--
-- 'limit', 'describeConfigRuleEvaluationStatus_limit' - The number of rule evaluation results that you want returned.
--
-- This parameter is required if the rule limit for your account is more
-- than the default of 150 rules.
--
-- For information about requesting a rule limit increase, see
-- <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_config Config Limits>
-- in the /Amazon Web Services General Reference Guide/.
--
-- 'nextToken', 'describeConfigRuleEvaluationStatus_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
newDescribeConfigRuleEvaluationStatus ::
  DescribeConfigRuleEvaluationStatus
newDescribeConfigRuleEvaluationStatus =
  DescribeConfigRuleEvaluationStatus'
    { configRuleNames =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The name of the Config managed rules for which you want status
-- information. If you do not specify any names, Config returns status
-- information for all Config managed rules that you use.
describeConfigRuleEvaluationStatus_configRuleNames :: Lens.Lens' DescribeConfigRuleEvaluationStatus (Prelude.Maybe [Prelude.Text])
describeConfigRuleEvaluationStatus_configRuleNames = Lens.lens (\DescribeConfigRuleEvaluationStatus' {configRuleNames} -> configRuleNames) (\s@DescribeConfigRuleEvaluationStatus' {} a -> s {configRuleNames = a} :: DescribeConfigRuleEvaluationStatus) Prelude.. Lens.mapping Lens.coerced

-- | The number of rule evaluation results that you want returned.
--
-- This parameter is required if the rule limit for your account is more
-- than the default of 150 rules.
--
-- For information about requesting a rule limit increase, see
-- <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_config Config Limits>
-- in the /Amazon Web Services General Reference Guide/.
describeConfigRuleEvaluationStatus_limit :: Lens.Lens' DescribeConfigRuleEvaluationStatus (Prelude.Maybe Prelude.Natural)
describeConfigRuleEvaluationStatus_limit = Lens.lens (\DescribeConfigRuleEvaluationStatus' {limit} -> limit) (\s@DescribeConfigRuleEvaluationStatus' {} a -> s {limit = a} :: DescribeConfigRuleEvaluationStatus)

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeConfigRuleEvaluationStatus_nextToken :: Lens.Lens' DescribeConfigRuleEvaluationStatus (Prelude.Maybe Prelude.Text)
describeConfigRuleEvaluationStatus_nextToken = Lens.lens (\DescribeConfigRuleEvaluationStatus' {nextToken} -> nextToken) (\s@DescribeConfigRuleEvaluationStatus' {} a -> s {nextToken = a} :: DescribeConfigRuleEvaluationStatus)

instance
  Core.AWSPager
    DescribeConfigRuleEvaluationStatus
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeConfigRuleEvaluationStatusResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeConfigRuleEvaluationStatusResponse_configRulesEvaluationStatus
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeConfigRuleEvaluationStatus_nextToken
          Lens..~ rs
          Lens.^? describeConfigRuleEvaluationStatusResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeConfigRuleEvaluationStatus
  where
  type
    AWSResponse DescribeConfigRuleEvaluationStatus =
      DescribeConfigRuleEvaluationStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConfigRuleEvaluationStatusResponse'
            Prelude.<$> ( x
                            Data..?> "ConfigRulesEvaluationStatus"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeConfigRuleEvaluationStatus
  where
  hashWithSalt
    _salt
    DescribeConfigRuleEvaluationStatus' {..} =
      _salt
        `Prelude.hashWithSalt` configRuleNames
        `Prelude.hashWithSalt` limit
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeConfigRuleEvaluationStatus
  where
  rnf DescribeConfigRuleEvaluationStatus' {..} =
    Prelude.rnf configRuleNames
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribeConfigRuleEvaluationStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DescribeConfigRuleEvaluationStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeConfigRuleEvaluationStatus
  where
  toJSON DescribeConfigRuleEvaluationStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConfigRuleNames" Data..=)
              Prelude.<$> configRuleNames,
            ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance
  Data.ToPath
    DescribeConfigRuleEvaluationStatus
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeConfigRuleEvaluationStatus
  where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeConfigRuleEvaluationStatusResponse' smart constructor.
data DescribeConfigRuleEvaluationStatusResponse = DescribeConfigRuleEvaluationStatusResponse'
  { -- | Status information about your Config managed rules.
    configRulesEvaluationStatus :: Prelude.Maybe [ConfigRuleEvaluationStatus],
    -- | The string that you use in a subsequent request to get the next page of
    -- results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConfigRuleEvaluationStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configRulesEvaluationStatus', 'describeConfigRuleEvaluationStatusResponse_configRulesEvaluationStatus' - Status information about your Config managed rules.
--
-- 'nextToken', 'describeConfigRuleEvaluationStatusResponse_nextToken' - The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
--
-- 'httpStatus', 'describeConfigRuleEvaluationStatusResponse_httpStatus' - The response's http status code.
newDescribeConfigRuleEvaluationStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConfigRuleEvaluationStatusResponse
newDescribeConfigRuleEvaluationStatusResponse
  pHttpStatus_ =
    DescribeConfigRuleEvaluationStatusResponse'
      { configRulesEvaluationStatus =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Status information about your Config managed rules.
describeConfigRuleEvaluationStatusResponse_configRulesEvaluationStatus :: Lens.Lens' DescribeConfigRuleEvaluationStatusResponse (Prelude.Maybe [ConfigRuleEvaluationStatus])
describeConfigRuleEvaluationStatusResponse_configRulesEvaluationStatus = Lens.lens (\DescribeConfigRuleEvaluationStatusResponse' {configRulesEvaluationStatus} -> configRulesEvaluationStatus) (\s@DescribeConfigRuleEvaluationStatusResponse' {} a -> s {configRulesEvaluationStatus = a} :: DescribeConfigRuleEvaluationStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
describeConfigRuleEvaluationStatusResponse_nextToken :: Lens.Lens' DescribeConfigRuleEvaluationStatusResponse (Prelude.Maybe Prelude.Text)
describeConfigRuleEvaluationStatusResponse_nextToken = Lens.lens (\DescribeConfigRuleEvaluationStatusResponse' {nextToken} -> nextToken) (\s@DescribeConfigRuleEvaluationStatusResponse' {} a -> s {nextToken = a} :: DescribeConfigRuleEvaluationStatusResponse)

-- | The response's http status code.
describeConfigRuleEvaluationStatusResponse_httpStatus :: Lens.Lens' DescribeConfigRuleEvaluationStatusResponse Prelude.Int
describeConfigRuleEvaluationStatusResponse_httpStatus = Lens.lens (\DescribeConfigRuleEvaluationStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeConfigRuleEvaluationStatusResponse' {} a -> s {httpStatus = a} :: DescribeConfigRuleEvaluationStatusResponse)

instance
  Prelude.NFData
    DescribeConfigRuleEvaluationStatusResponse
  where
  rnf DescribeConfigRuleEvaluationStatusResponse' {..} =
    Prelude.rnf configRulesEvaluationStatus
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
