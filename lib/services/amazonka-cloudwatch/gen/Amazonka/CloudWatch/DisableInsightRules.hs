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
-- Module      : Amazonka.CloudWatch.DisableInsightRules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the specified Contributor Insights rules. When rules are
-- disabled, they do not analyze log groups and do not incur costs.
module Amazonka.CloudWatch.DisableInsightRules
  ( -- * Creating a Request
    DisableInsightRules (..),
    newDisableInsightRules,

    -- * Request Lenses
    disableInsightRules_ruleNames,

    -- * Destructuring the Response
    DisableInsightRulesResponse (..),
    newDisableInsightRulesResponse,

    -- * Response Lenses
    disableInsightRulesResponse_failures,
    disableInsightRulesResponse_httpStatus,
  )
where

import Amazonka.CloudWatch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisableInsightRules' smart constructor.
data DisableInsightRules = DisableInsightRules'
  { -- | An array of the rule names to disable. If you need to find out the names
    -- of your rules, use
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules>.
    ruleNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableInsightRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleNames', 'disableInsightRules_ruleNames' - An array of the rule names to disable. If you need to find out the names
-- of your rules, use
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules>.
newDisableInsightRules ::
  DisableInsightRules
newDisableInsightRules =
  DisableInsightRules' {ruleNames = Prelude.mempty}

-- | An array of the rule names to disable. If you need to find out the names
-- of your rules, use
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules>.
disableInsightRules_ruleNames :: Lens.Lens' DisableInsightRules [Prelude.Text]
disableInsightRules_ruleNames = Lens.lens (\DisableInsightRules' {ruleNames} -> ruleNames) (\s@DisableInsightRules' {} a -> s {ruleNames = a} :: DisableInsightRules) Prelude.. Lens.coerced

instance Core.AWSRequest DisableInsightRules where
  type
    AWSResponse DisableInsightRules =
      DisableInsightRulesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DisableInsightRulesResult"
      ( \s h x ->
          DisableInsightRulesResponse'
            Prelude.<$> ( x Data..@? "Failures" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisableInsightRules where
  hashWithSalt _salt DisableInsightRules' {..} =
    _salt `Prelude.hashWithSalt` ruleNames

instance Prelude.NFData DisableInsightRules where
  rnf DisableInsightRules' {..} = Prelude.rnf ruleNames

instance Data.ToHeaders DisableInsightRules where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DisableInsightRules where
  toPath = Prelude.const "/"

instance Data.ToQuery DisableInsightRules where
  toQuery DisableInsightRules' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DisableInsightRules" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-08-01" :: Prelude.ByteString),
        "RuleNames"
          Data.=: Data.toQueryList "member" ruleNames
      ]

-- | /See:/ 'newDisableInsightRulesResponse' smart constructor.
data DisableInsightRulesResponse = DisableInsightRulesResponse'
  { -- | An array listing the rules that could not be disabled. You cannot
    -- disable built-in rules.
    failures :: Prelude.Maybe [PartialFailure],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableInsightRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failures', 'disableInsightRulesResponse_failures' - An array listing the rules that could not be disabled. You cannot
-- disable built-in rules.
--
-- 'httpStatus', 'disableInsightRulesResponse_httpStatus' - The response's http status code.
newDisableInsightRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisableInsightRulesResponse
newDisableInsightRulesResponse pHttpStatus_ =
  DisableInsightRulesResponse'
    { failures =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array listing the rules that could not be disabled. You cannot
-- disable built-in rules.
disableInsightRulesResponse_failures :: Lens.Lens' DisableInsightRulesResponse (Prelude.Maybe [PartialFailure])
disableInsightRulesResponse_failures = Lens.lens (\DisableInsightRulesResponse' {failures} -> failures) (\s@DisableInsightRulesResponse' {} a -> s {failures = a} :: DisableInsightRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
disableInsightRulesResponse_httpStatus :: Lens.Lens' DisableInsightRulesResponse Prelude.Int
disableInsightRulesResponse_httpStatus = Lens.lens (\DisableInsightRulesResponse' {httpStatus} -> httpStatus) (\s@DisableInsightRulesResponse' {} a -> s {httpStatus = a} :: DisableInsightRulesResponse)

instance Prelude.NFData DisableInsightRulesResponse where
  rnf DisableInsightRulesResponse' {..} =
    Prelude.rnf failures `Prelude.seq`
      Prelude.rnf httpStatus
