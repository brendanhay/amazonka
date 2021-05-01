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
-- Module      : Network.AWS.CloudWatch.DisableInsightRules
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the specified Contributor Insights rules. When rules are
-- disabled, they do not analyze log groups and do not incur costs.
module Network.AWS.CloudWatch.DisableInsightRules
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

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisableInsightRules' smart constructor.
data DisableInsightRules = DisableInsightRules'
  { -- | An array of the rule names to disable. If you need to find out the names
    -- of your rules, use
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules>.
    ruleNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
disableInsightRules_ruleNames = Lens.lens (\DisableInsightRules' {ruleNames} -> ruleNames) (\s@DisableInsightRules' {} a -> s {ruleNames = a} :: DisableInsightRules) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest DisableInsightRules where
  type
    Rs DisableInsightRules =
      DisableInsightRulesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DisableInsightRulesResult"
      ( \s h x ->
          DisableInsightRulesResponse'
            Prelude.<$> ( x Prelude..@? "Failures" Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisableInsightRules

instance Prelude.NFData DisableInsightRules

instance Prelude.ToHeaders DisableInsightRules where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DisableInsightRules where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DisableInsightRules where
  toQuery DisableInsightRules' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DisableInsightRules" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-08-01" :: Prelude.ByteString),
        "RuleNames"
          Prelude.=: Prelude.toQueryList "member" ruleNames
      ]

-- | /See:/ 'newDisableInsightRulesResponse' smart constructor.
data DisableInsightRulesResponse = DisableInsightRulesResponse'
  { -- | An array listing the rules that could not be disabled. You cannot
    -- disable built-in rules.
    failures :: Prelude.Maybe [PartialFailure],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
disableInsightRulesResponse_failures = Lens.lens (\DisableInsightRulesResponse' {failures} -> failures) (\s@DisableInsightRulesResponse' {} a -> s {failures = a} :: DisableInsightRulesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
disableInsightRulesResponse_httpStatus :: Lens.Lens' DisableInsightRulesResponse Prelude.Int
disableInsightRulesResponse_httpStatus = Lens.lens (\DisableInsightRulesResponse' {httpStatus} -> httpStatus) (\s@DisableInsightRulesResponse' {} a -> s {httpStatus = a} :: DisableInsightRulesResponse)

instance Prelude.NFData DisableInsightRulesResponse
