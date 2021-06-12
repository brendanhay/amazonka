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
-- Module      : Network.AWS.CloudWatch.EnableInsightRules
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified Contributor Insights rules. When rules are
-- enabled, they immediately begin analyzing log data.
module Network.AWS.CloudWatch.EnableInsightRules
  ( -- * Creating a Request
    EnableInsightRules (..),
    newEnableInsightRules,

    -- * Request Lenses
    enableInsightRules_ruleNames,

    -- * Destructuring the Response
    EnableInsightRulesResponse (..),
    newEnableInsightRulesResponse,

    -- * Response Lenses
    enableInsightRulesResponse_failures,
    enableInsightRulesResponse_httpStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newEnableInsightRules' smart constructor.
data EnableInsightRules = EnableInsightRules'
  { -- | An array of the rule names to enable. If you need to find out the names
    -- of your rules, use
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules>.
    ruleNames :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableInsightRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleNames', 'enableInsightRules_ruleNames' - An array of the rule names to enable. If you need to find out the names
-- of your rules, use
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules>.
newEnableInsightRules ::
  EnableInsightRules
newEnableInsightRules =
  EnableInsightRules' {ruleNames = Core.mempty}

-- | An array of the rule names to enable. If you need to find out the names
-- of your rules, use
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules>.
enableInsightRules_ruleNames :: Lens.Lens' EnableInsightRules [Core.Text]
enableInsightRules_ruleNames = Lens.lens (\EnableInsightRules' {ruleNames} -> ruleNames) (\s@EnableInsightRules' {} a -> s {ruleNames = a} :: EnableInsightRules) Core.. Lens._Coerce

instance Core.AWSRequest EnableInsightRules where
  type
    AWSResponse EnableInsightRules =
      EnableInsightRulesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "EnableInsightRulesResult"
      ( \s h x ->
          EnableInsightRulesResponse'
            Core.<$> ( x Core..@? "Failures" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable EnableInsightRules

instance Core.NFData EnableInsightRules

instance Core.ToHeaders EnableInsightRules where
  toHeaders = Core.const Core.mempty

instance Core.ToPath EnableInsightRules where
  toPath = Core.const "/"

instance Core.ToQuery EnableInsightRules where
  toQuery EnableInsightRules' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("EnableInsightRules" :: Core.ByteString),
        "Version" Core.=: ("2010-08-01" :: Core.ByteString),
        "RuleNames"
          Core.=: Core.toQueryList "member" ruleNames
      ]

-- | /See:/ 'newEnableInsightRulesResponse' smart constructor.
data EnableInsightRulesResponse = EnableInsightRulesResponse'
  { -- | An array listing the rules that could not be enabled. You cannot disable
    -- or enable built-in rules.
    failures :: Core.Maybe [PartialFailure],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableInsightRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failures', 'enableInsightRulesResponse_failures' - An array listing the rules that could not be enabled. You cannot disable
-- or enable built-in rules.
--
-- 'httpStatus', 'enableInsightRulesResponse_httpStatus' - The response's http status code.
newEnableInsightRulesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  EnableInsightRulesResponse
newEnableInsightRulesResponse pHttpStatus_ =
  EnableInsightRulesResponse'
    { failures =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array listing the rules that could not be enabled. You cannot disable
-- or enable built-in rules.
enableInsightRulesResponse_failures :: Lens.Lens' EnableInsightRulesResponse (Core.Maybe [PartialFailure])
enableInsightRulesResponse_failures = Lens.lens (\EnableInsightRulesResponse' {failures} -> failures) (\s@EnableInsightRulesResponse' {} a -> s {failures = a} :: EnableInsightRulesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
enableInsightRulesResponse_httpStatus :: Lens.Lens' EnableInsightRulesResponse Core.Int
enableInsightRulesResponse_httpStatus = Lens.lens (\EnableInsightRulesResponse' {httpStatus} -> httpStatus) (\s@EnableInsightRulesResponse' {} a -> s {httpStatus = a} :: EnableInsightRulesResponse)

instance Core.NFData EnableInsightRulesResponse
