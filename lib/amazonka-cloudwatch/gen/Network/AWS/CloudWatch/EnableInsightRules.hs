{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.EnableInsightRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified Contributor Insights rules. When rules are enabled, they immediately begin analyzing log data.
module Network.AWS.CloudWatch.EnableInsightRules
  ( -- * Creating a request
    EnableInsightRules (..),
    mkEnableInsightRules,

    -- ** Request lenses
    eirRuleNames,

    -- * Destructuring the response
    EnableInsightRulesResponse (..),
    mkEnableInsightRulesResponse,

    -- ** Response lenses
    eirrsFailures,
    eirrsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkEnableInsightRules' smart constructor.
newtype EnableInsightRules = EnableInsightRules'
  { -- | An array of the rule names to enable. If you need to find out the names of your rules, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules> .
    ruleNames :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableInsightRules' with the minimum fields required to make a request.
--
-- * 'ruleNames' - An array of the rule names to enable. If you need to find out the names of your rules, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules> .
mkEnableInsightRules ::
  EnableInsightRules
mkEnableInsightRules = EnableInsightRules' {ruleNames = Lude.mempty}

-- | An array of the rule names to enable. If you need to find out the names of your rules, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules> .
--
-- /Note:/ Consider using 'ruleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirRuleNames :: Lens.Lens' EnableInsightRules [Lude.Text]
eirRuleNames = Lens.lens (ruleNames :: EnableInsightRules -> [Lude.Text]) (\s a -> s {ruleNames = a} :: EnableInsightRules)
{-# DEPRECATED eirRuleNames "Use generic-lens or generic-optics with 'ruleNames' instead." #-}

instance Lude.AWSRequest EnableInsightRules where
  type Rs EnableInsightRules = EnableInsightRulesResponse
  request = Req.postQuery cloudWatchService
  response =
    Res.receiveXMLWrapper
      "EnableInsightRulesResult"
      ( \s h x ->
          EnableInsightRulesResponse'
            Lude.<$> ( x Lude..@? "Failures" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders EnableInsightRules where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath EnableInsightRules where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableInsightRules where
  toQuery EnableInsightRules' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("EnableInsightRules" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "RuleNames" Lude.=: Lude.toQueryList "member" ruleNames
      ]

-- | /See:/ 'mkEnableInsightRulesResponse' smart constructor.
data EnableInsightRulesResponse = EnableInsightRulesResponse'
  { -- | An array listing the rules that could not be enabled. You cannot disable or enable built-in rules.
    failures :: Lude.Maybe [PartialFailure],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableInsightRulesResponse' with the minimum fields required to make a request.
--
-- * 'failures' - An array listing the rules that could not be enabled. You cannot disable or enable built-in rules.
-- * 'responseStatus' - The response status code.
mkEnableInsightRulesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  EnableInsightRulesResponse
mkEnableInsightRulesResponse pResponseStatus_ =
  EnableInsightRulesResponse'
    { failures = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array listing the rules that could not be enabled. You cannot disable or enable built-in rules.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrsFailures :: Lens.Lens' EnableInsightRulesResponse (Lude.Maybe [PartialFailure])
eirrsFailures = Lens.lens (failures :: EnableInsightRulesResponse -> Lude.Maybe [PartialFailure]) (\s a -> s {failures = a} :: EnableInsightRulesResponse)
{-# DEPRECATED eirrsFailures "Use generic-lens or generic-optics with 'failures' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirrsResponseStatus :: Lens.Lens' EnableInsightRulesResponse Lude.Int
eirrsResponseStatus = Lens.lens (responseStatus :: EnableInsightRulesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: EnableInsightRulesResponse)
{-# DEPRECATED eirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
