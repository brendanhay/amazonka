{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DisableInsightRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the specified Contributor Insights rules. When rules are disabled, they do not analyze log groups and do not incur costs.
module Network.AWS.CloudWatch.DisableInsightRules
  ( -- * Creating a request
    DisableInsightRules (..),
    mkDisableInsightRules,

    -- ** Request lenses
    dirRuleNames,

    -- * Destructuring the response
    DisableInsightRulesResponse (..),
    mkDisableInsightRulesResponse,

    -- ** Response lenses
    dirsrsFailures,
    dirsrsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisableInsightRules' smart constructor.
newtype DisableInsightRules = DisableInsightRules'
  { -- | An array of the rule names to disable. If you need to find out the names of your rules, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules> .
    ruleNames :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableInsightRules' with the minimum fields required to make a request.
--
-- * 'ruleNames' - An array of the rule names to disable. If you need to find out the names of your rules, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules> .
mkDisableInsightRules ::
  DisableInsightRules
mkDisableInsightRules =
  DisableInsightRules' {ruleNames = Lude.mempty}

-- | An array of the rule names to disable. If you need to find out the names of your rules, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules> .
--
-- /Note:/ Consider using 'ruleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirRuleNames :: Lens.Lens' DisableInsightRules [Lude.Text]
dirRuleNames = Lens.lens (ruleNames :: DisableInsightRules -> [Lude.Text]) (\s a -> s {ruleNames = a} :: DisableInsightRules)
{-# DEPRECATED dirRuleNames "Use generic-lens or generic-optics with 'ruleNames' instead." #-}

instance Lude.AWSRequest DisableInsightRules where
  type Rs DisableInsightRules = DisableInsightRulesResponse
  request = Req.postQuery cloudWatchService
  response =
    Res.receiveXMLWrapper
      "DisableInsightRulesResult"
      ( \s h x ->
          DisableInsightRulesResponse'
            Lude.<$> ( x Lude..@? "Failures" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisableInsightRules where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DisableInsightRules where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableInsightRules where
  toQuery DisableInsightRules' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DisableInsightRules" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "RuleNames" Lude.=: Lude.toQueryList "member" ruleNames
      ]

-- | /See:/ 'mkDisableInsightRulesResponse' smart constructor.
data DisableInsightRulesResponse = DisableInsightRulesResponse'
  { -- | An array listing the rules that could not be disabled. You cannot disable built-in rules.
    failures :: Lude.Maybe [PartialFailure],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableInsightRulesResponse' with the minimum fields required to make a request.
--
-- * 'failures' - An array listing the rules that could not be disabled. You cannot disable built-in rules.
-- * 'responseStatus' - The response status code.
mkDisableInsightRulesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisableInsightRulesResponse
mkDisableInsightRulesResponse pResponseStatus_ =
  DisableInsightRulesResponse'
    { failures = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array listing the rules that could not be disabled. You cannot disable built-in rules.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsrsFailures :: Lens.Lens' DisableInsightRulesResponse (Lude.Maybe [PartialFailure])
dirsrsFailures = Lens.lens (failures :: DisableInsightRulesResponse -> Lude.Maybe [PartialFailure]) (\s a -> s {failures = a} :: DisableInsightRulesResponse)
{-# DEPRECATED dirsrsFailures "Use generic-lens or generic-optics with 'failures' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsrsResponseStatus :: Lens.Lens' DisableInsightRulesResponse Lude.Int
dirsrsResponseStatus = Lens.lens (responseStatus :: DisableInsightRulesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisableInsightRulesResponse)
{-# DEPRECATED dirsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
