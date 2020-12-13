{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DeleteInsightRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes the specified Contributor Insights rules.
--
-- If you create a rule, delete it, and then re-create it with the same name, historical data from the first time the rule was created might not be available.
module Network.AWS.CloudWatch.DeleteInsightRules
  ( -- * Creating a request
    DeleteInsightRules (..),
    mkDeleteInsightRules,

    -- ** Request lenses
    dRuleNames,

    -- * Destructuring the response
    DeleteInsightRulesResponse (..),
    mkDeleteInsightRulesResponse,

    -- ** Response lenses
    dirrsFailures,
    dirrsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteInsightRules' smart constructor.
newtype DeleteInsightRules = DeleteInsightRules'
  { -- | An array of the rule names to delete. If you need to find out the names of your rules, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules> .
    ruleNames :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteInsightRules' with the minimum fields required to make a request.
--
-- * 'ruleNames' - An array of the rule names to delete. If you need to find out the names of your rules, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules> .
mkDeleteInsightRules ::
  DeleteInsightRules
mkDeleteInsightRules = DeleteInsightRules' {ruleNames = Lude.mempty}

-- | An array of the rule names to delete. If you need to find out the names of your rules, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules> .
--
-- /Note:/ Consider using 'ruleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRuleNames :: Lens.Lens' DeleteInsightRules [Lude.Text]
dRuleNames = Lens.lens (ruleNames :: DeleteInsightRules -> [Lude.Text]) (\s a -> s {ruleNames = a} :: DeleteInsightRules)
{-# DEPRECATED dRuleNames "Use generic-lens or generic-optics with 'ruleNames' instead." #-}

instance Lude.AWSRequest DeleteInsightRules where
  type Rs DeleteInsightRules = DeleteInsightRulesResponse
  request = Req.postQuery cloudWatchService
  response =
    Res.receiveXMLWrapper
      "DeleteInsightRulesResult"
      ( \s h x ->
          DeleteInsightRulesResponse'
            Lude.<$> ( x Lude..@? "Failures" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteInsightRules where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteInsightRules where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteInsightRules where
  toQuery DeleteInsightRules' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteInsightRules" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "RuleNames" Lude.=: Lude.toQueryList "member" ruleNames
      ]

-- | /See:/ 'mkDeleteInsightRulesResponse' smart constructor.
data DeleteInsightRulesResponse = DeleteInsightRulesResponse'
  { -- | An array listing the rules that could not be deleted. You cannot delete built-in rules.
    failures :: Lude.Maybe [PartialFailure],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteInsightRulesResponse' with the minimum fields required to make a request.
--
-- * 'failures' - An array listing the rules that could not be deleted. You cannot delete built-in rules.
-- * 'responseStatus' - The response status code.
mkDeleteInsightRulesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteInsightRulesResponse
mkDeleteInsightRulesResponse pResponseStatus_ =
  DeleteInsightRulesResponse'
    { failures = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array listing the rules that could not be deleted. You cannot delete built-in rules.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsFailures :: Lens.Lens' DeleteInsightRulesResponse (Lude.Maybe [PartialFailure])
dirrsFailures = Lens.lens (failures :: DeleteInsightRulesResponse -> Lude.Maybe [PartialFailure]) (\s a -> s {failures = a} :: DeleteInsightRulesResponse)
{-# DEPRECATED dirrsFailures "Use generic-lens or generic-optics with 'failures' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsResponseStatus :: Lens.Lens' DeleteInsightRulesResponse Lude.Int
dirrsResponseStatus = Lens.lens (responseStatus :: DeleteInsightRulesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteInsightRulesResponse)
{-# DEPRECATED dirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
