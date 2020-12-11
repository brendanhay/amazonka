{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DescribeInsightRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all the Contributor Insights rules in your account.
--
-- For more information about Contributor Insights, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights.html Using Contributor Insights to Analyze High-Cardinality Data> .
module Network.AWS.CloudWatch.DescribeInsightRules
  ( -- * Creating a request
    DescribeInsightRules (..),
    mkDescribeInsightRules,

    -- ** Request lenses
    dirNextToken,
    dirMaxResults,

    -- * Destructuring the response
    DescribeInsightRulesResponse (..),
    mkDescribeInsightRulesResponse,

    -- ** Response lenses
    drsNextToken,
    drsInsightRules,
    drsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeInsightRules' smart constructor.
data DescribeInsightRules = DescribeInsightRules'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInsightRules' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to return in one operation. If you omit this parameter, the default of 500 is used.
-- * 'nextToken' - Include this value, if it was returned by the previous operation, to get the next set of rules.
mkDescribeInsightRules ::
  DescribeInsightRules
mkDescribeInsightRules =
  DescribeInsightRules'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Include this value, if it was returned by the previous operation, to get the next set of rules.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirNextToken :: Lens.Lens' DescribeInsightRules (Lude.Maybe Lude.Text)
dirNextToken = Lens.lens (nextToken :: DescribeInsightRules -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInsightRules)
{-# DEPRECATED dirNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return in one operation. If you omit this parameter, the default of 500 is used.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirMaxResults :: Lens.Lens' DescribeInsightRules (Lude.Maybe Lude.Natural)
dirMaxResults = Lens.lens (maxResults :: DescribeInsightRules -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeInsightRules)
{-# DEPRECATED dirMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest DescribeInsightRules where
  type Rs DescribeInsightRules = DescribeInsightRulesResponse
  request = Req.postQuery cloudWatchService
  response =
    Res.receiveXMLWrapper
      "DescribeInsightRulesResult"
      ( \s h x ->
          DescribeInsightRulesResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> ( x Lude..@? "InsightRules" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInsightRules where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeInsightRules where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeInsightRules where
  toQuery DescribeInsightRules' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeInsightRules" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeInsightRulesResponse' smart constructor.
data DescribeInsightRulesResponse = DescribeInsightRulesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    insightRules ::
      Lude.Maybe [InsightRule],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInsightRulesResponse' with the minimum fields required to make a request.
--
-- * 'insightRules' - The rules returned by the operation.
-- * 'nextToken' - If this parameter is present, it is a token that marks the start of the next batch of returned results.
-- * 'responseStatus' - The response status code.
mkDescribeInsightRulesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInsightRulesResponse
mkDescribeInsightRulesResponse pResponseStatus_ =
  DescribeInsightRulesResponse'
    { nextToken = Lude.Nothing,
      insightRules = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If this parameter is present, it is a token that marks the start of the next batch of returned results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsNextToken :: Lens.Lens' DescribeInsightRulesResponse (Lude.Maybe Lude.Text)
drsNextToken = Lens.lens (nextToken :: DescribeInsightRulesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInsightRulesResponse)
{-# DEPRECATED drsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The rules returned by the operation.
--
-- /Note:/ Consider using 'insightRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsInsightRules :: Lens.Lens' DescribeInsightRulesResponse (Lude.Maybe [InsightRule])
drsInsightRules = Lens.lens (insightRules :: DescribeInsightRulesResponse -> Lude.Maybe [InsightRule]) (\s a -> s {insightRules = a} :: DescribeInsightRulesResponse)
{-# DEPRECATED drsInsightRules "Use generic-lens or generic-optics with 'insightRules' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeInsightRulesResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeInsightRulesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInsightRulesResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
