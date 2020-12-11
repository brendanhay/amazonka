{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DescribeRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified rules or the rules for the specified listener. You must specify either a listener or one or more rules.
--
-- This operation returns paginated results.
module Network.AWS.ELBv2.DescribeRules
  ( -- * Creating a request
    DescribeRules (..),
    mkDescribeRules,

    -- ** Request lenses
    drListenerARN,
    drMarker,
    drRuleARNs,
    drPageSize,

    -- * Destructuring the response
    DescribeRulesResponse (..),
    mkDescribeRulesResponse,

    -- ** Response lenses
    drsrsRules,
    drsrsNextMarker,
    drsrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeRules' smart constructor.
data DescribeRules = DescribeRules'
  { listenerARN ::
      Lude.Maybe Lude.Text,
    marker :: Lude.Maybe Lude.Text,
    ruleARNs :: Lude.Maybe [Lude.Text],
    pageSize :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRules' with the minimum fields required to make a request.
--
-- * 'listenerARN' - The Amazon Resource Name (ARN) of the listener.
-- * 'marker' - The marker for the next set of results. (You received this marker from a previous call.)
-- * 'pageSize' - The maximum number of results to return with this call.
-- * 'ruleARNs' - The Amazon Resource Names (ARN) of the rules.
mkDescribeRules ::
  DescribeRules
mkDescribeRules =
  DescribeRules'
    { listenerARN = Lude.Nothing,
      marker = Lude.Nothing,
      ruleARNs = Lude.Nothing,
      pageSize = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the listener.
--
-- /Note:/ Consider using 'listenerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drListenerARN :: Lens.Lens' DescribeRules (Lude.Maybe Lude.Text)
drListenerARN = Lens.lens (listenerARN :: DescribeRules -> Lude.Maybe Lude.Text) (\s a -> s {listenerARN = a} :: DescribeRules)
{-# DEPRECATED drListenerARN "Use generic-lens or generic-optics with 'listenerARN' instead." #-}

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drMarker :: Lens.Lens' DescribeRules (Lude.Maybe Lude.Text)
drMarker = Lens.lens (marker :: DescribeRules -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeRules)
{-# DEPRECATED drMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The Amazon Resource Names (ARN) of the rules.
--
-- /Note:/ Consider using 'ruleARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drRuleARNs :: Lens.Lens' DescribeRules (Lude.Maybe [Lude.Text])
drRuleARNs = Lens.lens (ruleARNs :: DescribeRules -> Lude.Maybe [Lude.Text]) (\s a -> s {ruleARNs = a} :: DescribeRules)
{-# DEPRECATED drRuleARNs "Use generic-lens or generic-optics with 'ruleARNs' instead." #-}

-- | The maximum number of results to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drPageSize :: Lens.Lens' DescribeRules (Lude.Maybe Lude.Natural)
drPageSize = Lens.lens (pageSize :: DescribeRules -> Lude.Maybe Lude.Natural) (\s a -> s {pageSize = a} :: DescribeRules)
{-# DEPRECATED drPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Page.AWSPager DescribeRules where
  page rq rs
    | Page.stop (rs Lens.^. drsrsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. drsrsRules) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& drMarker Lens..~ rs Lens.^. drsrsNextMarker

instance Lude.AWSRequest DescribeRules where
  type Rs DescribeRules = DescribeRulesResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "DescribeRulesResult"
      ( \s h x ->
          DescribeRulesResponse'
            Lude.<$> ( x Lude..@? "Rules" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeRules where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeRules where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeRules where
  toQuery DescribeRules' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeRules" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "ListenerArn" Lude.=: listenerARN,
        "Marker" Lude.=: marker,
        "RuleArns"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> ruleARNs),
        "PageSize" Lude.=: pageSize
      ]

-- | /See:/ 'mkDescribeRulesResponse' smart constructor.
data DescribeRulesResponse = DescribeRulesResponse'
  { rules ::
      Lude.Maybe [Rule],
    nextMarker :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeRulesResponse' with the minimum fields required to make a request.
--
-- * 'nextMarker' - If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
-- * 'responseStatus' - The response status code.
-- * 'rules' - Information about the rules.
mkDescribeRulesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeRulesResponse
mkDescribeRulesResponse pResponseStatus_ =
  DescribeRulesResponse'
    { rules = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the rules.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsrsRules :: Lens.Lens' DescribeRulesResponse (Lude.Maybe [Rule])
drsrsRules = Lens.lens (rules :: DescribeRulesResponse -> Lude.Maybe [Rule]) (\s a -> s {rules = a} :: DescribeRulesResponse)
{-# DEPRECATED drsrsRules "Use generic-lens or generic-optics with 'rules' instead." #-}

-- | If there are additional results, this is the marker for the next set of results. Otherwise, this is null.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsrsNextMarker :: Lens.Lens' DescribeRulesResponse (Lude.Maybe Lude.Text)
drsrsNextMarker = Lens.lens (nextMarker :: DescribeRulesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: DescribeRulesResponse)
{-# DEPRECATED drsrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsrsResponseStatus :: Lens.Lens' DescribeRulesResponse Lude.Int
drsrsResponseStatus = Lens.lens (responseStatus :: DescribeRulesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeRulesResponse)
{-# DEPRECATED drsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
