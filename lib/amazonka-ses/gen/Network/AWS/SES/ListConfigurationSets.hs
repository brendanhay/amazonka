{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.ListConfigurationSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of the configuration sets associated with your Amazon SES account in the current AWS Region. For information about using configuration sets, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Monitoring Your Amazon SES Sending Activity> in the /Amazon SES Developer Guide./
--
-- You can execute this operation no more than once per second. This operation will return up to 1,000 configuration sets each time it is run. If your Amazon SES account has more than 1,000 configuration sets, this operation will also return a NextToken element. You can then execute the @ListConfigurationSets@ operation again, passing the @NextToken@ parameter and the value of the NextToken element to retrieve additional results.
--
-- This operation returns paginated results.
module Network.AWS.SES.ListConfigurationSets
  ( -- * Creating a request
    ListConfigurationSets (..),
    mkListConfigurationSets,

    -- ** Request lenses
    lcsNextToken,
    lcsMaxItems,

    -- * Destructuring the response
    ListConfigurationSetsResponse (..),
    mkListConfigurationSetsResponse,

    -- ** Response lenses
    lcsrsConfigurationSets,
    lcsrsNextToken,
    lcsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to list the configuration sets associated with your AWS account. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkListConfigurationSets' smart constructor.
data ListConfigurationSets = ListConfigurationSets'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxItems :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListConfigurationSets' with the minimum fields required to make a request.
--
-- * 'maxItems' - The number of configuration sets to return.
-- * 'nextToken' - A token returned from a previous call to @ListConfigurationSets@ to indicate the position of the configuration set in the configuration set list.
mkListConfigurationSets ::
  ListConfigurationSets
mkListConfigurationSets =
  ListConfigurationSets'
    { nextToken = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | A token returned from a previous call to @ListConfigurationSets@ to indicate the position of the configuration set in the configuration set list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsNextToken :: Lens.Lens' ListConfigurationSets (Lude.Maybe Lude.Text)
lcsNextToken = Lens.lens (nextToken :: ListConfigurationSets -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListConfigurationSets)
{-# DEPRECATED lcsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The number of configuration sets to return.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsMaxItems :: Lens.Lens' ListConfigurationSets (Lude.Maybe Lude.Int)
lcsMaxItems = Lens.lens (maxItems :: ListConfigurationSets -> Lude.Maybe Lude.Int) (\s a -> s {maxItems = a} :: ListConfigurationSets)
{-# DEPRECATED lcsMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListConfigurationSets where
  page rq rs
    | Page.stop (rs Lens.^. lcsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcsrsConfigurationSets) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcsNextToken Lens..~ rs Lens.^. lcsrsNextToken

instance Lude.AWSRequest ListConfigurationSets where
  type Rs ListConfigurationSets = ListConfigurationSetsResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "ListConfigurationSetsResult"
      ( \s h x ->
          ListConfigurationSetsResponse'
            Lude.<$> ( x Lude..@? "ConfigurationSets" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListConfigurationSets where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListConfigurationSets where
  toPath = Lude.const "/"

instance Lude.ToQuery ListConfigurationSets where
  toQuery ListConfigurationSets' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListConfigurationSets" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "MaxItems" Lude.=: maxItems
      ]

-- | A list of configuration sets associated with your AWS account. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkListConfigurationSetsResponse' smart constructor.
data ListConfigurationSetsResponse = ListConfigurationSetsResponse'
  { configurationSets ::
      Lude.Maybe [ConfigurationSet],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListConfigurationSetsResponse' with the minimum fields required to make a request.
--
-- * 'configurationSets' - A list of configuration sets.
-- * 'nextToken' - A token indicating that there are additional configuration sets available to be listed. Pass this token to successive calls of @ListConfigurationSets@ .
-- * 'responseStatus' - The response status code.
mkListConfigurationSetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListConfigurationSetsResponse
mkListConfigurationSetsResponse pResponseStatus_ =
  ListConfigurationSetsResponse'
    { configurationSets = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of configuration sets.
--
-- /Note:/ Consider using 'configurationSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsrsConfigurationSets :: Lens.Lens' ListConfigurationSetsResponse (Lude.Maybe [ConfigurationSet])
lcsrsConfigurationSets = Lens.lens (configurationSets :: ListConfigurationSetsResponse -> Lude.Maybe [ConfigurationSet]) (\s a -> s {configurationSets = a} :: ListConfigurationSetsResponse)
{-# DEPRECATED lcsrsConfigurationSets "Use generic-lens or generic-optics with 'configurationSets' instead." #-}

-- | A token indicating that there are additional configuration sets available to be listed. Pass this token to successive calls of @ListConfigurationSets@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsrsNextToken :: Lens.Lens' ListConfigurationSetsResponse (Lude.Maybe Lude.Text)
lcsrsNextToken = Lens.lens (nextToken :: ListConfigurationSetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListConfigurationSetsResponse)
{-# DEPRECATED lcsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsrsResponseStatus :: Lens.Lens' ListConfigurationSetsResponse Lude.Int
lcsrsResponseStatus = Lens.lens (responseStatus :: ListConfigurationSetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListConfigurationSetsResponse)
{-# DEPRECATED lcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
