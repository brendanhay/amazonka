{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListV2LoggingLevels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists logging levels.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListV2LoggingLevels
  ( -- * Creating a request
    ListV2LoggingLevels (..),
    mkListV2LoggingLevels,

    -- ** Request lenses
    lvllTargetType,
    lvllNextToken,
    lvllMaxResults,

    -- * Destructuring the response
    ListV2LoggingLevelsResponse (..),
    mkListV2LoggingLevelsResponse,

    -- ** Response lenses
    lvllrsLogTargetConfigurations,
    lvllrsNextToken,
    lvllrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListV2LoggingLevels' smart constructor.
data ListV2LoggingLevels = ListV2LoggingLevels'
  { -- | The type of resource for which you are configuring logging. Must be @THING_Group@ .
    targetType :: Lude.Maybe LogTargetType,
    -- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListV2LoggingLevels' with the minimum fields required to make a request.
--
-- * 'targetType' - The type of resource for which you are configuring logging. Must be @THING_Group@ .
-- * 'nextToken' - To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
-- * 'maxResults' - The maximum number of results to return at one time.
mkListV2LoggingLevels ::
  ListV2LoggingLevels
mkListV2LoggingLevels =
  ListV2LoggingLevels'
    { targetType = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The type of resource for which you are configuring logging. Must be @THING_Group@ .
--
-- /Note:/ Consider using 'targetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvllTargetType :: Lens.Lens' ListV2LoggingLevels (Lude.Maybe LogTargetType)
lvllTargetType = Lens.lens (targetType :: ListV2LoggingLevels -> Lude.Maybe LogTargetType) (\s a -> s {targetType = a} :: ListV2LoggingLevels)
{-# DEPRECATED lvllTargetType "Use generic-lens or generic-optics with 'targetType' instead." #-}

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvllNextToken :: Lens.Lens' ListV2LoggingLevels (Lude.Maybe Lude.Text)
lvllNextToken = Lens.lens (nextToken :: ListV2LoggingLevels -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListV2LoggingLevels)
{-# DEPRECATED lvllNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvllMaxResults :: Lens.Lens' ListV2LoggingLevels (Lude.Maybe Lude.Natural)
lvllMaxResults = Lens.lens (maxResults :: ListV2LoggingLevels -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListV2LoggingLevels)
{-# DEPRECATED lvllMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListV2LoggingLevels where
  page rq rs
    | Page.stop (rs Lens.^. lvllrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lvllrsLogTargetConfigurations) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lvllNextToken Lens..~ rs Lens.^. lvllrsNextToken

instance Lude.AWSRequest ListV2LoggingLevels where
  type Rs ListV2LoggingLevels = ListV2LoggingLevelsResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListV2LoggingLevelsResponse'
            Lude.<$> (x Lude..?> "logTargetConfigurations" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListV2LoggingLevels where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListV2LoggingLevels where
  toPath = Lude.const "/v2LoggingLevel"

instance Lude.ToQuery ListV2LoggingLevels where
  toQuery ListV2LoggingLevels' {..} =
    Lude.mconcat
      [ "targetType" Lude.=: targetType,
        "nextToken" Lude.=: nextToken,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListV2LoggingLevelsResponse' smart constructor.
data ListV2LoggingLevelsResponse = ListV2LoggingLevelsResponse'
  { -- | The logging configuration for a target.
    logTargetConfigurations :: Lude.Maybe [LogTargetConfiguration],
    -- | The token to use to get the next set of results, or __null__ if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListV2LoggingLevelsResponse' with the minimum fields required to make a request.
--
-- * 'logTargetConfigurations' - The logging configuration for a target.
-- * 'nextToken' - The token to use to get the next set of results, or __null__ if there are no additional results.
-- * 'responseStatus' - The response status code.
mkListV2LoggingLevelsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListV2LoggingLevelsResponse
mkListV2LoggingLevelsResponse pResponseStatus_ =
  ListV2LoggingLevelsResponse'
    { logTargetConfigurations =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The logging configuration for a target.
--
-- /Note:/ Consider using 'logTargetConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvllrsLogTargetConfigurations :: Lens.Lens' ListV2LoggingLevelsResponse (Lude.Maybe [LogTargetConfiguration])
lvllrsLogTargetConfigurations = Lens.lens (logTargetConfigurations :: ListV2LoggingLevelsResponse -> Lude.Maybe [LogTargetConfiguration]) (\s a -> s {logTargetConfigurations = a} :: ListV2LoggingLevelsResponse)
{-# DEPRECATED lvllrsLogTargetConfigurations "Use generic-lens or generic-optics with 'logTargetConfigurations' instead." #-}

-- | The token to use to get the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvllrsNextToken :: Lens.Lens' ListV2LoggingLevelsResponse (Lude.Maybe Lude.Text)
lvllrsNextToken = Lens.lens (nextToken :: ListV2LoggingLevelsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListV2LoggingLevelsResponse)
{-# DEPRECATED lvllrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvllrsResponseStatus :: Lens.Lens' ListV2LoggingLevelsResponse Lude.Int
lvllrsResponseStatus = Lens.lens (responseStatus :: ListV2LoggingLevelsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListV2LoggingLevelsResponse)
{-# DEPRECATED lvllrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
