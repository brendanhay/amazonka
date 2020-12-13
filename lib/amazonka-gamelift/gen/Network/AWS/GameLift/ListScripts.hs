{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.ListScripts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves script records for all Realtime scripts that are associated with the AWS account in use.
--
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/realtime-intro.html Amazon GameLift Realtime Servers>
-- __Related operations__
--
--     * 'CreateScript'
--
--
--     * 'ListScripts'
--
--
--     * 'DescribeScript'
--
--
--     * 'UpdateScript'
--
--
--     * 'DeleteScript'
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.ListScripts
  ( -- * Creating a request
    ListScripts (..),
    mkListScripts,

    -- ** Request lenses
    lsNextToken,
    lsLimit,

    -- * Destructuring the response
    ListScriptsResponse (..),
    mkListScriptsResponse,

    -- ** Response lenses
    lsrsScripts,
    lsrsNextToken,
    lsrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListScripts' smart constructor.
data ListScripts = ListScripts'
  { -- | A token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListScripts' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
-- * 'limit' - The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
mkListScripts ::
  ListScripts
mkListScripts =
  ListScripts' {nextToken = Lude.Nothing, limit = Lude.Nothing}

-- | A token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsNextToken :: Lens.Lens' ListScripts (Lude.Maybe Lude.Text)
lsNextToken = Lens.lens (nextToken :: ListScripts -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListScripts)
{-# DEPRECATED lsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsLimit :: Lens.Lens' ListScripts (Lude.Maybe Lude.Natural)
lsLimit = Lens.lens (limit :: ListScripts -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListScripts)
{-# DEPRECATED lsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager ListScripts where
  page rq rs
    | Page.stop (rs Lens.^. lsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsrsScripts) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsNextToken Lens..~ rs Lens.^. lsrsNextToken

instance Lude.AWSRequest ListScripts where
  type Rs ListScripts = ListScriptsResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListScriptsResponse'
            Lude.<$> (x Lude..?> "Scripts" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListScripts where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.ListScripts" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListScripts where
  toJSON ListScripts' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListScripts where
  toPath = Lude.const "/"

instance Lude.ToQuery ListScripts where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListScriptsResponse' smart constructor.
data ListScriptsResponse = ListScriptsResponse'
  { -- | A set of properties describing the requested script.
    scripts :: Lude.Maybe [Script],
    -- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListScriptsResponse' with the minimum fields required to make a request.
--
-- * 'scripts' - A set of properties describing the requested script.
-- * 'nextToken' - A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
-- * 'responseStatus' - The response status code.
mkListScriptsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListScriptsResponse
mkListScriptsResponse pResponseStatus_ =
  ListScriptsResponse'
    { scripts = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A set of properties describing the requested script.
--
-- /Note:/ Consider using 'scripts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsScripts :: Lens.Lens' ListScriptsResponse (Lude.Maybe [Script])
lsrsScripts = Lens.lens (scripts :: ListScriptsResponse -> Lude.Maybe [Script]) (\s a -> s {scripts = a} :: ListScriptsResponse)
{-# DEPRECATED lsrsScripts "Use generic-lens or generic-optics with 'scripts' instead." #-}

-- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsNextToken :: Lens.Lens' ListScriptsResponse (Lude.Maybe Lude.Text)
lsrsNextToken = Lens.lens (nextToken :: ListScriptsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListScriptsResponse)
{-# DEPRECATED lsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsResponseStatus :: Lens.Lens' ListScriptsResponse Lude.Int
lsrsResponseStatus = Lens.lens (responseStatus :: ListScriptsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListScriptsResponse)
{-# DEPRECATED lsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
