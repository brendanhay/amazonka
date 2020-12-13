{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListLexBots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all the Amazon Lex bots currently associated with the instance.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListLexBots
  ( -- * Creating a request
    ListLexBots (..),
    mkListLexBots,

    -- ** Request lenses
    llbInstanceId,
    llbNextToken,
    llbMaxResults,

    -- * Destructuring the response
    ListLexBotsResponse (..),
    mkListLexBotsResponse,

    -- ** Response lenses
    llbrsNextToken,
    llbrsLexBots,
    llbrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListLexBots' smart constructor.
data ListLexBots = ListLexBots'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximimum number of results to return per page.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListLexBots' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'nextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
-- * 'maxResults' - The maximimum number of results to return per page.
mkListLexBots ::
  -- | 'instanceId'
  Lude.Text ->
  ListLexBots
mkListLexBots pInstanceId_ =
  ListLexBots'
    { instanceId = pInstanceId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llbInstanceId :: Lens.Lens' ListLexBots Lude.Text
llbInstanceId = Lens.lens (instanceId :: ListLexBots -> Lude.Text) (\s a -> s {instanceId = a} :: ListLexBots)
{-# DEPRECATED llbInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llbNextToken :: Lens.Lens' ListLexBots (Lude.Maybe Lude.Text)
llbNextToken = Lens.lens (nextToken :: ListLexBots -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListLexBots)
{-# DEPRECATED llbNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llbMaxResults :: Lens.Lens' ListLexBots (Lude.Maybe Lude.Natural)
llbMaxResults = Lens.lens (maxResults :: ListLexBots -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListLexBots)
{-# DEPRECATED llbMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListLexBots where
  page rq rs
    | Page.stop (rs Lens.^. llbrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. llbrsLexBots) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& llbNextToken Lens..~ rs Lens.^. llbrsNextToken

instance Lude.AWSRequest ListLexBots where
  type Rs ListLexBots = ListLexBotsResponse
  request = Req.get connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListLexBotsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "LexBots" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListLexBots where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListLexBots where
  toPath ListLexBots' {..} =
    Lude.mconcat ["/instance/", Lude.toBS instanceId, "/lex-bots"]

instance Lude.ToQuery ListLexBots where
  toQuery ListLexBots' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListLexBotsResponse' smart constructor.
data ListLexBotsResponse = ListLexBotsResponse'
  { -- | If there are additional results, this is the token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The the names and regions of the Amazon Lex bots associated with the specified instance.
    lexBots :: Lude.Maybe [LexBot],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListLexBotsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If there are additional results, this is the token for the next set of results.
-- * 'lexBots' - The the names and regions of the Amazon Lex bots associated with the specified instance.
-- * 'responseStatus' - The response status code.
mkListLexBotsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListLexBotsResponse
mkListLexBotsResponse pResponseStatus_ =
  ListLexBotsResponse'
    { nextToken = Lude.Nothing,
      lexBots = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llbrsNextToken :: Lens.Lens' ListLexBotsResponse (Lude.Maybe Lude.Text)
llbrsNextToken = Lens.lens (nextToken :: ListLexBotsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListLexBotsResponse)
{-# DEPRECATED llbrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The the names and regions of the Amazon Lex bots associated with the specified instance.
--
-- /Note:/ Consider using 'lexBots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llbrsLexBots :: Lens.Lens' ListLexBotsResponse (Lude.Maybe [LexBot])
llbrsLexBots = Lens.lens (lexBots :: ListLexBotsResponse -> Lude.Maybe [LexBot]) (\s a -> s {lexBots = a} :: ListLexBotsResponse)
{-# DEPRECATED llbrsLexBots "Use generic-lens or generic-optics with 'lexBots' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llbrsResponseStatus :: Lens.Lens' ListLexBotsResponse Lude.Int
llbrsResponseStatus = Lens.lens (responseStatus :: ListLexBotsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListLexBotsResponse)
{-# DEPRECATED llbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
