{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListApprovedOrigins
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all approved origins associated with the instance.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListApprovedOrigins
  ( -- * Creating a request
    ListApprovedOrigins (..),
    mkListApprovedOrigins,

    -- ** Request lenses
    laoNextToken,
    laoMaxResults,
    laoInstanceId,

    -- * Destructuring the response
    ListApprovedOriginsResponse (..),
    mkListApprovedOriginsResponse,

    -- ** Response lenses
    laorsNextToken,
    laorsOrigins,
    laorsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListApprovedOrigins' smart constructor.
data ListApprovedOrigins = ListApprovedOrigins'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    instanceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListApprovedOrigins' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'maxResults' - The maximimum number of results to return per page.
-- * 'nextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
mkListApprovedOrigins ::
  -- | 'instanceId'
  Lude.Text ->
  ListApprovedOrigins
mkListApprovedOrigins pInstanceId_ =
  ListApprovedOrigins'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laoNextToken :: Lens.Lens' ListApprovedOrigins (Lude.Maybe Lude.Text)
laoNextToken = Lens.lens (nextToken :: ListApprovedOrigins -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListApprovedOrigins)
{-# DEPRECATED laoNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laoMaxResults :: Lens.Lens' ListApprovedOrigins (Lude.Maybe Lude.Natural)
laoMaxResults = Lens.lens (maxResults :: ListApprovedOrigins -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListApprovedOrigins)
{-# DEPRECATED laoMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laoInstanceId :: Lens.Lens' ListApprovedOrigins Lude.Text
laoInstanceId = Lens.lens (instanceId :: ListApprovedOrigins -> Lude.Text) (\s a -> s {instanceId = a} :: ListApprovedOrigins)
{-# DEPRECATED laoInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Page.AWSPager ListApprovedOrigins where
  page rq rs
    | Page.stop (rs Lens.^. laorsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. laorsOrigins) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& laoNextToken Lens..~ rs Lens.^. laorsNextToken

instance Lude.AWSRequest ListApprovedOrigins where
  type Rs ListApprovedOrigins = ListApprovedOriginsResponse
  request = Req.get connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListApprovedOriginsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Origins" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListApprovedOrigins where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListApprovedOrigins where
  toPath ListApprovedOrigins' {..} =
    Lude.mconcat
      ["/instance/", Lude.toBS instanceId, "/approved-origins"]

instance Lude.ToQuery ListApprovedOrigins where
  toQuery ListApprovedOrigins' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListApprovedOriginsResponse' smart constructor.
data ListApprovedOriginsResponse = ListApprovedOriginsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    origins :: Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'ListApprovedOriginsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If there are additional results, this is the token for the next set of results.
-- * 'origins' - The approved origins.
-- * 'responseStatus' - The response status code.
mkListApprovedOriginsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListApprovedOriginsResponse
mkListApprovedOriginsResponse pResponseStatus_ =
  ListApprovedOriginsResponse'
    { nextToken = Lude.Nothing,
      origins = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laorsNextToken :: Lens.Lens' ListApprovedOriginsResponse (Lude.Maybe Lude.Text)
laorsNextToken = Lens.lens (nextToken :: ListApprovedOriginsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListApprovedOriginsResponse)
{-# DEPRECATED laorsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The approved origins.
--
-- /Note:/ Consider using 'origins' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laorsOrigins :: Lens.Lens' ListApprovedOriginsResponse (Lude.Maybe [Lude.Text])
laorsOrigins = Lens.lens (origins :: ListApprovedOriginsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {origins = a} :: ListApprovedOriginsResponse)
{-# DEPRECATED laorsOrigins "Use generic-lens or generic-optics with 'origins' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laorsResponseStatus :: Lens.Lens' ListApprovedOriginsResponse Lude.Int
laorsResponseStatus = Lens.lens (responseStatus :: ListApprovedOriginsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListApprovedOriginsResponse)
{-# DEPRECATED laorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
