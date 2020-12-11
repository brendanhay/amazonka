{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.ListGraphqlAPIs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your GraphQL APIs.
--
-- This operation returns paginated results.
module Network.AWS.AppSync.ListGraphqlAPIs
  ( -- * Creating a request
    ListGraphqlAPIs (..),
    mkListGraphqlAPIs,

    -- ** Request lenses
    lgaNextToken,
    lgaMaxResults,

    -- * Destructuring the response
    ListGraphqlAPIsResponse (..),
    mkListGraphqlAPIsResponse,

    -- ** Response lenses
    lgarsNextToken,
    lgarsGraphqlAPIs,
    lgarsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListGraphqlAPIs' smart constructor.
data ListGraphqlAPIs = ListGraphqlAPIs'
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

-- | Creates a value of 'ListGraphqlAPIs' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results you want the request to return.
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
mkListGraphqlAPIs ::
  ListGraphqlAPIs
mkListGraphqlAPIs =
  ListGraphqlAPIs'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgaNextToken :: Lens.Lens' ListGraphqlAPIs (Lude.Maybe Lude.Text)
lgaNextToken = Lens.lens (nextToken :: ListGraphqlAPIs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListGraphqlAPIs)
{-# DEPRECATED lgaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results you want the request to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgaMaxResults :: Lens.Lens' ListGraphqlAPIs (Lude.Maybe Lude.Natural)
lgaMaxResults = Lens.lens (maxResults :: ListGraphqlAPIs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListGraphqlAPIs)
{-# DEPRECATED lgaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListGraphqlAPIs where
  page rq rs
    | Page.stop (rs Lens.^. lgarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lgarsGraphqlAPIs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lgaNextToken Lens..~ rs Lens.^. lgarsNextToken

instance Lude.AWSRequest ListGraphqlAPIs where
  type Rs ListGraphqlAPIs = ListGraphqlAPIsResponse
  request = Req.get appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListGraphqlAPIsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "graphqlApis" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListGraphqlAPIs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListGraphqlAPIs where
  toPath = Lude.const "/v1/apis"

instance Lude.ToQuery ListGraphqlAPIs where
  toQuery ListGraphqlAPIs' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListGraphqlAPIsResponse' smart constructor.
data ListGraphqlAPIsResponse = ListGraphqlAPIsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    graphqlAPIs :: Lude.Maybe [GraphqlAPI],
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

-- | Creates a value of 'ListGraphqlAPIsResponse' with the minimum fields required to make a request.
--
-- * 'graphqlAPIs' - The @GraphqlApi@ objects.
-- * 'nextToken' - An identifier to be passed in the next request to this operation to return the next set of items in the list.
-- * 'responseStatus' - The response status code.
mkListGraphqlAPIsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListGraphqlAPIsResponse
mkListGraphqlAPIsResponse pResponseStatus_ =
  ListGraphqlAPIsResponse'
    { nextToken = Lude.Nothing,
      graphqlAPIs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An identifier to be passed in the next request to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgarsNextToken :: Lens.Lens' ListGraphqlAPIsResponse (Lude.Maybe Lude.Text)
lgarsNextToken = Lens.lens (nextToken :: ListGraphqlAPIsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListGraphqlAPIsResponse)
{-# DEPRECATED lgarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The @GraphqlApi@ objects.
--
-- /Note:/ Consider using 'graphqlAPIs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgarsGraphqlAPIs :: Lens.Lens' ListGraphqlAPIsResponse (Lude.Maybe [GraphqlAPI])
lgarsGraphqlAPIs = Lens.lens (graphqlAPIs :: ListGraphqlAPIsResponse -> Lude.Maybe [GraphqlAPI]) (\s a -> s {graphqlAPIs = a} :: ListGraphqlAPIsResponse)
{-# DEPRECATED lgarsGraphqlAPIs "Use generic-lens or generic-optics with 'graphqlAPIs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgarsResponseStatus :: Lens.Lens' ListGraphqlAPIsResponse Lude.Int
lgarsResponseStatus = Lens.lens (responseStatus :: ListGraphqlAPIsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListGraphqlAPIsResponse)
{-# DEPRECATED lgarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
