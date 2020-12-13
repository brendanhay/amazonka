{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.ListResolversByFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the resolvers that are associated with a specific function.
--
-- This operation returns paginated results.
module Network.AWS.AppSync.ListResolversByFunction
  ( -- * Creating a request
    ListResolversByFunction (..),
    mkListResolversByFunction,

    -- ** Request lenses
    lrbfApiId,
    lrbfNextToken,
    lrbfFunctionId,
    lrbfMaxResults,

    -- * Destructuring the response
    ListResolversByFunctionResponse (..),
    mkListResolversByFunctionResponse,

    -- ** Response lenses
    lrbfrsNextToken,
    lrbfrsResolvers,
    lrbfrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListResolversByFunction' smart constructor.
data ListResolversByFunction = ListResolversByFunction'
  { -- | The API ID.
    apiId :: Lude.Text,
    -- | An identifier that was returned from the previous call to this operation, which you can use to return the next set of items in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The Function ID.
    functionId :: Lude.Text,
    -- | The maximum number of results you want the request to return.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListResolversByFunction' with the minimum fields required to make a request.
--
-- * 'apiId' - The API ID.
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which you can use to return the next set of items in the list.
-- * 'functionId' - The Function ID.
-- * 'maxResults' - The maximum number of results you want the request to return.
mkListResolversByFunction ::
  -- | 'apiId'
  Lude.Text ->
  -- | 'functionId'
  Lude.Text ->
  ListResolversByFunction
mkListResolversByFunction pApiId_ pFunctionId_ =
  ListResolversByFunction'
    { apiId = pApiId_,
      nextToken = Lude.Nothing,
      functionId = pFunctionId_,
      maxResults = Lude.Nothing
    }

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbfApiId :: Lens.Lens' ListResolversByFunction Lude.Text
lrbfApiId = Lens.lens (apiId :: ListResolversByFunction -> Lude.Text) (\s a -> s {apiId = a} :: ListResolversByFunction)
{-# DEPRECATED lrbfApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which you can use to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbfNextToken :: Lens.Lens' ListResolversByFunction (Lude.Maybe Lude.Text)
lrbfNextToken = Lens.lens (nextToken :: ListResolversByFunction -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListResolversByFunction)
{-# DEPRECATED lrbfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The Function ID.
--
-- /Note:/ Consider using 'functionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbfFunctionId :: Lens.Lens' ListResolversByFunction Lude.Text
lrbfFunctionId = Lens.lens (functionId :: ListResolversByFunction -> Lude.Text) (\s a -> s {functionId = a} :: ListResolversByFunction)
{-# DEPRECATED lrbfFunctionId "Use generic-lens or generic-optics with 'functionId' instead." #-}

-- | The maximum number of results you want the request to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbfMaxResults :: Lens.Lens' ListResolversByFunction (Lude.Maybe Lude.Natural)
lrbfMaxResults = Lens.lens (maxResults :: ListResolversByFunction -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListResolversByFunction)
{-# DEPRECATED lrbfMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListResolversByFunction where
  page rq rs
    | Page.stop (rs Lens.^. lrbfrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrbfrsResolvers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrbfNextToken Lens..~ rs Lens.^. lrbfrsNextToken

instance Lude.AWSRequest ListResolversByFunction where
  type Rs ListResolversByFunction = ListResolversByFunctionResponse
  request = Req.get appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListResolversByFunctionResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "resolvers" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListResolversByFunction where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListResolversByFunction where
  toPath ListResolversByFunction' {..} =
    Lude.mconcat
      [ "/v1/apis/",
        Lude.toBS apiId,
        "/functions/",
        Lude.toBS functionId,
        "/resolvers"
      ]

instance Lude.ToQuery ListResolversByFunction where
  toQuery ListResolversByFunction' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListResolversByFunctionResponse' smart constructor.
data ListResolversByFunctionResponse = ListResolversByFunctionResponse'
  { -- | An identifier that can be used to return the next set of items in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The list of resolvers.
    resolvers :: Lude.Maybe [Resolver],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListResolversByFunctionResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - An identifier that can be used to return the next set of items in the list.
-- * 'resolvers' - The list of resolvers.
-- * 'responseStatus' - The response status code.
mkListResolversByFunctionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListResolversByFunctionResponse
mkListResolversByFunctionResponse pResponseStatus_ =
  ListResolversByFunctionResponse'
    { nextToken = Lude.Nothing,
      resolvers = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An identifier that can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbfrsNextToken :: Lens.Lens' ListResolversByFunctionResponse (Lude.Maybe Lude.Text)
lrbfrsNextToken = Lens.lens (nextToken :: ListResolversByFunctionResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListResolversByFunctionResponse)
{-# DEPRECATED lrbfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of resolvers.
--
-- /Note:/ Consider using 'resolvers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbfrsResolvers :: Lens.Lens' ListResolversByFunctionResponse (Lude.Maybe [Resolver])
lrbfrsResolvers = Lens.lens (resolvers :: ListResolversByFunctionResponse -> Lude.Maybe [Resolver]) (\s a -> s {resolvers = a} :: ListResolversByFunctionResponse)
{-# DEPRECATED lrbfrsResolvers "Use generic-lens or generic-optics with 'resolvers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbfrsResponseStatus :: Lens.Lens' ListResolversByFunctionResponse Lude.Int
lrbfrsResponseStatus = Lens.lens (responseStatus :: ListResolversByFunctionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListResolversByFunctionResponse)
{-# DEPRECATED lrbfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
