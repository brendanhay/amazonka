{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.ListResolvers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resolvers for a given API and type.
--
-- This operation returns paginated results.
module Network.AWS.AppSync.ListResolvers
  ( -- * Creating a request
    ListResolvers (..),
    mkListResolvers,

    -- ** Request lenses
    lrNextToken,
    lrMaxResults,
    lrApiId,
    lrTypeName,

    -- * Destructuring the response
    ListResolversResponse (..),
    mkListResolversResponse,

    -- ** Response lenses
    lrrsNextToken,
    lrrsResolvers,
    lrrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListResolvers' smart constructor.
data ListResolvers = ListResolvers'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    apiId :: Lude.Text,
    typeName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListResolvers' with the minimum fields required to make a request.
--
-- * 'apiId' - The API ID.
-- * 'maxResults' - The maximum number of results you want the request to return.
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
-- * 'typeName' - The type name.
mkListResolvers ::
  -- | 'apiId'
  Lude.Text ->
  -- | 'typeName'
  Lude.Text ->
  ListResolvers
mkListResolvers pApiId_ pTypeName_ =
  ListResolvers'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      apiId = pApiId_,
      typeName = pTypeName_
    }

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNextToken :: Lens.Lens' ListResolvers (Lude.Maybe Lude.Text)
lrNextToken = Lens.lens (nextToken :: ListResolvers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListResolvers)
{-# DEPRECATED lrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results you want the request to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrMaxResults :: Lens.Lens' ListResolvers (Lude.Maybe Lude.Natural)
lrMaxResults = Lens.lens (maxResults :: ListResolvers -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListResolvers)
{-# DEPRECATED lrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrApiId :: Lens.Lens' ListResolvers Lude.Text
lrApiId = Lens.lens (apiId :: ListResolvers -> Lude.Text) (\s a -> s {apiId = a} :: ListResolvers)
{-# DEPRECATED lrApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | The type name.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrTypeName :: Lens.Lens' ListResolvers Lude.Text
lrTypeName = Lens.lens (typeName :: ListResolvers -> Lude.Text) (\s a -> s {typeName = a} :: ListResolvers)
{-# DEPRECATED lrTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

instance Page.AWSPager ListResolvers where
  page rq rs
    | Page.stop (rs Lens.^. lrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrrsResolvers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrNextToken Lens..~ rs Lens.^. lrrsNextToken

instance Lude.AWSRequest ListResolvers where
  type Rs ListResolvers = ListResolversResponse
  request = Req.get appSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListResolversResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "resolvers" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListResolvers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListResolvers where
  toPath ListResolvers' {..} =
    Lude.mconcat
      [ "/v1/apis/",
        Lude.toBS apiId,
        "/types/",
        Lude.toBS typeName,
        "/resolvers"
      ]

instance Lude.ToQuery ListResolvers where
  toQuery ListResolvers' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListResolversResponse' smart constructor.
data ListResolversResponse = ListResolversResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    resolvers :: Lude.Maybe [Resolver],
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

-- | Creates a value of 'ListResolversResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - An identifier to be passed in the next request to this operation to return the next set of items in the list.
-- * 'resolvers' - The @Resolver@ objects.
-- * 'responseStatus' - The response status code.
mkListResolversResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListResolversResponse
mkListResolversResponse pResponseStatus_ =
  ListResolversResponse'
    { nextToken = Lude.Nothing,
      resolvers = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An identifier to be passed in the next request to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsNextToken :: Lens.Lens' ListResolversResponse (Lude.Maybe Lude.Text)
lrrsNextToken = Lens.lens (nextToken :: ListResolversResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListResolversResponse)
{-# DEPRECATED lrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The @Resolver@ objects.
--
-- /Note:/ Consider using 'resolvers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsResolvers :: Lens.Lens' ListResolversResponse (Lude.Maybe [Resolver])
lrrsResolvers = Lens.lens (resolvers :: ListResolversResponse -> Lude.Maybe [Resolver]) (\s a -> s {resolvers = a} :: ListResolversResponse)
{-# DEPRECATED lrrsResolvers "Use generic-lens or generic-optics with 'resolvers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsResponseStatus :: Lens.Lens' ListResolversResponse Lude.Int
lrrsResponseStatus = Lens.lens (responseStatus :: ListResolversResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListResolversResponse)
{-# DEPRECATED lrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
