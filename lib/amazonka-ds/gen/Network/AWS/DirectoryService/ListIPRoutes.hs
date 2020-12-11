{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.ListIPRoutes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the address blocks that you have added to a directory.
--
-- This operation returns paginated results.
module Network.AWS.DirectoryService.ListIPRoutes
  ( -- * Creating a request
    ListIPRoutes (..),
    mkListIPRoutes,

    -- ** Request lenses
    lirNextToken,
    lirLimit,
    lirDirectoryId,

    -- * Destructuring the response
    ListIPRoutesResponse (..),
    mkListIPRoutesResponse,

    -- ** Response lenses
    lirrsIPRoutesInfo,
    lirrsNextToken,
    lirrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListIPRoutes' smart constructor.
data ListIPRoutes = ListIPRoutes'
  { nextToken ::
      Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    directoryId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListIPRoutes' with the minimum fields required to make a request.
--
-- * 'directoryId' - Identifier (ID) of the directory for which you want to retrieve the IP addresses.
-- * 'limit' - Maximum number of items to return. If this value is zero, the maximum number of items is specified by the limitations of the operation.
-- * 'nextToken' - The /ListIpRoutes.NextToken/ value from a previous call to 'ListIpRoutes' . Pass null if this is the first call.
mkListIPRoutes ::
  -- | 'directoryId'
  Lude.Text ->
  ListIPRoutes
mkListIPRoutes pDirectoryId_ =
  ListIPRoutes'
    { nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      directoryId = pDirectoryId_
    }

-- | The /ListIpRoutes.NextToken/ value from a previous call to 'ListIpRoutes' . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirNextToken :: Lens.Lens' ListIPRoutes (Lude.Maybe Lude.Text)
lirNextToken = Lens.lens (nextToken :: ListIPRoutes -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListIPRoutes)
{-# DEPRECATED lirNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Maximum number of items to return. If this value is zero, the maximum number of items is specified by the limitations of the operation.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirLimit :: Lens.Lens' ListIPRoutes (Lude.Maybe Lude.Natural)
lirLimit = Lens.lens (limit :: ListIPRoutes -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListIPRoutes)
{-# DEPRECATED lirLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | Identifier (ID) of the directory for which you want to retrieve the IP addresses.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirDirectoryId :: Lens.Lens' ListIPRoutes Lude.Text
lirDirectoryId = Lens.lens (directoryId :: ListIPRoutes -> Lude.Text) (\s a -> s {directoryId = a} :: ListIPRoutes)
{-# DEPRECATED lirDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

instance Page.AWSPager ListIPRoutes where
  page rq rs
    | Page.stop (rs Lens.^. lirrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lirrsIPRoutesInfo) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lirNextToken Lens..~ rs Lens.^. lirrsNextToken

instance Lude.AWSRequest ListIPRoutes where
  type Rs ListIPRoutes = ListIPRoutesResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListIPRoutesResponse'
            Lude.<$> (x Lude..?> "IpRoutesInfo" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListIPRoutes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.ListIpRoutes" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListIPRoutes where
  toJSON ListIPRoutes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just ("DirectoryId" Lude..= directoryId)
          ]
      )

instance Lude.ToPath ListIPRoutes where
  toPath = Lude.const "/"

instance Lude.ToQuery ListIPRoutes where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListIPRoutesResponse' smart constructor.
data ListIPRoutesResponse = ListIPRoutesResponse'
  { ipRoutesInfo ::
      Lude.Maybe [IPRouteInfo],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListIPRoutesResponse' with the minimum fields required to make a request.
--
-- * 'ipRoutesInfo' - A list of 'IpRoute' s.
-- * 'nextToken' - If not null, more results are available. Pass this value for the /NextToken/ parameter in a subsequent call to 'ListIpRoutes' to retrieve the next set of items.
-- * 'responseStatus' - The response status code.
mkListIPRoutesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListIPRoutesResponse
mkListIPRoutesResponse pResponseStatus_ =
  ListIPRoutesResponse'
    { ipRoutesInfo = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of 'IpRoute' s.
--
-- /Note:/ Consider using 'ipRoutesInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsIPRoutesInfo :: Lens.Lens' ListIPRoutesResponse (Lude.Maybe [IPRouteInfo])
lirrsIPRoutesInfo = Lens.lens (ipRoutesInfo :: ListIPRoutesResponse -> Lude.Maybe [IPRouteInfo]) (\s a -> s {ipRoutesInfo = a} :: ListIPRoutesResponse)
{-# DEPRECATED lirrsIPRoutesInfo "Use generic-lens or generic-optics with 'ipRoutesInfo' instead." #-}

-- | If not null, more results are available. Pass this value for the /NextToken/ parameter in a subsequent call to 'ListIpRoutes' to retrieve the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsNextToken :: Lens.Lens' ListIPRoutesResponse (Lude.Maybe Lude.Text)
lirrsNextToken = Lens.lens (nextToken :: ListIPRoutesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListIPRoutesResponse)
{-# DEPRECATED lirrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsResponseStatus :: Lens.Lens' ListIPRoutesResponse Lude.Int
lirrsResponseStatus = Lens.lens (responseStatus :: ListIPRoutesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListIPRoutesResponse)
{-# DEPRECATED lirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
