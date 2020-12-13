{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetVPCLinks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the 'VpcLinks' collection under the caller's account in a selected region.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetVPCLinks
  ( -- * Creating a request
    GetVPCLinks (..),
    mkGetVPCLinks,

    -- ** Request lenses
    gvlLimit,
    gvlPosition,

    -- * Destructuring the response
    GetVPCLinksResponse (..),
    mkGetVPCLinksResponse,

    -- ** Response lenses
    gvlrsItems,
    gvlrsPosition,
    gvlrsResponseStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Gets the 'VpcLinks' collection under the caller's account in a selected region.
--
-- /See:/ 'mkGetVPCLinks' smart constructor.
data GetVPCLinks = GetVPCLinks'
  { -- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
    limit :: Lude.Maybe Lude.Int,
    -- | The current pagination position in the paged result set.
    position :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetVPCLinks' with the minimum fields required to make a request.
--
-- * 'limit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
-- * 'position' - The current pagination position in the paged result set.
mkGetVPCLinks ::
  GetVPCLinks
mkGetVPCLinks =
  GetVPCLinks' {limit = Lude.Nothing, position = Lude.Nothing}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlLimit :: Lens.Lens' GetVPCLinks (Lude.Maybe Lude.Int)
gvlLimit = Lens.lens (limit :: GetVPCLinks -> Lude.Maybe Lude.Int) (\s a -> s {limit = a} :: GetVPCLinks)
{-# DEPRECATED gvlLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlPosition :: Lens.Lens' GetVPCLinks (Lude.Maybe Lude.Text)
gvlPosition = Lens.lens (position :: GetVPCLinks -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetVPCLinks)
{-# DEPRECATED gvlPosition "Use generic-lens or generic-optics with 'position' instead." #-}

instance Page.AWSPager GetVPCLinks where
  page rq rs
    | Page.stop (rs Lens.^. gvlrsPosition) = Lude.Nothing
    | Page.stop (rs Lens.^. gvlrsItems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gvlPosition Lens..~ rs Lens.^. gvlrsPosition

instance Lude.AWSRequest GetVPCLinks where
  type Rs GetVPCLinks = GetVPCLinksResponse
  request = Req.get apiGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetVPCLinksResponse'
            Lude.<$> (x Lude..?> "item" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "position")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetVPCLinks where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetVPCLinks where
  toPath = Lude.const "/vpclinks"

instance Lude.ToQuery GetVPCLinks where
  toQuery GetVPCLinks' {..} =
    Lude.mconcat ["limit" Lude.=: limit, "position" Lude.=: position]

-- | The collection of VPC links under the caller's account in a region.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/getting-started-with-private-integration.html Getting Started with Private Integrations> , <https://docs.aws.amazon.com/apigateway/latest/developerguide/set-up-private-integration.html Set up Private Integrations>
--
-- /See:/ 'mkGetVPCLinksResponse' smart constructor.
data GetVPCLinksResponse = GetVPCLinksResponse'
  { -- | The current page of elements from this collection.
    items :: Lude.Maybe [VPCLink],
    position :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetVPCLinksResponse' with the minimum fields required to make a request.
--
-- * 'items' - The current page of elements from this collection.
-- * 'position' -
-- * 'responseStatus' - The response status code.
mkGetVPCLinksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetVPCLinksResponse
mkGetVPCLinksResponse pResponseStatus_ =
  GetVPCLinksResponse'
    { items = Lude.Nothing,
      position = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlrsItems :: Lens.Lens' GetVPCLinksResponse (Lude.Maybe [VPCLink])
gvlrsItems = Lens.lens (items :: GetVPCLinksResponse -> Lude.Maybe [VPCLink]) (\s a -> s {items = a} :: GetVPCLinksResponse)
{-# DEPRECATED gvlrsItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlrsPosition :: Lens.Lens' GetVPCLinksResponse (Lude.Maybe Lude.Text)
gvlrsPosition = Lens.lens (position :: GetVPCLinksResponse -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetVPCLinksResponse)
{-# DEPRECATED gvlrsPosition "Use generic-lens or generic-optics with 'position' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlrsResponseStatus :: Lens.Lens' GetVPCLinksResponse Lude.Int
gvlrsResponseStatus = Lens.lens (responseStatus :: GetVPCLinksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetVPCLinksResponse)
{-# DEPRECATED gvlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
