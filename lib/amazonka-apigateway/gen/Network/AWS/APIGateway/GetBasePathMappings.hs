{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetBasePathMappings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a collection of 'BasePathMapping' resources.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetBasePathMappings
  ( -- * Creating a request
    GetBasePathMappings (..),
    mkGetBasePathMappings,

    -- ** Request lenses
    gbpmDomainName,
    gbpmLimit,
    gbpmPosition,

    -- * Destructuring the response
    GetBasePathMappingsResponse (..),
    mkGetBasePathMappingsResponse,

    -- ** Response lenses
    gbpmrsItems,
    gbpmrsPosition,
    gbpmrsResponseStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to get information about a collection of 'BasePathMapping' resources.
--
-- /See:/ 'mkGetBasePathMappings' smart constructor.
data GetBasePathMappings = GetBasePathMappings'
  { -- | [Required] The domain name of a 'BasePathMapping' resource.
    domainName :: Lude.Text,
    -- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
    limit :: Lude.Maybe Lude.Int,
    -- | The current pagination position in the paged result set.
    position :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBasePathMappings' with the minimum fields required to make a request.
--
-- * 'domainName' - [Required] The domain name of a 'BasePathMapping' resource.
-- * 'limit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
-- * 'position' - The current pagination position in the paged result set.
mkGetBasePathMappings ::
  -- | 'domainName'
  Lude.Text ->
  GetBasePathMappings
mkGetBasePathMappings pDomainName_ =
  GetBasePathMappings'
    { domainName = pDomainName_,
      limit = Lude.Nothing,
      position = Lude.Nothing
    }

-- | [Required] The domain name of a 'BasePathMapping' resource.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpmDomainName :: Lens.Lens' GetBasePathMappings Lude.Text
gbpmDomainName = Lens.lens (domainName :: GetBasePathMappings -> Lude.Text) (\s a -> s {domainName = a} :: GetBasePathMappings)
{-# DEPRECATED gbpmDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpmLimit :: Lens.Lens' GetBasePathMappings (Lude.Maybe Lude.Int)
gbpmLimit = Lens.lens (limit :: GetBasePathMappings -> Lude.Maybe Lude.Int) (\s a -> s {limit = a} :: GetBasePathMappings)
{-# DEPRECATED gbpmLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpmPosition :: Lens.Lens' GetBasePathMappings (Lude.Maybe Lude.Text)
gbpmPosition = Lens.lens (position :: GetBasePathMappings -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetBasePathMappings)
{-# DEPRECATED gbpmPosition "Use generic-lens or generic-optics with 'position' instead." #-}

instance Page.AWSPager GetBasePathMappings where
  page rq rs
    | Page.stop (rs Lens.^. gbpmrsPosition) = Lude.Nothing
    | Page.stop (rs Lens.^. gbpmrsItems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gbpmPosition Lens..~ rs Lens.^. gbpmrsPosition

instance Lude.AWSRequest GetBasePathMappings where
  type Rs GetBasePathMappings = GetBasePathMappingsResponse
  request = Req.get apiGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetBasePathMappingsResponse'
            Lude.<$> (x Lude..?> "item" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "position")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBasePathMappings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetBasePathMappings where
  toPath GetBasePathMappings' {..} =
    Lude.mconcat
      ["/domainnames/", Lude.toBS domainName, "/basepathmappings"]

instance Lude.ToQuery GetBasePathMappings where
  toQuery GetBasePathMappings' {..} =
    Lude.mconcat ["limit" Lude.=: limit, "position" Lude.=: position]

-- | Represents a collection of 'BasePathMapping' resources.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-custom-domains.html Use Custom Domain Names>
--
-- /See:/ 'mkGetBasePathMappingsResponse' smart constructor.
data GetBasePathMappingsResponse = GetBasePathMappingsResponse'
  { -- | The current page of elements from this collection.
    items :: Lude.Maybe [BasePathMapping],
    position :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBasePathMappingsResponse' with the minimum fields required to make a request.
--
-- * 'items' - The current page of elements from this collection.
-- * 'position' -
-- * 'responseStatus' - The response status code.
mkGetBasePathMappingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBasePathMappingsResponse
mkGetBasePathMappingsResponse pResponseStatus_ =
  GetBasePathMappingsResponse'
    { items = Lude.Nothing,
      position = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpmrsItems :: Lens.Lens' GetBasePathMappingsResponse (Lude.Maybe [BasePathMapping])
gbpmrsItems = Lens.lens (items :: GetBasePathMappingsResponse -> Lude.Maybe [BasePathMapping]) (\s a -> s {items = a} :: GetBasePathMappingsResponse)
{-# DEPRECATED gbpmrsItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpmrsPosition :: Lens.Lens' GetBasePathMappingsResponse (Lude.Maybe Lude.Text)
gbpmrsPosition = Lens.lens (position :: GetBasePathMappingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetBasePathMappingsResponse)
{-# DEPRECATED gbpmrsPosition "Use generic-lens or generic-optics with 'position' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpmrsResponseStatus :: Lens.Lens' GetBasePathMappingsResponse Lude.Int
gbpmrsResponseStatus = Lens.lens (responseStatus :: GetBasePathMappingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBasePathMappingsResponse)
{-# DEPRECATED gbpmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
