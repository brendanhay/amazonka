{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetDomainNames
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a collection of 'DomainName' resources.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetDomainNames
  ( -- * Creating a request
    GetDomainNames (..),
    mkGetDomainNames,

    -- ** Request lenses
    gdnLimit,
    gdnPosition,

    -- * Destructuring the response
    GetDomainNamesResponse (..),
    mkGetDomainNamesResponse,

    -- ** Response lenses
    gdnrsItems,
    gdnrsPosition,
    gdnrsResponseStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to describe a collection of 'DomainName' resources.
--
-- /See:/ 'mkGetDomainNames' smart constructor.
data GetDomainNames = GetDomainNames'
  { -- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
    limit :: Lude.Maybe Lude.Int,
    -- | The current pagination position in the paged result set.
    position :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDomainNames' with the minimum fields required to make a request.
--
-- * 'limit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
-- * 'position' - The current pagination position in the paged result set.
mkGetDomainNames ::
  GetDomainNames
mkGetDomainNames =
  GetDomainNames' {limit = Lude.Nothing, position = Lude.Nothing}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdnLimit :: Lens.Lens' GetDomainNames (Lude.Maybe Lude.Int)
gdnLimit = Lens.lens (limit :: GetDomainNames -> Lude.Maybe Lude.Int) (\s a -> s {limit = a} :: GetDomainNames)
{-# DEPRECATED gdnLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdnPosition :: Lens.Lens' GetDomainNames (Lude.Maybe Lude.Text)
gdnPosition = Lens.lens (position :: GetDomainNames -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetDomainNames)
{-# DEPRECATED gdnPosition "Use generic-lens or generic-optics with 'position' instead." #-}

instance Page.AWSPager GetDomainNames where
  page rq rs
    | Page.stop (rs Lens.^. gdnrsPosition) = Lude.Nothing
    | Page.stop (rs Lens.^. gdnrsItems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gdnPosition Lens..~ rs Lens.^. gdnrsPosition

instance Lude.AWSRequest GetDomainNames where
  type Rs GetDomainNames = GetDomainNamesResponse
  request = Req.get apiGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDomainNamesResponse'
            Lude.<$> (x Lude..?> "item" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "position")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDomainNames where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetDomainNames where
  toPath = Lude.const "/domainnames"

instance Lude.ToQuery GetDomainNames where
  toQuery GetDomainNames' {..} =
    Lude.mconcat ["limit" Lude.=: limit, "position" Lude.=: position]

-- | Represents a collection of 'DomainName' resources.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-custom-domains.html Use Client-Side Certificate>
--
-- /See:/ 'mkGetDomainNamesResponse' smart constructor.
data GetDomainNamesResponse = GetDomainNamesResponse'
  { -- | The current page of elements from this collection.
    items :: Lude.Maybe [DomainName],
    position :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDomainNamesResponse' with the minimum fields required to make a request.
--
-- * 'items' - The current page of elements from this collection.
-- * 'position' -
-- * 'responseStatus' - The response status code.
mkGetDomainNamesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDomainNamesResponse
mkGetDomainNamesResponse pResponseStatus_ =
  GetDomainNamesResponse'
    { items = Lude.Nothing,
      position = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdnrsItems :: Lens.Lens' GetDomainNamesResponse (Lude.Maybe [DomainName])
gdnrsItems = Lens.lens (items :: GetDomainNamesResponse -> Lude.Maybe [DomainName]) (\s a -> s {items = a} :: GetDomainNamesResponse)
{-# DEPRECATED gdnrsItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdnrsPosition :: Lens.Lens' GetDomainNamesResponse (Lude.Maybe Lude.Text)
gdnrsPosition = Lens.lens (position :: GetDomainNamesResponse -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetDomainNamesResponse)
{-# DEPRECATED gdnrsPosition "Use generic-lens or generic-optics with 'position' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdnrsResponseStatus :: Lens.Lens' GetDomainNamesResponse Lude.Int
gdnrsResponseStatus = Lens.lens (responseStatus :: GetDomainNamesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDomainNamesResponse)
{-# DEPRECATED gdnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
