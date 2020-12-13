{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetClientCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a collection of 'ClientCertificate' resources.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetClientCertificates
  ( -- * Creating a request
    GetClientCertificates (..),
    mkGetClientCertificates,

    -- ** Request lenses
    gccLimit,
    gccPosition,

    -- * Destructuring the response
    GetClientCertificatesResponse (..),
    mkGetClientCertificatesResponse,

    -- ** Response lenses
    gccrsItems,
    gccrsPosition,
    gccrsResponseStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to get information about a collection of 'ClientCertificate' resources.
--
-- /See:/ 'mkGetClientCertificates' smart constructor.
data GetClientCertificates = GetClientCertificates'
  { -- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
    limit :: Lude.Maybe Lude.Int,
    -- | The current pagination position in the paged result set.
    position :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetClientCertificates' with the minimum fields required to make a request.
--
-- * 'limit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
-- * 'position' - The current pagination position in the paged result set.
mkGetClientCertificates ::
  GetClientCertificates
mkGetClientCertificates =
  GetClientCertificates'
    { limit = Lude.Nothing,
      position = Lude.Nothing
    }

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccLimit :: Lens.Lens' GetClientCertificates (Lude.Maybe Lude.Int)
gccLimit = Lens.lens (limit :: GetClientCertificates -> Lude.Maybe Lude.Int) (\s a -> s {limit = a} :: GetClientCertificates)
{-# DEPRECATED gccLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccPosition :: Lens.Lens' GetClientCertificates (Lude.Maybe Lude.Text)
gccPosition = Lens.lens (position :: GetClientCertificates -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetClientCertificates)
{-# DEPRECATED gccPosition "Use generic-lens or generic-optics with 'position' instead." #-}

instance Page.AWSPager GetClientCertificates where
  page rq rs
    | Page.stop (rs Lens.^. gccrsPosition) = Lude.Nothing
    | Page.stop (rs Lens.^. gccrsItems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gccPosition Lens..~ rs Lens.^. gccrsPosition

instance Lude.AWSRequest GetClientCertificates where
  type Rs GetClientCertificates = GetClientCertificatesResponse
  request = Req.get apiGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetClientCertificatesResponse'
            Lude.<$> (x Lude..?> "item" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "position")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetClientCertificates where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetClientCertificates where
  toPath = Lude.const "/clientcertificates"

instance Lude.ToQuery GetClientCertificates where
  toQuery GetClientCertificates' {..} =
    Lude.mconcat ["limit" Lude.=: limit, "position" Lude.=: position]

-- | Represents a collection of 'ClientCertificate' resources.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/getting-started-client-side-ssl-authentication.html Use Client-Side Certificate>
--
-- /See:/ 'mkGetClientCertificatesResponse' smart constructor.
data GetClientCertificatesResponse = GetClientCertificatesResponse'
  { -- | The current page of elements from this collection.
    items :: Lude.Maybe [ClientCertificate],
    position :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetClientCertificatesResponse' with the minimum fields required to make a request.
--
-- * 'items' - The current page of elements from this collection.
-- * 'position' -
-- * 'responseStatus' - The response status code.
mkGetClientCertificatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetClientCertificatesResponse
mkGetClientCertificatesResponse pResponseStatus_ =
  GetClientCertificatesResponse'
    { items = Lude.Nothing,
      position = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccrsItems :: Lens.Lens' GetClientCertificatesResponse (Lude.Maybe [ClientCertificate])
gccrsItems = Lens.lens (items :: GetClientCertificatesResponse -> Lude.Maybe [ClientCertificate]) (\s a -> s {items = a} :: GetClientCertificatesResponse)
{-# DEPRECATED gccrsItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccrsPosition :: Lens.Lens' GetClientCertificatesResponse (Lude.Maybe Lude.Text)
gccrsPosition = Lens.lens (position :: GetClientCertificatesResponse -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetClientCertificatesResponse)
{-# DEPRECATED gccrsPosition "Use generic-lens or generic-optics with 'position' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccrsResponseStatus :: Lens.Lens' GetClientCertificatesResponse Lude.Int
gccrsResponseStatus = Lens.lens (responseStatus :: GetClientCertificatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetClientCertificatesResponse)
{-# DEPRECATED gccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
