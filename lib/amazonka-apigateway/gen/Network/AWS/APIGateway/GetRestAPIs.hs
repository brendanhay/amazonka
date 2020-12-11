{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetRestAPIs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the 'RestApis' resources for your collection.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetRestAPIs
  ( -- * Creating a request
    GetRestAPIs (..),
    mkGetRestAPIs,

    -- ** Request lenses
    graLimit,
    graPosition,

    -- * Destructuring the response
    GetRestAPIsResponse (..),
    mkGetRestAPIsResponse,

    -- ** Response lenses
    grarsItems,
    grarsPosition,
    grarsResponseStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The GET request to list existing 'RestApis' defined for your collection.
--
-- /See:/ 'mkGetRestAPIs' smart constructor.
data GetRestAPIs = GetRestAPIs'
  { limit :: Lude.Maybe Lude.Int,
    position :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRestAPIs' with the minimum fields required to make a request.
--
-- * 'limit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
-- * 'position' - The current pagination position in the paged result set.
mkGetRestAPIs ::
  GetRestAPIs
mkGetRestAPIs =
  GetRestAPIs' {limit = Lude.Nothing, position = Lude.Nothing}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
graLimit :: Lens.Lens' GetRestAPIs (Lude.Maybe Lude.Int)
graLimit = Lens.lens (limit :: GetRestAPIs -> Lude.Maybe Lude.Int) (\s a -> s {limit = a} :: GetRestAPIs)
{-# DEPRECATED graLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
graPosition :: Lens.Lens' GetRestAPIs (Lude.Maybe Lude.Text)
graPosition = Lens.lens (position :: GetRestAPIs -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetRestAPIs)
{-# DEPRECATED graPosition "Use generic-lens or generic-optics with 'position' instead." #-}

instance Page.AWSPager GetRestAPIs where
  page rq rs
    | Page.stop (rs Lens.^. grarsPosition) = Lude.Nothing
    | Page.stop (rs Lens.^. grarsItems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& graPosition Lens..~ rs Lens.^. grarsPosition

instance Lude.AWSRequest GetRestAPIs where
  type Rs GetRestAPIs = GetRestAPIsResponse
  request = Req.get apiGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRestAPIsResponse'
            Lude.<$> (x Lude..?> "item" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "position")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRestAPIs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetRestAPIs where
  toPath = Lude.const "/restapis"

instance Lude.ToQuery GetRestAPIs where
  toQuery GetRestAPIs' {..} =
    Lude.mconcat ["limit" Lude.=: limit, "position" Lude.=: position]

-- | Contains references to your APIs and links that guide you in how to interact with your collection. A collection offers a paginated view of your APIs.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Create an API>
--
-- /See:/ 'mkGetRestAPIsResponse' smart constructor.
data GetRestAPIsResponse = GetRestAPIsResponse'
  { items ::
      Lude.Maybe [RestAPI],
    position :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetRestAPIsResponse' with the minimum fields required to make a request.
--
-- * 'items' - The current page of elements from this collection.
-- * 'position' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkGetRestAPIsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRestAPIsResponse
mkGetRestAPIsResponse pResponseStatus_ =
  GetRestAPIsResponse'
    { items = Lude.Nothing,
      position = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grarsItems :: Lens.Lens' GetRestAPIsResponse (Lude.Maybe [RestAPI])
grarsItems = Lens.lens (items :: GetRestAPIsResponse -> Lude.Maybe [RestAPI]) (\s a -> s {items = a} :: GetRestAPIsResponse)
{-# DEPRECATED grarsItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grarsPosition :: Lens.Lens' GetRestAPIsResponse (Lude.Maybe Lude.Text)
grarsPosition = Lens.lens (position :: GetRestAPIsResponse -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetRestAPIsResponse)
{-# DEPRECATED grarsPosition "Use generic-lens or generic-optics with 'position' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grarsResponseStatus :: Lens.Lens' GetRestAPIsResponse Lude.Int
grarsResponseStatus = Lens.lens (responseStatus :: GetRestAPIsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRestAPIsResponse)
{-# DEPRECATED grarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
