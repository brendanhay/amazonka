{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetUsagePlans
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets all the usage plans of the caller's account.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetUsagePlans
  ( -- * Creating a request
    GetUsagePlans (..),
    mkGetUsagePlans,

    -- ** Request lenses
    gupKeyId,
    gupLimit,
    gupPosition,

    -- * Destructuring the response
    GetUsagePlansResponse (..),
    mkGetUsagePlansResponse,

    -- ** Response lenses
    guprsItems,
    guprsPosition,
    guprsResponseStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The GET request to get all the usage plans of the caller's account.
--
-- /See:/ 'mkGetUsagePlans' smart constructor.
data GetUsagePlans = GetUsagePlans'
  { -- | The identifier of the API key associated with the usage plans.
    keyId :: Lude.Maybe Lude.Text,
    -- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
    limit :: Lude.Maybe Lude.Int,
    -- | The current pagination position in the paged result set.
    position :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUsagePlans' with the minimum fields required to make a request.
--
-- * 'keyId' - The identifier of the API key associated with the usage plans.
-- * 'limit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
-- * 'position' - The current pagination position in the paged result set.
mkGetUsagePlans ::
  GetUsagePlans
mkGetUsagePlans =
  GetUsagePlans'
    { keyId = Lude.Nothing,
      limit = Lude.Nothing,
      position = Lude.Nothing
    }

-- | The identifier of the API key associated with the usage plans.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupKeyId :: Lens.Lens' GetUsagePlans (Lude.Maybe Lude.Text)
gupKeyId = Lens.lens (keyId :: GetUsagePlans -> Lude.Maybe Lude.Text) (\s a -> s {keyId = a} :: GetUsagePlans)
{-# DEPRECATED gupKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupLimit :: Lens.Lens' GetUsagePlans (Lude.Maybe Lude.Int)
gupLimit = Lens.lens (limit :: GetUsagePlans -> Lude.Maybe Lude.Int) (\s a -> s {limit = a} :: GetUsagePlans)
{-# DEPRECATED gupLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupPosition :: Lens.Lens' GetUsagePlans (Lude.Maybe Lude.Text)
gupPosition = Lens.lens (position :: GetUsagePlans -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetUsagePlans)
{-# DEPRECATED gupPosition "Use generic-lens or generic-optics with 'position' instead." #-}

instance Page.AWSPager GetUsagePlans where
  page rq rs
    | Page.stop (rs Lens.^. guprsPosition) = Lude.Nothing
    | Page.stop (rs Lens.^. guprsItems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gupPosition Lens..~ rs Lens.^. guprsPosition

instance Lude.AWSRequest GetUsagePlans where
  type Rs GetUsagePlans = GetUsagePlansResponse
  request = Req.get apiGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetUsagePlansResponse'
            Lude.<$> (x Lude..?> "item" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "position")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetUsagePlans where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetUsagePlans where
  toPath = Lude.const "/usageplans"

instance Lude.ToQuery GetUsagePlans where
  toQuery GetUsagePlans' {..} =
    Lude.mconcat
      [ "keyId" Lude.=: keyId,
        "limit" Lude.=: limit,
        "position" Lude.=: position
      ]

-- | Represents a collection of usage plans for an AWS account.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html Create and Use Usage Plans>
--
-- /See:/ 'mkGetUsagePlansResponse' smart constructor.
data GetUsagePlansResponse = GetUsagePlansResponse'
  { -- | The current page of elements from this collection.
    items :: Lude.Maybe [UsagePlan],
    position :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUsagePlansResponse' with the minimum fields required to make a request.
--
-- * 'items' - The current page of elements from this collection.
-- * 'position' -
-- * 'responseStatus' - The response status code.
mkGetUsagePlansResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetUsagePlansResponse
mkGetUsagePlansResponse pResponseStatus_ =
  GetUsagePlansResponse'
    { items = Lude.Nothing,
      position = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guprsItems :: Lens.Lens' GetUsagePlansResponse (Lude.Maybe [UsagePlan])
guprsItems = Lens.lens (items :: GetUsagePlansResponse -> Lude.Maybe [UsagePlan]) (\s a -> s {items = a} :: GetUsagePlansResponse)
{-# DEPRECATED guprsItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guprsPosition :: Lens.Lens' GetUsagePlansResponse (Lude.Maybe Lude.Text)
guprsPosition = Lens.lens (position :: GetUsagePlansResponse -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetUsagePlansResponse)
{-# DEPRECATED guprsPosition "Use generic-lens or generic-optics with 'position' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guprsResponseStatus :: Lens.Lens' GetUsagePlansResponse Lude.Int
guprsResponseStatus = Lens.lens (responseStatus :: GetUsagePlansResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetUsagePlansResponse)
{-# DEPRECATED guprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
