{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetUsagePlanKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets all the usage plan keys representing the API keys added to a specified usage plan.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetUsagePlanKeys
  ( -- * Creating a request
    GetUsagePlanKeys (..),
    mkGetUsagePlanKeys,

    -- ** Request lenses
    gupkUsagePlanId,
    gupkNameQuery,
    gupkLimit,
    gupkPosition,

    -- * Destructuring the response
    GetUsagePlanKeysResponse (..),
    mkGetUsagePlanKeysResponse,

    -- ** Response lenses
    gupkrsItems,
    gupkrsPosition,
    gupkrsResponseStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The GET request to get all the usage plan keys representing the API keys added to a specified usage plan.
--
-- /See:/ 'mkGetUsagePlanKeys' smart constructor.
data GetUsagePlanKeys = GetUsagePlanKeys'
  { -- | [Required] The Id of the 'UsagePlan' resource representing the usage plan containing the to-be-retrieved 'UsagePlanKey' resource representing a plan customer.
    usagePlanId :: Lude.Text,
    -- | A query parameter specifying the name of the to-be-returned usage plan keys.
    nameQuery :: Lude.Maybe Lude.Text,
    -- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
    limit :: Lude.Maybe Lude.Int,
    -- | The current pagination position in the paged result set.
    position :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUsagePlanKeys' with the minimum fields required to make a request.
--
-- * 'usagePlanId' - [Required] The Id of the 'UsagePlan' resource representing the usage plan containing the to-be-retrieved 'UsagePlanKey' resource representing a plan customer.
-- * 'nameQuery' - A query parameter specifying the name of the to-be-returned usage plan keys.
-- * 'limit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
-- * 'position' - The current pagination position in the paged result set.
mkGetUsagePlanKeys ::
  -- | 'usagePlanId'
  Lude.Text ->
  GetUsagePlanKeys
mkGetUsagePlanKeys pUsagePlanId_ =
  GetUsagePlanKeys'
    { usagePlanId = pUsagePlanId_,
      nameQuery = Lude.Nothing,
      limit = Lude.Nothing,
      position = Lude.Nothing
    }

-- | [Required] The Id of the 'UsagePlan' resource representing the usage plan containing the to-be-retrieved 'UsagePlanKey' resource representing a plan customer.
--
-- /Note:/ Consider using 'usagePlanId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupkUsagePlanId :: Lens.Lens' GetUsagePlanKeys Lude.Text
gupkUsagePlanId = Lens.lens (usagePlanId :: GetUsagePlanKeys -> Lude.Text) (\s a -> s {usagePlanId = a} :: GetUsagePlanKeys)
{-# DEPRECATED gupkUsagePlanId "Use generic-lens or generic-optics with 'usagePlanId' instead." #-}

-- | A query parameter specifying the name of the to-be-returned usage plan keys.
--
-- /Note:/ Consider using 'nameQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupkNameQuery :: Lens.Lens' GetUsagePlanKeys (Lude.Maybe Lude.Text)
gupkNameQuery = Lens.lens (nameQuery :: GetUsagePlanKeys -> Lude.Maybe Lude.Text) (\s a -> s {nameQuery = a} :: GetUsagePlanKeys)
{-# DEPRECATED gupkNameQuery "Use generic-lens or generic-optics with 'nameQuery' instead." #-}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupkLimit :: Lens.Lens' GetUsagePlanKeys (Lude.Maybe Lude.Int)
gupkLimit = Lens.lens (limit :: GetUsagePlanKeys -> Lude.Maybe Lude.Int) (\s a -> s {limit = a} :: GetUsagePlanKeys)
{-# DEPRECATED gupkLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupkPosition :: Lens.Lens' GetUsagePlanKeys (Lude.Maybe Lude.Text)
gupkPosition = Lens.lens (position :: GetUsagePlanKeys -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetUsagePlanKeys)
{-# DEPRECATED gupkPosition "Use generic-lens or generic-optics with 'position' instead." #-}

instance Page.AWSPager GetUsagePlanKeys where
  page rq rs
    | Page.stop (rs Lens.^. gupkrsPosition) = Lude.Nothing
    | Page.stop (rs Lens.^. gupkrsItems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gupkPosition Lens..~ rs Lens.^. gupkrsPosition

instance Lude.AWSRequest GetUsagePlanKeys where
  type Rs GetUsagePlanKeys = GetUsagePlanKeysResponse
  request = Req.get apiGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetUsagePlanKeysResponse'
            Lude.<$> (x Lude..?> "item" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "position")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetUsagePlanKeys where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetUsagePlanKeys where
  toPath GetUsagePlanKeys' {..} =
    Lude.mconcat ["/usageplans/", Lude.toBS usagePlanId, "/keys"]

instance Lude.ToQuery GetUsagePlanKeys where
  toQuery GetUsagePlanKeys' {..} =
    Lude.mconcat
      [ "name" Lude.=: nameQuery,
        "limit" Lude.=: limit,
        "position" Lude.=: position
      ]

-- | Represents the collection of usage plan keys added to usage plans for the associated API keys and, possibly, other types of keys.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html Create and Use Usage Plans>
--
-- /See:/ 'mkGetUsagePlanKeysResponse' smart constructor.
data GetUsagePlanKeysResponse = GetUsagePlanKeysResponse'
  { -- | The current page of elements from this collection.
    items :: Lude.Maybe [UsagePlanKey],
    position :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUsagePlanKeysResponse' with the minimum fields required to make a request.
--
-- * 'items' - The current page of elements from this collection.
-- * 'position' -
-- * 'responseStatus' - The response status code.
mkGetUsagePlanKeysResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetUsagePlanKeysResponse
mkGetUsagePlanKeysResponse pResponseStatus_ =
  GetUsagePlanKeysResponse'
    { items = Lude.Nothing,
      position = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupkrsItems :: Lens.Lens' GetUsagePlanKeysResponse (Lude.Maybe [UsagePlanKey])
gupkrsItems = Lens.lens (items :: GetUsagePlanKeysResponse -> Lude.Maybe [UsagePlanKey]) (\s a -> s {items = a} :: GetUsagePlanKeysResponse)
{-# DEPRECATED gupkrsItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupkrsPosition :: Lens.Lens' GetUsagePlanKeysResponse (Lude.Maybe Lude.Text)
gupkrsPosition = Lens.lens (position :: GetUsagePlanKeysResponse -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetUsagePlanKeysResponse)
{-# DEPRECATED gupkrsPosition "Use generic-lens or generic-optics with 'position' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupkrsResponseStatus :: Lens.Lens' GetUsagePlanKeysResponse Lude.Int
gupkrsResponseStatus = Lens.lens (responseStatus :: GetUsagePlanKeysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetUsagePlanKeysResponse)
{-# DEPRECATED gupkrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
