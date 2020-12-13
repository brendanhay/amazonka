{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetSDKTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetSDKTypes
  ( -- * Creating a request
    GetSDKTypes (..),
    mkGetSDKTypes,

    -- ** Request lenses
    gstLimit,
    gstPosition,

    -- * Destructuring the response
    GetSDKTypesResponse (..),
    mkGetSDKTypesResponse,

    -- ** Response lenses
    gstrsItems,
    gstrsPosition,
    gstrsResponseStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Get the 'SdkTypes' collection.
--
-- /See:/ 'mkGetSDKTypes' smart constructor.
data GetSDKTypes = GetSDKTypes'
  { -- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
    limit :: Lude.Maybe Lude.Int,
    -- | The current pagination position in the paged result set.
    position :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSDKTypes' with the minimum fields required to make a request.
--
-- * 'limit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
-- * 'position' - The current pagination position in the paged result set.
mkGetSDKTypes ::
  GetSDKTypes
mkGetSDKTypes =
  GetSDKTypes' {limit = Lude.Nothing, position = Lude.Nothing}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstLimit :: Lens.Lens' GetSDKTypes (Lude.Maybe Lude.Int)
gstLimit = Lens.lens (limit :: GetSDKTypes -> Lude.Maybe Lude.Int) (\s a -> s {limit = a} :: GetSDKTypes)
{-# DEPRECATED gstLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstPosition :: Lens.Lens' GetSDKTypes (Lude.Maybe Lude.Text)
gstPosition = Lens.lens (position :: GetSDKTypes -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetSDKTypes)
{-# DEPRECATED gstPosition "Use generic-lens or generic-optics with 'position' instead." #-}

instance Page.AWSPager GetSDKTypes where
  page rq rs
    | Page.stop (rs Lens.^. gstrsPosition) = Lude.Nothing
    | Page.stop (rs Lens.^. gstrsItems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gstPosition Lens..~ rs Lens.^. gstrsPosition

instance Lude.AWSRequest GetSDKTypes where
  type Rs GetSDKTypes = GetSDKTypesResponse
  request = Req.get apiGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSDKTypesResponse'
            Lude.<$> (x Lude..?> "item" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "position")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSDKTypes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetSDKTypes where
  toPath = Lude.const "/sdktypes"

instance Lude.ToQuery GetSDKTypes where
  toQuery GetSDKTypes' {..} =
    Lude.mconcat ["limit" Lude.=: limit, "position" Lude.=: position]

-- | The collection of 'SdkType' instances.
--
-- /See:/ 'mkGetSDKTypesResponse' smart constructor.
data GetSDKTypesResponse = GetSDKTypesResponse'
  { -- | The current page of elements from this collection.
    items :: Lude.Maybe [SDKType],
    position :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSDKTypesResponse' with the minimum fields required to make a request.
--
-- * 'items' - The current page of elements from this collection.
-- * 'position' -
-- * 'responseStatus' - The response status code.
mkGetSDKTypesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSDKTypesResponse
mkGetSDKTypesResponse pResponseStatus_ =
  GetSDKTypesResponse'
    { items = Lude.Nothing,
      position = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrsItems :: Lens.Lens' GetSDKTypesResponse (Lude.Maybe [SDKType])
gstrsItems = Lens.lens (items :: GetSDKTypesResponse -> Lude.Maybe [SDKType]) (\s a -> s {items = a} :: GetSDKTypesResponse)
{-# DEPRECATED gstrsItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrsPosition :: Lens.Lens' GetSDKTypesResponse (Lude.Maybe Lude.Text)
gstrsPosition = Lens.lens (position :: GetSDKTypesResponse -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetSDKTypesResponse)
{-# DEPRECATED gstrsPosition "Use generic-lens or generic-optics with 'position' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrsResponseStatus :: Lens.Lens' GetSDKTypesResponse Lude.Int
gstrsResponseStatus = Lens.lens (responseStatus :: GetSDKTypesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSDKTypesResponse)
{-# DEPRECATED gstrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
