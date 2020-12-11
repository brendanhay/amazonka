{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.ListKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of all customer master keys (CMKs) in the caller's AWS account and Region.
--
-- This operation returns paginated results.
module Network.AWS.KMS.ListKeys
  ( -- * Creating a request
    ListKeys (..),
    mkListKeys,

    -- ** Request lenses
    lkMarker,
    lkLimit,

    -- * Destructuring the response
    ListKeysResponse (..),
    mkListKeysResponse,

    -- ** Response lenses
    lkrsTruncated,
    lkrsKeys,
    lkrsNextMarker,
    lkrsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListKeys' smart constructor.
data ListKeys = ListKeys'
  { marker :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListKeys' with the minimum fields required to make a request.
--
-- * 'limit' - Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and 1000, inclusive. If you do not include a value, it defaults to 100.
-- * 'marker' - Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
mkListKeys ::
  ListKeys
mkListKeys = ListKeys' {marker = Lude.Nothing, limit = Lude.Nothing}

-- | Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkMarker :: Lens.Lens' ListKeys (Lude.Maybe Lude.Text)
lkMarker = Lens.lens (marker :: ListKeys -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListKeys)
{-# DEPRECATED lkMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer.
--
-- This value is optional. If you include a value, it must be between 1 and 1000, inclusive. If you do not include a value, it defaults to 100.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkLimit :: Lens.Lens' ListKeys (Lude.Maybe Lude.Natural)
lkLimit = Lens.lens (limit :: ListKeys -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListKeys)
{-# DEPRECATED lkLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager ListKeys where
  page rq rs
    | Page.stop (rs Lens.^. lkrsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lkrsNextMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lkMarker Lens..~ rs Lens.^. lkrsNextMarker

instance Lude.AWSRequest ListKeys where
  type Rs ListKeys = ListKeysResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListKeysResponse'
            Lude.<$> (x Lude..?> "Truncated")
            Lude.<*> (x Lude..?> "Keys" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListKeys where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.ListKeys" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListKeys where
  toJSON ListKeys' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Marker" Lude..=) Lude.<$> marker,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListKeys where
  toPath = Lude.const "/"

instance Lude.ToQuery ListKeys where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListKeysResponse' smart constructor.
data ListKeysResponse = ListKeysResponse'
  { truncated ::
      Lude.Maybe Lude.Bool,
    keys :: Lude.Maybe [KeyListEntry],
    nextMarker :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListKeysResponse' with the minimum fields required to make a request.
--
-- * 'keys' - A list of customer master keys (CMKs).
-- * 'nextMarker' - When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
-- * 'responseStatus' - The response status code.
-- * 'truncated' - A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
mkListKeysResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListKeysResponse
mkListKeysResponse pResponseStatus_ =
  ListKeysResponse'
    { truncated = Lude.Nothing,
      keys = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
--
-- /Note:/ Consider using 'truncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkrsTruncated :: Lens.Lens' ListKeysResponse (Lude.Maybe Lude.Bool)
lkrsTruncated = Lens.lens (truncated :: ListKeysResponse -> Lude.Maybe Lude.Bool) (\s a -> s {truncated = a} :: ListKeysResponse)
{-# DEPRECATED lkrsTruncated "Use generic-lens or generic-optics with 'truncated' instead." #-}

-- | A list of customer master keys (CMKs).
--
-- /Note:/ Consider using 'keys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkrsKeys :: Lens.Lens' ListKeysResponse (Lude.Maybe [KeyListEntry])
lkrsKeys = Lens.lens (keys :: ListKeysResponse -> Lude.Maybe [KeyListEntry]) (\s a -> s {keys = a} :: ListKeysResponse)
{-# DEPRECATED lkrsKeys "Use generic-lens or generic-optics with 'keys' instead." #-}

-- | When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkrsNextMarker :: Lens.Lens' ListKeysResponse (Lude.Maybe Lude.Text)
lkrsNextMarker = Lens.lens (nextMarker :: ListKeysResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListKeysResponse)
{-# DEPRECATED lkrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lkrsResponseStatus :: Lens.Lens' ListKeysResponse Lude.Int
lkrsResponseStatus = Lens.lens (responseStatus :: ListKeysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListKeysResponse)
{-# DEPRECATED lkrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
