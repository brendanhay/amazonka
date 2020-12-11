{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.GetTagKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all tag keys in the specified Region for the AWS account.
--
-- This operation returns paginated results.
module Network.AWS.ResourceGroupsTagging.GetTagKeys
  ( -- * Creating a request
    GetTagKeys (..),
    mkGetTagKeys,

    -- ** Request lenses
    gtkPaginationToken,

    -- * Destructuring the response
    GetTagKeysResponse (..),
    mkGetTagKeysResponse,

    -- ** Response lenses
    gtkrsPaginationToken,
    gtkrsTagKeys,
    gtkrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import Network.AWS.ResourceGroupsTagging.Types
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetTagKeys' smart constructor.
newtype GetTagKeys = GetTagKeys'
  { paginationToken ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTagKeys' with the minimum fields required to make a request.
--
-- * 'paginationToken' - A string that indicates that additional data is available. Leave this value empty for your initial request. If the response includes a @PaginationToken@ , use that string for this value to request an additional page of data.
mkGetTagKeys ::
  GetTagKeys
mkGetTagKeys = GetTagKeys' {paginationToken = Lude.Nothing}

-- | A string that indicates that additional data is available. Leave this value empty for your initial request. If the response includes a @PaginationToken@ , use that string for this value to request an additional page of data.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtkPaginationToken :: Lens.Lens' GetTagKeys (Lude.Maybe Lude.Text)
gtkPaginationToken = Lens.lens (paginationToken :: GetTagKeys -> Lude.Maybe Lude.Text) (\s a -> s {paginationToken = a} :: GetTagKeys)
{-# DEPRECATED gtkPaginationToken "Use generic-lens or generic-optics with 'paginationToken' instead." #-}

instance Page.AWSPager GetTagKeys where
  page rq rs
    | Page.stop (rs Lens.^. gtkrsPaginationToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gtkrsTagKeys) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gtkPaginationToken Lens..~ rs Lens.^. gtkrsPaginationToken

instance Lude.AWSRequest GetTagKeys where
  type Rs GetTagKeys = GetTagKeysResponse
  request = Req.postJSON resourceGroupsTaggingService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTagKeysResponse'
            Lude.<$> (x Lude..?> "PaginationToken")
            Lude.<*> (x Lude..?> "TagKeys" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTagKeys where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "ResourceGroupsTaggingAPI_20170126.GetTagKeys" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetTagKeys where
  toJSON GetTagKeys' {..} =
    Lude.object
      ( Lude.catMaybes
          [("PaginationToken" Lude..=) Lude.<$> paginationToken]
      )

instance Lude.ToPath GetTagKeys where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTagKeys where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetTagKeysResponse' smart constructor.
data GetTagKeysResponse = GetTagKeysResponse'
  { paginationToken ::
      Lude.Maybe Lude.Text,
    tagKeys :: Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'GetTagKeysResponse' with the minimum fields required to make a request.
--
-- * 'paginationToken' - A string that indicates that the response contains more data than can be returned in a single response. To receive additional data, specify this string for the @PaginationToken@ value in a subsequent request.
-- * 'responseStatus' - The response status code.
-- * 'tagKeys' - A list of all tag keys in the AWS account.
mkGetTagKeysResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTagKeysResponse
mkGetTagKeysResponse pResponseStatus_ =
  GetTagKeysResponse'
    { paginationToken = Lude.Nothing,
      tagKeys = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A string that indicates that the response contains more data than can be returned in a single response. To receive additional data, specify this string for the @PaginationToken@ value in a subsequent request.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtkrsPaginationToken :: Lens.Lens' GetTagKeysResponse (Lude.Maybe Lude.Text)
gtkrsPaginationToken = Lens.lens (paginationToken :: GetTagKeysResponse -> Lude.Maybe Lude.Text) (\s a -> s {paginationToken = a} :: GetTagKeysResponse)
{-# DEPRECATED gtkrsPaginationToken "Use generic-lens or generic-optics with 'paginationToken' instead." #-}

-- | A list of all tag keys in the AWS account.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtkrsTagKeys :: Lens.Lens' GetTagKeysResponse (Lude.Maybe [Lude.Text])
gtkrsTagKeys = Lens.lens (tagKeys :: GetTagKeysResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {tagKeys = a} :: GetTagKeysResponse)
{-# DEPRECATED gtkrsTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtkrsResponseStatus :: Lens.Lens' GetTagKeysResponse Lude.Int
gtkrsResponseStatus = Lens.lens (responseStatus :: GetTagKeysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTagKeysResponse)
{-# DEPRECATED gtkrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
