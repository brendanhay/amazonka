{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.GetTagValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all tag values for the specified key in the specified Region for the AWS account.
--
-- This operation returns paginated results.
module Network.AWS.ResourceGroupsTagging.GetTagValues
  ( -- * Creating a request
    GetTagValues (..),
    mkGetTagValues,

    -- ** Request lenses
    gtvPaginationToken,
    gtvKey,

    -- * Destructuring the response
    GetTagValuesResponse (..),
    mkGetTagValuesResponse,

    -- ** Response lenses
    gtvrsPaginationToken,
    gtvrsTagValues,
    gtvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import Network.AWS.ResourceGroupsTagging.Types
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetTagValues' smart constructor.
data GetTagValues = GetTagValues'
  { paginationToken ::
      Lude.Maybe Lude.Text,
    key :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTagValues' with the minimum fields required to make a request.
--
-- * 'key' - The key for which you want to list all existing values in the specified Region for the AWS account.
-- * 'paginationToken' - A string that indicates that additional data is available. Leave this value empty for your initial request. If the response includes a @PaginationToken@ , use that string for this value to request an additional page of data.
mkGetTagValues ::
  -- | 'key'
  Lude.Text ->
  GetTagValues
mkGetTagValues pKey_ =
  GetTagValues' {paginationToken = Lude.Nothing, key = pKey_}

-- | A string that indicates that additional data is available. Leave this value empty for your initial request. If the response includes a @PaginationToken@ , use that string for this value to request an additional page of data.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvPaginationToken :: Lens.Lens' GetTagValues (Lude.Maybe Lude.Text)
gtvPaginationToken = Lens.lens (paginationToken :: GetTagValues -> Lude.Maybe Lude.Text) (\s a -> s {paginationToken = a} :: GetTagValues)
{-# DEPRECATED gtvPaginationToken "Use generic-lens or generic-optics with 'paginationToken' instead." #-}

-- | The key for which you want to list all existing values in the specified Region for the AWS account.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvKey :: Lens.Lens' GetTagValues Lude.Text
gtvKey = Lens.lens (key :: GetTagValues -> Lude.Text) (\s a -> s {key = a} :: GetTagValues)
{-# DEPRECATED gtvKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Page.AWSPager GetTagValues where
  page rq rs
    | Page.stop (rs Lens.^. gtvrsPaginationToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gtvrsTagValues) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gtvPaginationToken Lens..~ rs Lens.^. gtvrsPaginationToken

instance Lude.AWSRequest GetTagValues where
  type Rs GetTagValues = GetTagValuesResponse
  request = Req.postJSON resourceGroupsTaggingService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTagValuesResponse'
            Lude.<$> (x Lude..?> "PaginationToken")
            Lude.<*> (x Lude..?> "TagValues" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTagValues where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "ResourceGroupsTaggingAPI_20170126.GetTagValues" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetTagValues where
  toJSON GetTagValues' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PaginationToken" Lude..=) Lude.<$> paginationToken,
            Lude.Just ("Key" Lude..= key)
          ]
      )

instance Lude.ToPath GetTagValues where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTagValues where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetTagValuesResponse' smart constructor.
data GetTagValuesResponse = GetTagValuesResponse'
  { paginationToken ::
      Lude.Maybe Lude.Text,
    tagValues :: Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'GetTagValuesResponse' with the minimum fields required to make a request.
--
-- * 'paginationToken' - A string that indicates that the response contains more data than can be returned in a single response. To receive additional data, specify this string for the @PaginationToken@ value in a subsequent request.
-- * 'responseStatus' - The response status code.
-- * 'tagValues' - A list of all tag values for the specified key in the AWS account.
mkGetTagValuesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTagValuesResponse
mkGetTagValuesResponse pResponseStatus_ =
  GetTagValuesResponse'
    { paginationToken = Lude.Nothing,
      tagValues = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A string that indicates that the response contains more data than can be returned in a single response. To receive additional data, specify this string for the @PaginationToken@ value in a subsequent request.
--
-- /Note:/ Consider using 'paginationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvrsPaginationToken :: Lens.Lens' GetTagValuesResponse (Lude.Maybe Lude.Text)
gtvrsPaginationToken = Lens.lens (paginationToken :: GetTagValuesResponse -> Lude.Maybe Lude.Text) (\s a -> s {paginationToken = a} :: GetTagValuesResponse)
{-# DEPRECATED gtvrsPaginationToken "Use generic-lens or generic-optics with 'paginationToken' instead." #-}

-- | A list of all tag values for the specified key in the AWS account.
--
-- /Note:/ Consider using 'tagValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvrsTagValues :: Lens.Lens' GetTagValuesResponse (Lude.Maybe [Lude.Text])
gtvrsTagValues = Lens.lens (tagValues :: GetTagValuesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {tagValues = a} :: GetTagValuesResponse)
{-# DEPRECATED gtvrsTagValues "Use generic-lens or generic-optics with 'tagValues' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtvrsResponseStatus :: Lens.Lens' GetTagValuesResponse Lude.Int
gtvrsResponseStatus = Lens.lens (responseStatus :: GetTagValuesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTagValuesResponse)
{-# DEPRECATED gtvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
