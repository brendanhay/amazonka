{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.ListPresets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a JSON array of up to twenty of your presets. This will return the presets themselves, not just a list of them. To retrieve the next twenty presets, use the nextToken string returned with the array.
--
-- This operation returns paginated results.
module Network.AWS.MediaConvert.ListPresets
  ( -- * Creating a request
    ListPresets (..),
    mkListPresets,

    -- ** Request lenses
    lpCategory,
    lpListBy,
    lpNextToken,
    lpOrder,
    lpMaxResults,

    -- * Destructuring the response
    ListPresetsResponse (..),
    mkListPresetsResponse,

    -- ** Response lenses
    lprsPresets,
    lprsNextToken,
    lprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListPresets' smart constructor.
data ListPresets = ListPresets'
  { category :: Lude.Maybe Lude.Text,
    listBy :: Lude.Maybe PresetListBy,
    nextToken :: Lude.Maybe Lude.Text,
    order :: Lude.Maybe Order,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPresets' with the minimum fields required to make a request.
--
-- * 'category' - Optionally, specify a preset category to limit responses to only presets from that category.
-- * 'listBy' - Optional. When you request a list of presets, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by name.
-- * 'maxResults' - Optional. Number of presets, up to twenty, that will be returned at one time
-- * 'nextToken' - Use this string, provided with the response to a previous request, to request the next batch of presets.
-- * 'order' - Optional. When you request lists of resources, you can specify whether they are sorted in ASCENDING or DESCENDING order. Default varies by resource.
mkListPresets ::
  ListPresets
mkListPresets =
  ListPresets'
    { category = Lude.Nothing,
      listBy = Lude.Nothing,
      nextToken = Lude.Nothing,
      order = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Optionally, specify a preset category to limit responses to only presets from that category.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpCategory :: Lens.Lens' ListPresets (Lude.Maybe Lude.Text)
lpCategory = Lens.lens (category :: ListPresets -> Lude.Maybe Lude.Text) (\s a -> s {category = a} :: ListPresets)
{-# DEPRECATED lpCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | Optional. When you request a list of presets, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by name.
--
-- /Note:/ Consider using 'listBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpListBy :: Lens.Lens' ListPresets (Lude.Maybe PresetListBy)
lpListBy = Lens.lens (listBy :: ListPresets -> Lude.Maybe PresetListBy) (\s a -> s {listBy = a} :: ListPresets)
{-# DEPRECATED lpListBy "Use generic-lens or generic-optics with 'listBy' instead." #-}

-- | Use this string, provided with the response to a previous request, to request the next batch of presets.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpNextToken :: Lens.Lens' ListPresets (Lude.Maybe Lude.Text)
lpNextToken = Lens.lens (nextToken :: ListPresets -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPresets)
{-# DEPRECATED lpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Optional. When you request lists of resources, you can specify whether they are sorted in ASCENDING or DESCENDING order. Default varies by resource.
--
-- /Note:/ Consider using 'order' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpOrder :: Lens.Lens' ListPresets (Lude.Maybe Order)
lpOrder = Lens.lens (order :: ListPresets -> Lude.Maybe Order) (\s a -> s {order = a} :: ListPresets)
{-# DEPRECATED lpOrder "Use generic-lens or generic-optics with 'order' instead." #-}

-- | Optional. Number of presets, up to twenty, that will be returned at one time
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMaxResults :: Lens.Lens' ListPresets (Lude.Maybe Lude.Natural)
lpMaxResults = Lens.lens (maxResults :: ListPresets -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListPresets)
{-# DEPRECATED lpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListPresets where
  page rq rs
    | Page.stop (rs Lens.^. lprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lprsPresets) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpNextToken Lens..~ rs Lens.^. lprsNextToken

instance Lude.AWSRequest ListPresets where
  type Rs ListPresets = ListPresetsResponse
  request = Req.get mediaConvertService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPresetsResponse'
            Lude.<$> (x Lude..?> "presets" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPresets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListPresets where
  toPath = Lude.const "/2017-08-29/presets"

instance Lude.ToQuery ListPresets where
  toQuery ListPresets' {..} =
    Lude.mconcat
      [ "category" Lude.=: category,
        "listBy" Lude.=: listBy,
        "nextToken" Lude.=: nextToken,
        "order" Lude.=: order,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListPresetsResponse' smart constructor.
data ListPresetsResponse = ListPresetsResponse'
  { presets ::
      Lude.Maybe [Preset],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListPresetsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Use this string to request the next batch of presets.
-- * 'presets' - List of presets
-- * 'responseStatus' - The response status code.
mkListPresetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPresetsResponse
mkListPresetsResponse pResponseStatus_ =
  ListPresetsResponse'
    { presets = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | List of presets
--
-- /Note:/ Consider using 'presets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsPresets :: Lens.Lens' ListPresetsResponse (Lude.Maybe [Preset])
lprsPresets = Lens.lens (presets :: ListPresetsResponse -> Lude.Maybe [Preset]) (\s a -> s {presets = a} :: ListPresetsResponse)
{-# DEPRECATED lprsPresets "Use generic-lens or generic-optics with 'presets' instead." #-}

-- | Use this string to request the next batch of presets.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsNextToken :: Lens.Lens' ListPresetsResponse (Lude.Maybe Lude.Text)
lprsNextToken = Lens.lens (nextToken :: ListPresetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPresetsResponse)
{-# DEPRECATED lprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsResponseStatus :: Lens.Lens' ListPresetsResponse Lude.Int
lprsResponseStatus = Lens.lens (responseStatus :: ListPresetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPresetsResponse)
{-# DEPRECATED lprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
