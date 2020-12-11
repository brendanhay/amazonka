{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.ListPresets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ListPresets operation gets a list of the default presets included with Elastic Transcoder and the presets that you've added in an AWS region.
--
-- This operation returns paginated results.
module Network.AWS.ElasticTranscoder.ListPresets
  ( -- * Creating a request
    ListPresets (..),
    mkListPresets,

    -- ** Request lenses
    lAscending,
    lPageToken,

    -- * Destructuring the response
    ListPresetsResponse (..),
    mkListPresetsResponse,

    -- ** Response lenses
    lrsNextPageToken,
    lrsPresets,
    lrsResponseStatus,
  )
where

import Network.AWS.ElasticTranscoder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The @ListPresetsRequest@ structure.
--
-- /See:/ 'mkListPresets' smart constructor.
data ListPresets = ListPresets'
  { ascending :: Lude.Maybe Lude.Text,
    pageToken :: Lude.Maybe Lude.Text
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
-- * 'ascending' - To list presets in chronological order by the date and time that they were created, enter @true@ . To list presets in reverse chronological order, enter @false@ .
-- * 'pageToken' - When Elastic Transcoder returns more than one page of results, use @pageToken@ in subsequent @GET@ requests to get each successive page of results.
mkListPresets ::
  ListPresets
mkListPresets =
  ListPresets' {ascending = Lude.Nothing, pageToken = Lude.Nothing}

-- | To list presets in chronological order by the date and time that they were created, enter @true@ . To list presets in reverse chronological order, enter @false@ .
--
-- /Note:/ Consider using 'ascending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lAscending :: Lens.Lens' ListPresets (Lude.Maybe Lude.Text)
lAscending = Lens.lens (ascending :: ListPresets -> Lude.Maybe Lude.Text) (\s a -> s {ascending = a} :: ListPresets)
{-# DEPRECATED lAscending "Use generic-lens or generic-optics with 'ascending' instead." #-}

-- | When Elastic Transcoder returns more than one page of results, use @pageToken@ in subsequent @GET@ requests to get each successive page of results.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lPageToken :: Lens.Lens' ListPresets (Lude.Maybe Lude.Text)
lPageToken = Lens.lens (pageToken :: ListPresets -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: ListPresets)
{-# DEPRECATED lPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Page.AWSPager ListPresets where
  page rq rs
    | Page.stop (rs Lens.^. lrsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrsPresets) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lPageToken Lens..~ rs Lens.^. lrsNextPageToken

instance Lude.AWSRequest ListPresets where
  type Rs ListPresets = ListPresetsResponse
  request = Req.get elasticTranscoderService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPresetsResponse'
            Lude.<$> (x Lude..?> "NextPageToken")
            Lude.<*> (x Lude..?> "Presets" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPresets where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListPresets where
  toPath = Lude.const "/2012-09-25/presets"

instance Lude.ToQuery ListPresets where
  toQuery ListPresets' {..} =
    Lude.mconcat
      ["Ascending" Lude.=: ascending, "PageToken" Lude.=: pageToken]

-- | The @ListPresetsResponse@ structure.
--
-- /See:/ 'mkListPresetsResponse' smart constructor.
data ListPresetsResponse = ListPresetsResponse'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    presets :: Lude.Maybe [Preset],
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
-- * 'nextPageToken' - A value that you use to access the second and subsequent pages of results, if any. When the presets fit on one page or when you've reached the last page of results, the value of @NextPageToken@ is @null@ .
-- * 'presets' - An array of @Preset@ objects.
-- * 'responseStatus' - The response status code.
mkListPresetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPresetsResponse
mkListPresetsResponse pResponseStatus_ =
  ListPresetsResponse'
    { nextPageToken = Lude.Nothing,
      presets = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A value that you use to access the second and subsequent pages of results, if any. When the presets fit on one page or when you've reached the last page of results, the value of @NextPageToken@ is @null@ .
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextPageToken :: Lens.Lens' ListPresetsResponse (Lude.Maybe Lude.Text)
lrsNextPageToken = Lens.lens (nextPageToken :: ListPresetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: ListPresetsResponse)
{-# DEPRECATED lrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An array of @Preset@ objects.
--
-- /Note:/ Consider using 'presets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsPresets :: Lens.Lens' ListPresetsResponse (Lude.Maybe [Preset])
lrsPresets = Lens.lens (presets :: ListPresetsResponse -> Lude.Maybe [Preset]) (\s a -> s {presets = a} :: ListPresetsResponse)
{-# DEPRECATED lrsPresets "Use generic-lens or generic-optics with 'presets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListPresetsResponse Lude.Int
lrsResponseStatus = Lens.lens (responseStatus :: ListPresetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPresetsResponse)
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
