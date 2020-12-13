{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.ListCodeSigningConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of <https://docs.aws.amazon.com/lambda/latest/dg/configuring-codesigning.html code signing configurations> for the specified function. A request returns up to 10,000 configurations per call. You can use the @MaxItems@ parameter to return fewer configurations per call.
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListCodeSigningConfigs
  ( -- * Creating a request
    ListCodeSigningConfigs (..),
    mkListCodeSigningConfigs,

    -- ** Request lenses
    lcscMarker,
    lcscMaxItems,

    -- * Destructuring the response
    ListCodeSigningConfigsResponse (..),
    mkListCodeSigningConfigsResponse,

    -- ** Response lenses
    lcscrsCodeSigningConfigs,
    lcscrsNextMarker,
    lcscrsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListCodeSigningConfigs' smart constructor.
data ListCodeSigningConfigs = ListCodeSigningConfigs'
  { -- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
    marker :: Lude.Maybe Lude.Text,
    -- | Maximum number of items to return.
    maxItems :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCodeSigningConfigs' with the minimum fields required to make a request.
--
-- * 'marker' - Specify the pagination token that's returned by a previous request to retrieve the next page of results.
-- * 'maxItems' - Maximum number of items to return.
mkListCodeSigningConfigs ::
  ListCodeSigningConfigs
mkListCodeSigningConfigs =
  ListCodeSigningConfigs'
    { marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcscMarker :: Lens.Lens' ListCodeSigningConfigs (Lude.Maybe Lude.Text)
lcscMarker = Lens.lens (marker :: ListCodeSigningConfigs -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListCodeSigningConfigs)
{-# DEPRECATED lcscMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Maximum number of items to return.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcscMaxItems :: Lens.Lens' ListCodeSigningConfigs (Lude.Maybe Lude.Natural)
lcscMaxItems = Lens.lens (maxItems :: ListCodeSigningConfigs -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListCodeSigningConfigs)
{-# DEPRECATED lcscMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListCodeSigningConfigs where
  page rq rs
    | Page.stop (rs Lens.^. lcscrsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lcscrsCodeSigningConfigs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcscMarker Lens..~ rs Lens.^. lcscrsNextMarker

instance Lude.AWSRequest ListCodeSigningConfigs where
  type Rs ListCodeSigningConfigs = ListCodeSigningConfigsResponse
  request = Req.get lambdaService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListCodeSigningConfigsResponse'
            Lude.<$> (x Lude..?> "CodeSigningConfigs" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListCodeSigningConfigs where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListCodeSigningConfigs where
  toPath = Lude.const "/2020-04-22/code-signing-configs/"

instance Lude.ToQuery ListCodeSigningConfigs where
  toQuery ListCodeSigningConfigs' {..} =
    Lude.mconcat
      ["Marker" Lude.=: marker, "MaxItems" Lude.=: maxItems]

-- | /See:/ 'mkListCodeSigningConfigsResponse' smart constructor.
data ListCodeSigningConfigsResponse = ListCodeSigningConfigsResponse'
  { -- | The code signing configurations
    codeSigningConfigs :: Lude.Maybe [CodeSigningConfig],
    -- | The pagination token that's included if more results are available.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCodeSigningConfigsResponse' with the minimum fields required to make a request.
--
-- * 'codeSigningConfigs' - The code signing configurations
-- * 'nextMarker' - The pagination token that's included if more results are available.
-- * 'responseStatus' - The response status code.
mkListCodeSigningConfigsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListCodeSigningConfigsResponse
mkListCodeSigningConfigsResponse pResponseStatus_ =
  ListCodeSigningConfigsResponse'
    { codeSigningConfigs =
        Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The code signing configurations
--
-- /Note:/ Consider using 'codeSigningConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcscrsCodeSigningConfigs :: Lens.Lens' ListCodeSigningConfigsResponse (Lude.Maybe [CodeSigningConfig])
lcscrsCodeSigningConfigs = Lens.lens (codeSigningConfigs :: ListCodeSigningConfigsResponse -> Lude.Maybe [CodeSigningConfig]) (\s a -> s {codeSigningConfigs = a} :: ListCodeSigningConfigsResponse)
{-# DEPRECATED lcscrsCodeSigningConfigs "Use generic-lens or generic-optics with 'codeSigningConfigs' instead." #-}

-- | The pagination token that's included if more results are available.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcscrsNextMarker :: Lens.Lens' ListCodeSigningConfigsResponse (Lude.Maybe Lude.Text)
lcscrsNextMarker = Lens.lens (nextMarker :: ListCodeSigningConfigsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListCodeSigningConfigsResponse)
{-# DEPRECATED lcscrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcscrsResponseStatus :: Lens.Lens' ListCodeSigningConfigsResponse Lude.Int
lcscrsResponseStatus = Lens.lens (responseStatus :: ListCodeSigningConfigsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListCodeSigningConfigsResponse)
{-# DEPRECATED lcscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
