{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.ListFunctionsByCodeSigningConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the functions that use the specified code signing configuration. You can use this method prior to deleting a code signing configuration, to verify that no functions are using it.
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListFunctionsByCodeSigningConfig
  ( -- * Creating a request
    ListFunctionsByCodeSigningConfig (..),
    mkListFunctionsByCodeSigningConfig,

    -- ** Request lenses
    lfbcscCodeSigningConfigARN,
    lfbcscMarker,
    lfbcscMaxItems,

    -- * Destructuring the response
    ListFunctionsByCodeSigningConfigResponse (..),
    mkListFunctionsByCodeSigningConfigResponse,

    -- ** Response lenses
    lfbcscrsFunctionARNs,
    lfbcscrsNextMarker,
    lfbcscrsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListFunctionsByCodeSigningConfig' smart constructor.
data ListFunctionsByCodeSigningConfig = ListFunctionsByCodeSigningConfig'
  { -- | The The Amazon Resource Name (ARN) of the code signing configuration.
    codeSigningConfigARN :: Lude.Text,
    -- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
    marker :: Lude.Maybe Lude.Text,
    -- | Maximum number of items to return.
    maxItems :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListFunctionsByCodeSigningConfig' with the minimum fields required to make a request.
--
-- * 'codeSigningConfigARN' - The The Amazon Resource Name (ARN) of the code signing configuration.
-- * 'marker' - Specify the pagination token that's returned by a previous request to retrieve the next page of results.
-- * 'maxItems' - Maximum number of items to return.
mkListFunctionsByCodeSigningConfig ::
  -- | 'codeSigningConfigARN'
  Lude.Text ->
  ListFunctionsByCodeSigningConfig
mkListFunctionsByCodeSigningConfig pCodeSigningConfigARN_ =
  ListFunctionsByCodeSigningConfig'
    { codeSigningConfigARN =
        pCodeSigningConfigARN_,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | The The Amazon Resource Name (ARN) of the code signing configuration.
--
-- /Note:/ Consider using 'codeSigningConfigARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfbcscCodeSigningConfigARN :: Lens.Lens' ListFunctionsByCodeSigningConfig Lude.Text
lfbcscCodeSigningConfigARN = Lens.lens (codeSigningConfigARN :: ListFunctionsByCodeSigningConfig -> Lude.Text) (\s a -> s {codeSigningConfigARN = a} :: ListFunctionsByCodeSigningConfig)
{-# DEPRECATED lfbcscCodeSigningConfigARN "Use generic-lens or generic-optics with 'codeSigningConfigARN' instead." #-}

-- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfbcscMarker :: Lens.Lens' ListFunctionsByCodeSigningConfig (Lude.Maybe Lude.Text)
lfbcscMarker = Lens.lens (marker :: ListFunctionsByCodeSigningConfig -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListFunctionsByCodeSigningConfig)
{-# DEPRECATED lfbcscMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Maximum number of items to return.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfbcscMaxItems :: Lens.Lens' ListFunctionsByCodeSigningConfig (Lude.Maybe Lude.Natural)
lfbcscMaxItems = Lens.lens (maxItems :: ListFunctionsByCodeSigningConfig -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListFunctionsByCodeSigningConfig)
{-# DEPRECATED lfbcscMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListFunctionsByCodeSigningConfig where
  page rq rs
    | Page.stop (rs Lens.^. lfbcscrsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lfbcscrsFunctionARNs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lfbcscMarker Lens..~ rs Lens.^. lfbcscrsNextMarker

instance Lude.AWSRequest ListFunctionsByCodeSigningConfig where
  type
    Rs ListFunctionsByCodeSigningConfig =
      ListFunctionsByCodeSigningConfigResponse
  request = Req.get lambdaService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListFunctionsByCodeSigningConfigResponse'
            Lude.<$> (x Lude..?> "FunctionArns" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListFunctionsByCodeSigningConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListFunctionsByCodeSigningConfig where
  toPath ListFunctionsByCodeSigningConfig' {..} =
    Lude.mconcat
      [ "/2020-04-22/code-signing-configs/",
        Lude.toBS codeSigningConfigARN,
        "/functions"
      ]

instance Lude.ToQuery ListFunctionsByCodeSigningConfig where
  toQuery ListFunctionsByCodeSigningConfig' {..} =
    Lude.mconcat
      ["Marker" Lude.=: marker, "MaxItems" Lude.=: maxItems]

-- | /See:/ 'mkListFunctionsByCodeSigningConfigResponse' smart constructor.
data ListFunctionsByCodeSigningConfigResponse = ListFunctionsByCodeSigningConfigResponse'
  { -- | The function ARNs.
    functionARNs :: Lude.Maybe [Lude.Text],
    -- | The pagination token that's included if more results are available.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListFunctionsByCodeSigningConfigResponse' with the minimum fields required to make a request.
--
-- * 'functionARNs' - The function ARNs.
-- * 'nextMarker' - The pagination token that's included if more results are available.
-- * 'responseStatus' - The response status code.
mkListFunctionsByCodeSigningConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListFunctionsByCodeSigningConfigResponse
mkListFunctionsByCodeSigningConfigResponse pResponseStatus_ =
  ListFunctionsByCodeSigningConfigResponse'
    { functionARNs =
        Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The function ARNs.
--
-- /Note:/ Consider using 'functionARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfbcscrsFunctionARNs :: Lens.Lens' ListFunctionsByCodeSigningConfigResponse (Lude.Maybe [Lude.Text])
lfbcscrsFunctionARNs = Lens.lens (functionARNs :: ListFunctionsByCodeSigningConfigResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {functionARNs = a} :: ListFunctionsByCodeSigningConfigResponse)
{-# DEPRECATED lfbcscrsFunctionARNs "Use generic-lens or generic-optics with 'functionARNs' instead." #-}

-- | The pagination token that's included if more results are available.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfbcscrsNextMarker :: Lens.Lens' ListFunctionsByCodeSigningConfigResponse (Lude.Maybe Lude.Text)
lfbcscrsNextMarker = Lens.lens (nextMarker :: ListFunctionsByCodeSigningConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListFunctionsByCodeSigningConfigResponse)
{-# DEPRECATED lfbcscrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfbcscrsResponseStatus :: Lens.Lens' ListFunctionsByCodeSigningConfigResponse Lude.Int
lfbcscrsResponseStatus = Lens.lens (responseStatus :: ListFunctionsByCodeSigningConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListFunctionsByCodeSigningConfigResponse)
{-# DEPRECATED lfbcscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
