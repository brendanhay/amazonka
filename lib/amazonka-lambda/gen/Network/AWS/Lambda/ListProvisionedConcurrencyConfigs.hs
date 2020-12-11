{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.ListProvisionedConcurrencyConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of provisioned concurrency configurations for a function.
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListProvisionedConcurrencyConfigs
  ( -- * Creating a request
    ListProvisionedConcurrencyConfigs (..),
    mkListProvisionedConcurrencyConfigs,

    -- ** Request lenses
    lpccMarker,
    lpccMaxItems,
    lpccFunctionName,

    -- * Destructuring the response
    ListProvisionedConcurrencyConfigsResponse (..),
    mkListProvisionedConcurrencyConfigsResponse,

    -- ** Response lenses
    lpccrsProvisionedConcurrencyConfigs,
    lpccrsNextMarker,
    lpccrsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListProvisionedConcurrencyConfigs' smart constructor.
data ListProvisionedConcurrencyConfigs = ListProvisionedConcurrencyConfigs'
  { marker ::
      Lude.Maybe Lude.Text,
    maxItems ::
      Lude.Maybe Lude.Natural,
    functionName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListProvisionedConcurrencyConfigs' with the minimum fields required to make a request.
--
-- * 'functionName' - The name of the Lambda function.
--
-- __Name formats__
--
--     * __Function name__ - @my-function@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
-- * 'marker' - Specify the pagination token that's returned by a previous request to retrieve the next page of results.
-- * 'maxItems' - Specify a number to limit the number of configurations returned.
mkListProvisionedConcurrencyConfigs ::
  -- | 'functionName'
  Lude.Text ->
  ListProvisionedConcurrencyConfigs
mkListProvisionedConcurrencyConfigs pFunctionName_ =
  ListProvisionedConcurrencyConfigs'
    { marker = Lude.Nothing,
      maxItems = Lude.Nothing,
      functionName = pFunctionName_
    }

-- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpccMarker :: Lens.Lens' ListProvisionedConcurrencyConfigs (Lude.Maybe Lude.Text)
lpccMarker = Lens.lens (marker :: ListProvisionedConcurrencyConfigs -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListProvisionedConcurrencyConfigs)
{-# DEPRECATED lpccMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Specify a number to limit the number of configurations returned.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpccMaxItems :: Lens.Lens' ListProvisionedConcurrencyConfigs (Lude.Maybe Lude.Natural)
lpccMaxItems = Lens.lens (maxItems :: ListProvisionedConcurrencyConfigs -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListProvisionedConcurrencyConfigs)
{-# DEPRECATED lpccMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The name of the Lambda function.
--
-- __Name formats__
--
--     * __Function name__ - @my-function@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpccFunctionName :: Lens.Lens' ListProvisionedConcurrencyConfigs Lude.Text
lpccFunctionName = Lens.lens (functionName :: ListProvisionedConcurrencyConfigs -> Lude.Text) (\s a -> s {functionName = a} :: ListProvisionedConcurrencyConfigs)
{-# DEPRECATED lpccFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

instance Page.AWSPager ListProvisionedConcurrencyConfigs where
  page rq rs
    | Page.stop (rs Lens.^. lpccrsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lpccrsProvisionedConcurrencyConfigs) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpccMarker Lens..~ rs Lens.^. lpccrsNextMarker

instance Lude.AWSRequest ListProvisionedConcurrencyConfigs where
  type
    Rs ListProvisionedConcurrencyConfigs =
      ListProvisionedConcurrencyConfigsResponse
  request = Req.get lambdaService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListProvisionedConcurrencyConfigsResponse'
            Lude.<$> (x Lude..?> "ProvisionedConcurrencyConfigs" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListProvisionedConcurrencyConfigs where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListProvisionedConcurrencyConfigs where
  toPath ListProvisionedConcurrencyConfigs' {..} =
    Lude.mconcat
      [ "/2019-09-30/functions/",
        Lude.toBS functionName,
        "/provisioned-concurrency"
      ]

instance Lude.ToQuery ListProvisionedConcurrencyConfigs where
  toQuery ListProvisionedConcurrencyConfigs' {..} =
    Lude.mconcat
      ["Marker" Lude.=: marker, "MaxItems" Lude.=: maxItems, "List=ALL"]

-- | /See:/ 'mkListProvisionedConcurrencyConfigsResponse' smart constructor.
data ListProvisionedConcurrencyConfigsResponse = ListProvisionedConcurrencyConfigsResponse'
  { provisionedConcurrencyConfigs ::
      Lude.Maybe
        [ProvisionedConcurrencyConfigListItem],
    nextMarker ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListProvisionedConcurrencyConfigsResponse' with the minimum fields required to make a request.
--
-- * 'nextMarker' - The pagination token that's included if more results are available.
-- * 'provisionedConcurrencyConfigs' - A list of provisioned concurrency configurations.
-- * 'responseStatus' - The response status code.
mkListProvisionedConcurrencyConfigsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListProvisionedConcurrencyConfigsResponse
mkListProvisionedConcurrencyConfigsResponse pResponseStatus_ =
  ListProvisionedConcurrencyConfigsResponse'
    { provisionedConcurrencyConfigs =
        Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of provisioned concurrency configurations.
--
-- /Note:/ Consider using 'provisionedConcurrencyConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpccrsProvisionedConcurrencyConfigs :: Lens.Lens' ListProvisionedConcurrencyConfigsResponse (Lude.Maybe [ProvisionedConcurrencyConfigListItem])
lpccrsProvisionedConcurrencyConfigs = Lens.lens (provisionedConcurrencyConfigs :: ListProvisionedConcurrencyConfigsResponse -> Lude.Maybe [ProvisionedConcurrencyConfigListItem]) (\s a -> s {provisionedConcurrencyConfigs = a} :: ListProvisionedConcurrencyConfigsResponse)
{-# DEPRECATED lpccrsProvisionedConcurrencyConfigs "Use generic-lens or generic-optics with 'provisionedConcurrencyConfigs' instead." #-}

-- | The pagination token that's included if more results are available.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpccrsNextMarker :: Lens.Lens' ListProvisionedConcurrencyConfigsResponse (Lude.Maybe Lude.Text)
lpccrsNextMarker = Lens.lens (nextMarker :: ListProvisionedConcurrencyConfigsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListProvisionedConcurrencyConfigsResponse)
{-# DEPRECATED lpccrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpccrsResponseStatus :: Lens.Lens' ListProvisionedConcurrencyConfigsResponse Lude.Int
lpccrsResponseStatus = Lens.lens (responseStatus :: ListProvisionedConcurrencyConfigsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListProvisionedConcurrencyConfigsResponse)
{-# DEPRECATED lpccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
