{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.ListFunctionEventInvokeConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of configurations for asynchronous invocation for a function.
--
-- To configure options for asynchronous invocation, use 'PutFunctionEventInvokeConfig' .
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListFunctionEventInvokeConfigs
  ( -- * Creating a request
    ListFunctionEventInvokeConfigs (..),
    mkListFunctionEventInvokeConfigs,

    -- ** Request lenses
    lfeicMarker,
    lfeicMaxItems,
    lfeicFunctionName,

    -- * Destructuring the response
    ListFunctionEventInvokeConfigsResponse (..),
    mkListFunctionEventInvokeConfigsResponse,

    -- ** Response lenses
    lfeicrsFunctionEventInvokeConfigs,
    lfeicrsNextMarker,
    lfeicrsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListFunctionEventInvokeConfigs' smart constructor.
data ListFunctionEventInvokeConfigs = ListFunctionEventInvokeConfigs'
  { -- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of configurations to return.
    maxItems :: Lude.Maybe Lude.Natural,
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
    functionName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListFunctionEventInvokeConfigs' with the minimum fields required to make a request.
--
-- * 'marker' - Specify the pagination token that's returned by a previous request to retrieve the next page of results.
-- * 'maxItems' - The maximum number of configurations to return.
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
mkListFunctionEventInvokeConfigs ::
  -- | 'functionName'
  Lude.Text ->
  ListFunctionEventInvokeConfigs
mkListFunctionEventInvokeConfigs pFunctionName_ =
  ListFunctionEventInvokeConfigs'
    { marker = Lude.Nothing,
      maxItems = Lude.Nothing,
      functionName = pFunctionName_
    }

-- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfeicMarker :: Lens.Lens' ListFunctionEventInvokeConfigs (Lude.Maybe Lude.Text)
lfeicMarker = Lens.lens (marker :: ListFunctionEventInvokeConfigs -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListFunctionEventInvokeConfigs)
{-# DEPRECATED lfeicMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of configurations to return.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfeicMaxItems :: Lens.Lens' ListFunctionEventInvokeConfigs (Lude.Maybe Lude.Natural)
lfeicMaxItems = Lens.lens (maxItems :: ListFunctionEventInvokeConfigs -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListFunctionEventInvokeConfigs)
{-# DEPRECATED lfeicMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

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
lfeicFunctionName :: Lens.Lens' ListFunctionEventInvokeConfigs Lude.Text
lfeicFunctionName = Lens.lens (functionName :: ListFunctionEventInvokeConfigs -> Lude.Text) (\s a -> s {functionName = a} :: ListFunctionEventInvokeConfigs)
{-# DEPRECATED lfeicFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

instance Page.AWSPager ListFunctionEventInvokeConfigs where
  page rq rs
    | Page.stop (rs Lens.^. lfeicrsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lfeicrsFunctionEventInvokeConfigs) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lfeicMarker Lens..~ rs Lens.^. lfeicrsNextMarker

instance Lude.AWSRequest ListFunctionEventInvokeConfigs where
  type
    Rs ListFunctionEventInvokeConfigs =
      ListFunctionEventInvokeConfigsResponse
  request = Req.get lambdaService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListFunctionEventInvokeConfigsResponse'
            Lude.<$> (x Lude..?> "FunctionEventInvokeConfigs" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListFunctionEventInvokeConfigs where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListFunctionEventInvokeConfigs where
  toPath ListFunctionEventInvokeConfigs' {..} =
    Lude.mconcat
      [ "/2019-09-25/functions/",
        Lude.toBS functionName,
        "/event-invoke-config/list"
      ]

instance Lude.ToQuery ListFunctionEventInvokeConfigs where
  toQuery ListFunctionEventInvokeConfigs' {..} =
    Lude.mconcat
      ["Marker" Lude.=: marker, "MaxItems" Lude.=: maxItems]

-- | /See:/ 'mkListFunctionEventInvokeConfigsResponse' smart constructor.
data ListFunctionEventInvokeConfigsResponse = ListFunctionEventInvokeConfigsResponse'
  { -- | A list of configurations.
    functionEventInvokeConfigs :: Lude.Maybe [FunctionEventInvokeConfig],
    -- | The pagination token that's included if more results are available.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListFunctionEventInvokeConfigsResponse' with the minimum fields required to make a request.
--
-- * 'functionEventInvokeConfigs' - A list of configurations.
-- * 'nextMarker' - The pagination token that's included if more results are available.
-- * 'responseStatus' - The response status code.
mkListFunctionEventInvokeConfigsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListFunctionEventInvokeConfigsResponse
mkListFunctionEventInvokeConfigsResponse pResponseStatus_ =
  ListFunctionEventInvokeConfigsResponse'
    { functionEventInvokeConfigs =
        Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of configurations.
--
-- /Note:/ Consider using 'functionEventInvokeConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfeicrsFunctionEventInvokeConfigs :: Lens.Lens' ListFunctionEventInvokeConfigsResponse (Lude.Maybe [FunctionEventInvokeConfig])
lfeicrsFunctionEventInvokeConfigs = Lens.lens (functionEventInvokeConfigs :: ListFunctionEventInvokeConfigsResponse -> Lude.Maybe [FunctionEventInvokeConfig]) (\s a -> s {functionEventInvokeConfigs = a} :: ListFunctionEventInvokeConfigsResponse)
{-# DEPRECATED lfeicrsFunctionEventInvokeConfigs "Use generic-lens or generic-optics with 'functionEventInvokeConfigs' instead." #-}

-- | The pagination token that's included if more results are available.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfeicrsNextMarker :: Lens.Lens' ListFunctionEventInvokeConfigsResponse (Lude.Maybe Lude.Text)
lfeicrsNextMarker = Lens.lens (nextMarker :: ListFunctionEventInvokeConfigsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListFunctionEventInvokeConfigsResponse)
{-# DEPRECATED lfeicrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfeicrsResponseStatus :: Lens.Lens' ListFunctionEventInvokeConfigsResponse Lude.Int
lfeicrsResponseStatus = Lens.lens (responseStatus :: ListFunctionEventInvokeConfigsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListFunctionEventInvokeConfigsResponse)
{-# DEPRECATED lfeicrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
