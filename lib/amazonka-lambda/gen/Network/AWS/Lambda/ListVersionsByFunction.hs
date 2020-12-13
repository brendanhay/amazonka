{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.ListVersionsByFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html versions> , with the version-specific configuration of each. Lambda returns up to 50 versions per call.
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListVersionsByFunction
  ( -- * Creating a request
    ListVersionsByFunction (..),
    mkListVersionsByFunction,

    -- ** Request lenses
    lvbfMarker,
    lvbfMaxItems,
    lvbfFunctionName,

    -- * Destructuring the response
    ListVersionsByFunctionResponse (..),
    mkListVersionsByFunctionResponse,

    -- ** Response lenses
    lvbfrsVersions,
    lvbfrsNextMarker,
    lvbfrsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListVersionsByFunction' smart constructor.
data ListVersionsByFunction = ListVersionsByFunction'
  { -- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of versions to return.
    maxItems :: Lude.Maybe Lude.Natural,
    -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    --     * __Function name__ - @MyFunction@ .
    --
    --
    --     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
    --
    --
    --     * __Partial ARN__ - @123456789012:function:MyFunction@ .
    --
    --
    -- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
    functionName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListVersionsByFunction' with the minimum fields required to make a request.
--
-- * 'marker' - Specify the pagination token that's returned by a previous request to retrieve the next page of results.
-- * 'maxItems' - The maximum number of versions to return.
-- * 'functionName' - The name of the Lambda function.
--
-- __Name formats__
--
--     * __Function name__ - @MyFunction@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
--
--
--     * __Partial ARN__ - @123456789012:function:MyFunction@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
mkListVersionsByFunction ::
  -- | 'functionName'
  Lude.Text ->
  ListVersionsByFunction
mkListVersionsByFunction pFunctionName_ =
  ListVersionsByFunction'
    { marker = Lude.Nothing,
      maxItems = Lude.Nothing,
      functionName = pFunctionName_
    }

-- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvbfMarker :: Lens.Lens' ListVersionsByFunction (Lude.Maybe Lude.Text)
lvbfMarker = Lens.lens (marker :: ListVersionsByFunction -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListVersionsByFunction)
{-# DEPRECATED lvbfMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of versions to return.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvbfMaxItems :: Lens.Lens' ListVersionsByFunction (Lude.Maybe Lude.Natural)
lvbfMaxItems = Lens.lens (maxItems :: ListVersionsByFunction -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListVersionsByFunction)
{-# DEPRECATED lvbfMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The name of the Lambda function.
--
-- __Name formats__
--
--     * __Function name__ - @MyFunction@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
--
--
--     * __Partial ARN__ - @123456789012:function:MyFunction@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvbfFunctionName :: Lens.Lens' ListVersionsByFunction Lude.Text
lvbfFunctionName = Lens.lens (functionName :: ListVersionsByFunction -> Lude.Text) (\s a -> s {functionName = a} :: ListVersionsByFunction)
{-# DEPRECATED lvbfFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

instance Page.AWSPager ListVersionsByFunction where
  page rq rs
    | Page.stop (rs Lens.^. lvbfrsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lvbfrsVersions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lvbfMarker Lens..~ rs Lens.^. lvbfrsNextMarker

instance Lude.AWSRequest ListVersionsByFunction where
  type Rs ListVersionsByFunction = ListVersionsByFunctionResponse
  request = Req.get lambdaService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListVersionsByFunctionResponse'
            Lude.<$> (x Lude..?> "Versions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListVersionsByFunction where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListVersionsByFunction where
  toPath ListVersionsByFunction' {..} =
    Lude.mconcat
      ["/2015-03-31/functions/", Lude.toBS functionName, "/versions"]

instance Lude.ToQuery ListVersionsByFunction where
  toQuery ListVersionsByFunction' {..} =
    Lude.mconcat
      ["Marker" Lude.=: marker, "MaxItems" Lude.=: maxItems]

-- | /See:/ 'mkListVersionsByFunctionResponse' smart constructor.
data ListVersionsByFunctionResponse = ListVersionsByFunctionResponse'
  { -- | A list of Lambda function versions.
    versions :: Lude.Maybe [FunctionConfiguration],
    -- | The pagination token that's included if more results are available.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListVersionsByFunctionResponse' with the minimum fields required to make a request.
--
-- * 'versions' - A list of Lambda function versions.
-- * 'nextMarker' - The pagination token that's included if more results are available.
-- * 'responseStatus' - The response status code.
mkListVersionsByFunctionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListVersionsByFunctionResponse
mkListVersionsByFunctionResponse pResponseStatus_ =
  ListVersionsByFunctionResponse'
    { versions = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of Lambda function versions.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvbfrsVersions :: Lens.Lens' ListVersionsByFunctionResponse (Lude.Maybe [FunctionConfiguration])
lvbfrsVersions = Lens.lens (versions :: ListVersionsByFunctionResponse -> Lude.Maybe [FunctionConfiguration]) (\s a -> s {versions = a} :: ListVersionsByFunctionResponse)
{-# DEPRECATED lvbfrsVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | The pagination token that's included if more results are available.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvbfrsNextMarker :: Lens.Lens' ListVersionsByFunctionResponse (Lude.Maybe Lude.Text)
lvbfrsNextMarker = Lens.lens (nextMarker :: ListVersionsByFunctionResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListVersionsByFunctionResponse)
{-# DEPRECATED lvbfrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvbfrsResponseStatus :: Lens.Lens' ListVersionsByFunctionResponse Lude.Int
lvbfrsResponseStatus = Lens.lens (responseStatus :: ListVersionsByFunctionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListVersionsByFunctionResponse)
{-# DEPRECATED lvbfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
