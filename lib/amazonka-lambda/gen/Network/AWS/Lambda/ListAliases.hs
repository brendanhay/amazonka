{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.ListAliases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html aliases> for a Lambda function.
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListAliases
  ( -- * Creating a request
    ListAliases (..),
    mkListAliases,

    -- ** Request lenses
    laMarker,
    laMaxItems,
    laFunctionName,
    laFunctionVersion,

    -- * Destructuring the response
    ListAliasesResponse (..),
    mkListAliasesResponse,

    -- ** Response lenses
    larsAliases,
    larsNextMarker,
    larsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAliases' smart constructor.
data ListAliases = ListAliases'
  { -- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
    marker :: Lude.Maybe Lude.Text,
    -- | Limit the number of aliases returned.
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
    functionName :: Lude.Text,
    -- | Specify a function version to only list aliases that invoke that version.
    functionVersion :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAliases' with the minimum fields required to make a request.
--
-- * 'marker' - Specify the pagination token that's returned by a previous request to retrieve the next page of results.
-- * 'maxItems' - Limit the number of aliases returned.
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
-- * 'functionVersion' - Specify a function version to only list aliases that invoke that version.
mkListAliases ::
  -- | 'functionName'
  Lude.Text ->
  ListAliases
mkListAliases pFunctionName_ =
  ListAliases'
    { marker = Lude.Nothing,
      maxItems = Lude.Nothing,
      functionName = pFunctionName_,
      functionVersion = Lude.Nothing
    }

-- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laMarker :: Lens.Lens' ListAliases (Lude.Maybe Lude.Text)
laMarker = Lens.lens (marker :: ListAliases -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListAliases)
{-# DEPRECATED laMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Limit the number of aliases returned.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laMaxItems :: Lens.Lens' ListAliases (Lude.Maybe Lude.Natural)
laMaxItems = Lens.lens (maxItems :: ListAliases -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListAliases)
{-# DEPRECATED laMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

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
laFunctionName :: Lens.Lens' ListAliases Lude.Text
laFunctionName = Lens.lens (functionName :: ListAliases -> Lude.Text) (\s a -> s {functionName = a} :: ListAliases)
{-# DEPRECATED laFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | Specify a function version to only list aliases that invoke that version.
--
-- /Note:/ Consider using 'functionVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laFunctionVersion :: Lens.Lens' ListAliases (Lude.Maybe Lude.Text)
laFunctionVersion = Lens.lens (functionVersion :: ListAliases -> Lude.Maybe Lude.Text) (\s a -> s {functionVersion = a} :: ListAliases)
{-# DEPRECATED laFunctionVersion "Use generic-lens or generic-optics with 'functionVersion' instead." #-}

instance Page.AWSPager ListAliases where
  page rq rs
    | Page.stop (rs Lens.^. larsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. larsAliases) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& laMarker Lens..~ rs Lens.^. larsNextMarker

instance Lude.AWSRequest ListAliases where
  type Rs ListAliases = ListAliasesResponse
  request = Req.get lambdaService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAliasesResponse'
            Lude.<$> (x Lude..?> "Aliases" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAliases where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListAliases where
  toPath ListAliases' {..} =
    Lude.mconcat
      ["/2015-03-31/functions/", Lude.toBS functionName, "/aliases"]

instance Lude.ToQuery ListAliases where
  toQuery ListAliases' {..} =
    Lude.mconcat
      [ "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems,
        "FunctionVersion" Lude.=: functionVersion
      ]

-- | /See:/ 'mkListAliasesResponse' smart constructor.
data ListAliasesResponse = ListAliasesResponse'
  { -- | A list of aliases.
    aliases :: Lude.Maybe [AliasConfiguration],
    -- | The pagination token that's included if more results are available.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAliasesResponse' with the minimum fields required to make a request.
--
-- * 'aliases' - A list of aliases.
-- * 'nextMarker' - The pagination token that's included if more results are available.
-- * 'responseStatus' - The response status code.
mkListAliasesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAliasesResponse
mkListAliasesResponse pResponseStatus_ =
  ListAliasesResponse'
    { aliases = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of aliases.
--
-- /Note:/ Consider using 'aliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsAliases :: Lens.Lens' ListAliasesResponse (Lude.Maybe [AliasConfiguration])
larsAliases = Lens.lens (aliases :: ListAliasesResponse -> Lude.Maybe [AliasConfiguration]) (\s a -> s {aliases = a} :: ListAliasesResponse)
{-# DEPRECATED larsAliases "Use generic-lens or generic-optics with 'aliases' instead." #-}

-- | The pagination token that's included if more results are available.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsNextMarker :: Lens.Lens' ListAliasesResponse (Lude.Maybe Lude.Text)
larsNextMarker = Lens.lens (nextMarker :: ListAliasesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListAliasesResponse)
{-# DEPRECATED larsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larsResponseStatus :: Lens.Lens' ListAliasesResponse Lude.Int
larsResponseStatus = Lens.lens (responseStatus :: ListAliasesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAliasesResponse)
{-# DEPRECATED larsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
