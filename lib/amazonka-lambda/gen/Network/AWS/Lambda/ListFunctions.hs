{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.ListFunctions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of Lambda functions, with the version-specific configuration of each. Lambda returns up to 50 functions per call.
--
-- Set @FunctionVersion@ to @ALL@ to include all published versions of each function in addition to the unpublished version. To get more information about a function or version, use 'GetFunction' .
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListFunctions
  ( -- * Creating a request
    ListFunctions (..),
    mkListFunctions,

    -- ** Request lenses
    lfMasterRegion,
    lfMarker,
    lfMaxItems,
    lfFunctionVersion,

    -- * Destructuring the response
    ListFunctionsResponse (..),
    mkListFunctionsResponse,

    -- ** Response lenses
    lfrsNextMarker,
    lfrsFunctions,
    lfrsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListFunctions' smart constructor.
data ListFunctions = ListFunctions'
  { -- | For Lambda@Edge functions, the AWS Region of the master function. For example, @us-east-1@ filters the list of functions to only include Lambda@Edge functions replicated from a master function in US East (N. Virginia). If specified, you must set @FunctionVersion@ to @ALL@ .
    masterRegion :: Lude.Maybe Lude.Text,
    -- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of functions to return.
    maxItems :: Lude.Maybe Lude.Natural,
    -- | Set to @ALL@ to include entries for all published versions of each function.
    functionVersion :: Lude.Maybe FunctionVersion
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListFunctions' with the minimum fields required to make a request.
--
-- * 'masterRegion' - For Lambda@Edge functions, the AWS Region of the master function. For example, @us-east-1@ filters the list of functions to only include Lambda@Edge functions replicated from a master function in US East (N. Virginia). If specified, you must set @FunctionVersion@ to @ALL@ .
-- * 'marker' - Specify the pagination token that's returned by a previous request to retrieve the next page of results.
-- * 'maxItems' - The maximum number of functions to return.
-- * 'functionVersion' - Set to @ALL@ to include entries for all published versions of each function.
mkListFunctions ::
  ListFunctions
mkListFunctions =
  ListFunctions'
    { masterRegion = Lude.Nothing,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing,
      functionVersion = Lude.Nothing
    }

-- | For Lambda@Edge functions, the AWS Region of the master function. For example, @us-east-1@ filters the list of functions to only include Lambda@Edge functions replicated from a master function in US East (N. Virginia). If specified, you must set @FunctionVersion@ to @ALL@ .
--
-- /Note:/ Consider using 'masterRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfMasterRegion :: Lens.Lens' ListFunctions (Lude.Maybe Lude.Text)
lfMasterRegion = Lens.lens (masterRegion :: ListFunctions -> Lude.Maybe Lude.Text) (\s a -> s {masterRegion = a} :: ListFunctions)
{-# DEPRECATED lfMasterRegion "Use generic-lens or generic-optics with 'masterRegion' instead." #-}

-- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfMarker :: Lens.Lens' ListFunctions (Lude.Maybe Lude.Text)
lfMarker = Lens.lens (marker :: ListFunctions -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListFunctions)
{-# DEPRECATED lfMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of functions to return.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfMaxItems :: Lens.Lens' ListFunctions (Lude.Maybe Lude.Natural)
lfMaxItems = Lens.lens (maxItems :: ListFunctions -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListFunctions)
{-# DEPRECATED lfMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | Set to @ALL@ to include entries for all published versions of each function.
--
-- /Note:/ Consider using 'functionVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfFunctionVersion :: Lens.Lens' ListFunctions (Lude.Maybe FunctionVersion)
lfFunctionVersion = Lens.lens (functionVersion :: ListFunctions -> Lude.Maybe FunctionVersion) (\s a -> s {functionVersion = a} :: ListFunctions)
{-# DEPRECATED lfFunctionVersion "Use generic-lens or generic-optics with 'functionVersion' instead." #-}

instance Page.AWSPager ListFunctions where
  page rq rs
    | Page.stop (rs Lens.^. lfrsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lfrsFunctions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lfMarker Lens..~ rs Lens.^. lfrsNextMarker

instance Lude.AWSRequest ListFunctions where
  type Rs ListFunctions = ListFunctionsResponse
  request = Req.get lambdaService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListFunctionsResponse'
            Lude.<$> (x Lude..?> "NextMarker")
            Lude.<*> (x Lude..?> "Functions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListFunctions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListFunctions where
  toPath = Lude.const "/2015-03-31/functions/"

instance Lude.ToQuery ListFunctions where
  toQuery ListFunctions' {..} =
    Lude.mconcat
      [ "MasterRegion" Lude.=: masterRegion,
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems,
        "FunctionVersion" Lude.=: functionVersion
      ]

-- | A list of Lambda functions.
--
-- /See:/ 'mkListFunctionsResponse' smart constructor.
data ListFunctionsResponse = ListFunctionsResponse'
  { -- | The pagination token that's included if more results are available.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | A list of Lambda functions.
    functions :: Lude.Maybe [FunctionConfiguration],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListFunctionsResponse' with the minimum fields required to make a request.
--
-- * 'nextMarker' - The pagination token that's included if more results are available.
-- * 'functions' - A list of Lambda functions.
-- * 'responseStatus' - The response status code.
mkListFunctionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListFunctionsResponse
mkListFunctionsResponse pResponseStatus_ =
  ListFunctionsResponse'
    { nextMarker = Lude.Nothing,
      functions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The pagination token that's included if more results are available.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrsNextMarker :: Lens.Lens' ListFunctionsResponse (Lude.Maybe Lude.Text)
lfrsNextMarker = Lens.lens (nextMarker :: ListFunctionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListFunctionsResponse)
{-# DEPRECATED lfrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | A list of Lambda functions.
--
-- /Note:/ Consider using 'functions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrsFunctions :: Lens.Lens' ListFunctionsResponse (Lude.Maybe [FunctionConfiguration])
lfrsFunctions = Lens.lens (functions :: ListFunctionsResponse -> Lude.Maybe [FunctionConfiguration]) (\s a -> s {functions = a} :: ListFunctionsResponse)
{-# DEPRECATED lfrsFunctions "Use generic-lens or generic-optics with 'functions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrsResponseStatus :: Lens.Lens' ListFunctionsResponse Lude.Int
lfrsResponseStatus = Lens.lens (responseStatus :: ListFunctionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListFunctionsResponse)
{-# DEPRECATED lfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
