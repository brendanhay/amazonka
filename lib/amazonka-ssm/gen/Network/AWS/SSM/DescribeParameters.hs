{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about a parameter.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeParameters
  ( -- * Creating a request
    DescribeParameters (..),
    mkDescribeParameters,

    -- ** Request lenses
    dpParameterFilters,
    dpFilters,
    dpNextToken,
    dpMaxResults,

    -- * Destructuring the response
    DescribeParametersResponse (..),
    mkDescribeParametersResponse,

    -- ** Response lenses
    dpsrsNextToken,
    dpsrsParameters,
    dpsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeParameters' smart constructor.
data DescribeParameters = DescribeParameters'
  { -- | Filters to limit the request results.
    parameterFilters :: Lude.Maybe [ParameterStringFilter],
    -- | This data type is deprecated. Instead, use @ParameterFilters@ .
    filters :: Lude.Maybe [ParametersFilter],
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeParameters' with the minimum fields required to make a request.
--
-- * 'parameterFilters' - Filters to limit the request results.
-- * 'filters' - This data type is deprecated. Instead, use @ParameterFilters@ .
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
mkDescribeParameters ::
  DescribeParameters
mkDescribeParameters =
  DescribeParameters'
    { parameterFilters = Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Filters to limit the request results.
--
-- /Note:/ Consider using 'parameterFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpParameterFilters :: Lens.Lens' DescribeParameters (Lude.Maybe [ParameterStringFilter])
dpParameterFilters = Lens.lens (parameterFilters :: DescribeParameters -> Lude.Maybe [ParameterStringFilter]) (\s a -> s {parameterFilters = a} :: DescribeParameters)
{-# DEPRECATED dpParameterFilters "Use generic-lens or generic-optics with 'parameterFilters' instead." #-}

-- | This data type is deprecated. Instead, use @ParameterFilters@ .
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpFilters :: Lens.Lens' DescribeParameters (Lude.Maybe [ParametersFilter])
dpFilters = Lens.lens (filters :: DescribeParameters -> Lude.Maybe [ParametersFilter]) (\s a -> s {filters = a} :: DescribeParameters)
{-# DEPRECATED dpFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpNextToken :: Lens.Lens' DescribeParameters (Lude.Maybe Lude.Text)
dpNextToken = Lens.lens (nextToken :: DescribeParameters -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeParameters)
{-# DEPRECATED dpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpMaxResults :: Lens.Lens' DescribeParameters (Lude.Maybe Lude.Natural)
dpMaxResults = Lens.lens (maxResults :: DescribeParameters -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeParameters)
{-# DEPRECATED dpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeParameters where
  page rq rs
    | Page.stop (rs Lens.^. dpsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dpsrsParameters) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dpNextToken Lens..~ rs Lens.^. dpsrsNextToken

instance Lude.AWSRequest DescribeParameters where
  type Rs DescribeParameters = DescribeParametersResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeParametersResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Parameters" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeParameters where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DescribeParameters" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeParameters where
  toJSON DescribeParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ParameterFilters" Lude..=) Lude.<$> parameterFilters,
            ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeParameters where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeParameters where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeParametersResponse' smart constructor.
data DescribeParametersResponse = DescribeParametersResponse'
  { -- | The token to use when requesting the next set of items.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Parameters returned by the request.
    parameters :: Lude.Maybe [ParameterMetadata],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeParametersResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use when requesting the next set of items.
-- * 'parameters' - Parameters returned by the request.
-- * 'responseStatus' - The response status code.
mkDescribeParametersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeParametersResponse
mkDescribeParametersResponse pResponseStatus_ =
  DescribeParametersResponse'
    { nextToken = Lude.Nothing,
      parameters = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use when requesting the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsrsNextToken :: Lens.Lens' DescribeParametersResponse (Lude.Maybe Lude.Text)
dpsrsNextToken = Lens.lens (nextToken :: DescribeParametersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeParametersResponse)
{-# DEPRECATED dpsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Parameters returned by the request.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsrsParameters :: Lens.Lens' DescribeParametersResponse (Lude.Maybe [ParameterMetadata])
dpsrsParameters = Lens.lens (parameters :: DescribeParametersResponse -> Lude.Maybe [ParameterMetadata]) (\s a -> s {parameters = a} :: DescribeParametersResponse)
{-# DEPRECATED dpsrsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsrsResponseStatus :: Lens.Lens' DescribeParametersResponse Lude.Int
dpsrsResponseStatus = Lens.lens (responseStatus :: DescribeParametersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeParametersResponse)
{-# DEPRECATED dpsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
