{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.DescribeParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the detailed parameter list for a particular parameter group.
--
-- This operation returns paginated results.
module Network.AWS.DAX.DescribeParameters
  ( -- * Creating a request
    DescribeParameters (..),
    mkDescribeParameters,

    -- ** Request lenses
    dpNextToken,
    dpSource,
    dpMaxResults,
    dpParameterGroupName,

    -- * Destructuring the response
    DescribeParametersResponse (..),
    mkDescribeParametersResponse,

    -- ** Response lenses
    dprsNextToken,
    dprsParameters,
    dprsResponseStatus,
  )
where

import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeParameters' smart constructor.
data DescribeParameters = DescribeParameters'
  { -- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | How the parameter is defined. For example, @system@ denotes a system-defined parameter.
    source :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
    --
    -- The value for @MaxResults@ must be between 20 and 100.
    maxResults :: Lude.Maybe Lude.Int,
    -- | The name of the parameter group.
    parameterGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeParameters' with the minimum fields required to make a request.
--
-- * 'nextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
-- * 'source' - How the parameter is defined. For example, @system@ denotes a system-defined parameter.
-- * 'maxResults' - The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
-- * 'parameterGroupName' - The name of the parameter group.
mkDescribeParameters ::
  -- | 'parameterGroupName'
  Lude.Text ->
  DescribeParameters
mkDescribeParameters pParameterGroupName_ =
  DescribeParameters'
    { nextToken = Lude.Nothing,
      source = Lude.Nothing,
      maxResults = Lude.Nothing,
      parameterGroupName = pParameterGroupName_
    }

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpNextToken :: Lens.Lens' DescribeParameters (Lude.Maybe Lude.Text)
dpNextToken = Lens.lens (nextToken :: DescribeParameters -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeParameters)
{-# DEPRECATED dpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | How the parameter is defined. For example, @system@ denotes a system-defined parameter.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpSource :: Lens.Lens' DescribeParameters (Lude.Maybe Lude.Text)
dpSource = Lens.lens (source :: DescribeParameters -> Lude.Maybe Lude.Text) (\s a -> s {source = a} :: DescribeParameters)
{-# DEPRECATED dpSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpMaxResults :: Lens.Lens' DescribeParameters (Lude.Maybe Lude.Int)
dpMaxResults = Lens.lens (maxResults :: DescribeParameters -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeParameters)
{-# DEPRECATED dpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The name of the parameter group.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpParameterGroupName :: Lens.Lens' DescribeParameters Lude.Text
dpParameterGroupName = Lens.lens (parameterGroupName :: DescribeParameters -> Lude.Text) (\s a -> s {parameterGroupName = a} :: DescribeParameters)
{-# DEPRECATED dpParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

instance Page.AWSPager DescribeParameters where
  page rq rs
    | Page.stop (rs Lens.^. dprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dprsParameters) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dpNextToken Lens..~ rs Lens.^. dprsNextToken

instance Lude.AWSRequest DescribeParameters where
  type Rs DescribeParameters = DescribeParametersResponse
  request = Req.postJSON daxService
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
              Lude.=# ("AmazonDAXV3.DescribeParameters" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeParameters where
  toJSON DescribeParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Source" Lude..=) Lude.<$> source,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("ParameterGroupName" Lude..= parameterGroupName)
          ]
      )

instance Lude.ToPath DescribeParameters where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeParameters where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeParametersResponse' smart constructor.
data DescribeParametersResponse = DescribeParametersResponse'
  { -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of parameters within a parameter group. Each element in the list represents one parameter.
    parameters :: Lude.Maybe [Parameter],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeParametersResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Provides an identifier to allow retrieval of paginated results.
-- * 'parameters' - A list of parameters within a parameter group. Each element in the list represents one parameter.
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

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsNextToken :: Lens.Lens' DescribeParametersResponse (Lude.Maybe Lude.Text)
dprsNextToken = Lens.lens (nextToken :: DescribeParametersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeParametersResponse)
{-# DEPRECATED dprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of parameters within a parameter group. Each element in the list represents one parameter.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsParameters :: Lens.Lens' DescribeParametersResponse (Lude.Maybe [Parameter])
dprsParameters = Lens.lens (parameters :: DescribeParametersResponse -> Lude.Maybe [Parameter]) (\s a -> s {parameters = a} :: DescribeParametersResponse)
{-# DEPRECATED dprsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsResponseStatus :: Lens.Lens' DescribeParametersResponse Lude.Int
dprsResponseStatus = Lens.lens (responseStatus :: DescribeParametersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeParametersResponse)
{-# DEPRECATED dprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
