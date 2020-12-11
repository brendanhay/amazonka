{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.DescribeDefaultParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the default system parameter information for the DAX caching software.
--
-- This operation returns paginated results.
module Network.AWS.DAX.DescribeDefaultParameters
  ( -- * Creating a request
    DescribeDefaultParameters (..),
    mkDescribeDefaultParameters,

    -- ** Request lenses
    ddpNextToken,
    ddpMaxResults,

    -- * Destructuring the response
    DescribeDefaultParametersResponse (..),
    mkDescribeDefaultParametersResponse,

    -- ** Response lenses
    ddprsNextToken,
    ddprsParameters,
    ddprsResponseStatus,
  )
where

import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeDefaultParameters' smart constructor.
data DescribeDefaultParameters = DescribeDefaultParameters'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDefaultParameters' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
-- * 'nextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
mkDescribeDefaultParameters ::
  DescribeDefaultParameters
mkDescribeDefaultParameters =
  DescribeDefaultParameters'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpNextToken :: Lens.Lens' DescribeDefaultParameters (Lude.Maybe Lude.Text)
ddpNextToken = Lens.lens (nextToken :: DescribeDefaultParameters -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeDefaultParameters)
{-# DEPRECATED ddpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpMaxResults :: Lens.Lens' DescribeDefaultParameters (Lude.Maybe Lude.Int)
ddpMaxResults = Lens.lens (maxResults :: DescribeDefaultParameters -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeDefaultParameters)
{-# DEPRECATED ddpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeDefaultParameters where
  page rq rs
    | Page.stop (rs Lens.^. ddprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ddprsParameters) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddpNextToken Lens..~ rs Lens.^. ddprsNextToken

instance Lude.AWSRequest DescribeDefaultParameters where
  type
    Rs DescribeDefaultParameters =
      DescribeDefaultParametersResponse
  request = Req.postJSON daxService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDefaultParametersResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Parameters" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDefaultParameters where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDAXV3.DescribeDefaultParameters" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeDefaultParameters where
  toJSON DescribeDefaultParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeDefaultParameters where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDefaultParameters where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeDefaultParametersResponse' smart constructor.
data DescribeDefaultParametersResponse = DescribeDefaultParametersResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    parameters ::
      Lude.Maybe [Parameter],
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

-- | Creates a value of 'DescribeDefaultParametersResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Provides an identifier to allow retrieval of paginated results.
-- * 'parameters' - A list of parameters. Each element in the list represents one parameter.
-- * 'responseStatus' - The response status code.
mkDescribeDefaultParametersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDefaultParametersResponse
mkDescribeDefaultParametersResponse pResponseStatus_ =
  DescribeDefaultParametersResponse'
    { nextToken = Lude.Nothing,
      parameters = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddprsNextToken :: Lens.Lens' DescribeDefaultParametersResponse (Lude.Maybe Lude.Text)
ddprsNextToken = Lens.lens (nextToken :: DescribeDefaultParametersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeDefaultParametersResponse)
{-# DEPRECATED ddprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of parameters. Each element in the list represents one parameter.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddprsParameters :: Lens.Lens' DescribeDefaultParametersResponse (Lude.Maybe [Parameter])
ddprsParameters = Lens.lens (parameters :: DescribeDefaultParametersResponse -> Lude.Maybe [Parameter]) (\s a -> s {parameters = a} :: DescribeDefaultParametersResponse)
{-# DEPRECATED ddprsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddprsResponseStatus :: Lens.Lens' DescribeDefaultParametersResponse Lude.Int
ddprsResponseStatus = Lens.lens (responseStatus :: DescribeDefaultParametersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDefaultParametersResponse)
{-# DEPRECATED ddprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
