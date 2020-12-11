{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DescribeCapacityProviders
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your capacity providers.
module Network.AWS.ECS.DescribeCapacityProviders
  ( -- * Creating a request
    DescribeCapacityProviders (..),
    mkDescribeCapacityProviders,

    -- ** Request lenses
    dcpInclude,
    dcpNextToken,
    dcpCapacityProviders,
    dcpMaxResults,

    -- * Destructuring the response
    DescribeCapacityProvidersResponse (..),
    mkDescribeCapacityProvidersResponse,

    -- ** Response lenses
    dcprsFailures,
    dcprsNextToken,
    dcprsCapacityProviders,
    dcprsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeCapacityProviders' smart constructor.
data DescribeCapacityProviders = DescribeCapacityProviders'
  { include ::
      Lude.Maybe [CapacityProviderField],
    nextToken :: Lude.Maybe Lude.Text,
    capacityProviders ::
      Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'DescribeCapacityProviders' with the minimum fields required to make a request.
--
-- * 'capacityProviders' - The short name or full Amazon Resource Name (ARN) of one or more capacity providers. Up to @100@ capacity providers can be described in an action.
-- * 'include' - Specifies whether or not you want to see the resource tags for the capacity provider. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
-- * 'maxResults' - The maximum number of account setting results returned by @DescribeCapacityProviders@ in paginated output. When this parameter is used, @DescribeCapacityProviders@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeCapacityProviders@ request with the returned @nextToken@ value. This value can be between 1 and 10. If this parameter is not used, then @DescribeCapacityProviders@ returns up to 10 results and a @nextToken@ value if applicable.
-- * 'nextToken' - The @nextToken@ value returned from a previous paginated @DescribeCapacityProviders@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value.
mkDescribeCapacityProviders ::
  DescribeCapacityProviders
mkDescribeCapacityProviders =
  DescribeCapacityProviders'
    { include = Lude.Nothing,
      nextToken = Lude.Nothing,
      capacityProviders = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Specifies whether or not you want to see the resource tags for the capacity provider. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
--
-- /Note:/ Consider using 'include' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpInclude :: Lens.Lens' DescribeCapacityProviders (Lude.Maybe [CapacityProviderField])
dcpInclude = Lens.lens (include :: DescribeCapacityProviders -> Lude.Maybe [CapacityProviderField]) (\s a -> s {include = a} :: DescribeCapacityProviders)
{-# DEPRECATED dcpInclude "Use generic-lens or generic-optics with 'include' instead." #-}

-- | The @nextToken@ value returned from a previous paginated @DescribeCapacityProviders@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpNextToken :: Lens.Lens' DescribeCapacityProviders (Lude.Maybe Lude.Text)
dcpNextToken = Lens.lens (nextToken :: DescribeCapacityProviders -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeCapacityProviders)
{-# DEPRECATED dcpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of one or more capacity providers. Up to @100@ capacity providers can be described in an action.
--
-- /Note:/ Consider using 'capacityProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpCapacityProviders :: Lens.Lens' DescribeCapacityProviders (Lude.Maybe [Lude.Text])
dcpCapacityProviders = Lens.lens (capacityProviders :: DescribeCapacityProviders -> Lude.Maybe [Lude.Text]) (\s a -> s {capacityProviders = a} :: DescribeCapacityProviders)
{-# DEPRECATED dcpCapacityProviders "Use generic-lens or generic-optics with 'capacityProviders' instead." #-}

-- | The maximum number of account setting results returned by @DescribeCapacityProviders@ in paginated output. When this parameter is used, @DescribeCapacityProviders@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeCapacityProviders@ request with the returned @nextToken@ value. This value can be between 1 and 10. If this parameter is not used, then @DescribeCapacityProviders@ returns up to 10 results and a @nextToken@ value if applicable.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpMaxResults :: Lens.Lens' DescribeCapacityProviders (Lude.Maybe Lude.Int)
dcpMaxResults = Lens.lens (maxResults :: DescribeCapacityProviders -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeCapacityProviders)
{-# DEPRECATED dcpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest DescribeCapacityProviders where
  type
    Rs DescribeCapacityProviders =
      DescribeCapacityProvidersResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeCapacityProvidersResponse'
            Lude.<$> (x Lude..?> "failures" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "capacityProviders" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCapacityProviders where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.DescribeCapacityProviders" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeCapacityProviders where
  toJSON DescribeCapacityProviders' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("include" Lude..=) Lude.<$> include,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("capacityProviders" Lude..=) Lude.<$> capacityProviders,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeCapacityProviders where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCapacityProviders where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeCapacityProvidersResponse' smart constructor.
data DescribeCapacityProvidersResponse = DescribeCapacityProvidersResponse'
  { failures ::
      Lude.Maybe [Failure],
    nextToken ::
      Lude.Maybe Lude.Text,
    capacityProviders ::
      Lude.Maybe
        [CapacityProvider],
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

-- | Creates a value of 'DescribeCapacityProvidersResponse' with the minimum fields required to make a request.
--
-- * 'capacityProviders' - The list of capacity providers.
-- * 'failures' - Any failures associated with the call.
-- * 'nextToken' - The @nextToken@ value to include in a future @DescribeCapacityProviders@ request. When the results of a @DescribeCapacityProviders@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeCapacityProvidersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCapacityProvidersResponse
mkDescribeCapacityProvidersResponse pResponseStatus_ =
  DescribeCapacityProvidersResponse'
    { failures = Lude.Nothing,
      nextToken = Lude.Nothing,
      capacityProviders = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Any failures associated with the call.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprsFailures :: Lens.Lens' DescribeCapacityProvidersResponse (Lude.Maybe [Failure])
dcprsFailures = Lens.lens (failures :: DescribeCapacityProvidersResponse -> Lude.Maybe [Failure]) (\s a -> s {failures = a} :: DescribeCapacityProvidersResponse)
{-# DEPRECATED dcprsFailures "Use generic-lens or generic-optics with 'failures' instead." #-}

-- | The @nextToken@ value to include in a future @DescribeCapacityProviders@ request. When the results of a @DescribeCapacityProviders@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprsNextToken :: Lens.Lens' DescribeCapacityProvidersResponse (Lude.Maybe Lude.Text)
dcprsNextToken = Lens.lens (nextToken :: DescribeCapacityProvidersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeCapacityProvidersResponse)
{-# DEPRECATED dcprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of capacity providers.
--
-- /Note:/ Consider using 'capacityProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprsCapacityProviders :: Lens.Lens' DescribeCapacityProvidersResponse (Lude.Maybe [CapacityProvider])
dcprsCapacityProviders = Lens.lens (capacityProviders :: DescribeCapacityProvidersResponse -> Lude.Maybe [CapacityProvider]) (\s a -> s {capacityProviders = a} :: DescribeCapacityProvidersResponse)
{-# DEPRECATED dcprsCapacityProviders "Use generic-lens or generic-optics with 'capacityProviders' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprsResponseStatus :: Lens.Lens' DescribeCapacityProvidersResponse Lude.Int
dcprsResponseStatus = Lens.lens (responseStatus :: DescribeCapacityProvidersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCapacityProvidersResponse)
{-# DEPRECATED dcprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
