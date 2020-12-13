{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeCacheParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the detailed parameter list for a particular cache parameter group.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeCacheParameters
  ( -- * Creating a request
    DescribeCacheParameters (..),
    mkDescribeCacheParameters,

    -- ** Request lenses
    dcpCacheParameterGroupName,
    dcpMarker,
    dcpMaxRecords,
    dcpSource,

    -- * Destructuring the response
    DescribeCacheParametersResponse (..),
    mkDescribeCacheParametersResponse,

    -- ** Response lenses
    dcprsCacheNodeTypeSpecificParameters,
    dcprsMarker,
    dcprsParameters,
    dcprsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DescribeCacheParameters@ operation.
--
-- /See:/ 'mkDescribeCacheParameters' smart constructor.
data DescribeCacheParameters = DescribeCacheParameters'
  { -- | The name of a specific cache parameter group to return details for.
    cacheParameterGroupName :: Lude.Text,
    -- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    -- Constraints: minimum 20; maximum 100.
    maxRecords :: Lude.Maybe Lude.Int,
    -- | The parameter types to return.
    --
    -- Valid values: @user@ | @system@ | @engine-default@
    source :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCacheParameters' with the minimum fields required to make a request.
--
-- * 'cacheParameterGroupName' - The name of a specific cache parameter group to return details for.
-- * 'marker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
-- * 'source' - The parameter types to return.
--
-- Valid values: @user@ | @system@ | @engine-default@
mkDescribeCacheParameters ::
  -- | 'cacheParameterGroupName'
  Lude.Text ->
  DescribeCacheParameters
mkDescribeCacheParameters pCacheParameterGroupName_ =
  DescribeCacheParameters'
    { cacheParameterGroupName =
        pCacheParameterGroupName_,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      source = Lude.Nothing
    }

-- | The name of a specific cache parameter group to return details for.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpCacheParameterGroupName :: Lens.Lens' DescribeCacheParameters Lude.Text
dcpCacheParameterGroupName = Lens.lens (cacheParameterGroupName :: DescribeCacheParameters -> Lude.Text) (\s a -> s {cacheParameterGroupName = a} :: DescribeCacheParameters)
{-# DEPRECATED dcpCacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpMarker :: Lens.Lens' DescribeCacheParameters (Lude.Maybe Lude.Text)
dcpMarker = Lens.lens (marker :: DescribeCacheParameters -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeCacheParameters)
{-# DEPRECATED dcpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpMaxRecords :: Lens.Lens' DescribeCacheParameters (Lude.Maybe Lude.Int)
dcpMaxRecords = Lens.lens (maxRecords :: DescribeCacheParameters -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeCacheParameters)
{-# DEPRECATED dcpMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The parameter types to return.
--
-- Valid values: @user@ | @system@ | @engine-default@
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpSource :: Lens.Lens' DescribeCacheParameters (Lude.Maybe Lude.Text)
dcpSource = Lens.lens (source :: DescribeCacheParameters -> Lude.Maybe Lude.Text) (\s a -> s {source = a} :: DescribeCacheParameters)
{-# DEPRECATED dcpSource "Use generic-lens or generic-optics with 'source' instead." #-}

instance Page.AWSPager DescribeCacheParameters where
  page rq rs
    | Page.stop (rs Lens.^. dcprsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dcprsParameters) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& dcpMarker Lens..~ rs Lens.^. dcprsMarker

instance Lude.AWSRequest DescribeCacheParameters where
  type Rs DescribeCacheParameters = DescribeCacheParametersResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "DescribeCacheParametersResult"
      ( \s h x ->
          DescribeCacheParametersResponse'
            Lude.<$> ( x Lude..@? "CacheNodeTypeSpecificParameters" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "CacheNodeTypeSpecificParameter")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "Parameters" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "Parameter")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCacheParameters where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeCacheParameters where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCacheParameters where
  toQuery DescribeCacheParameters' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeCacheParameters" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "CacheParameterGroupName" Lude.=: cacheParameterGroupName,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "Source" Lude.=: source
      ]

-- | Represents the output of a @DescribeCacheParameters@ operation.
--
-- /See:/ 'mkDescribeCacheParametersResponse' smart constructor.
data DescribeCacheParametersResponse = DescribeCacheParametersResponse'
  { -- | A list of parameters specific to a particular cache node type. Each element in the list contains detailed information about one parameter.
    cacheNodeTypeSpecificParameters :: Lude.Maybe [CacheNodeTypeSpecificParameter],
    -- | Provides an identifier to allow retrieval of paginated results.
    marker :: Lude.Maybe Lude.Text,
    -- | A list of 'Parameter' instances.
    parameters :: Lude.Maybe [Parameter],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCacheParametersResponse' with the minimum fields required to make a request.
--
-- * 'cacheNodeTypeSpecificParameters' - A list of parameters specific to a particular cache node type. Each element in the list contains detailed information about one parameter.
-- * 'marker' - Provides an identifier to allow retrieval of paginated results.
-- * 'parameters' - A list of 'Parameter' instances.
-- * 'responseStatus' - The response status code.
mkDescribeCacheParametersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCacheParametersResponse
mkDescribeCacheParametersResponse pResponseStatus_ =
  DescribeCacheParametersResponse'
    { cacheNodeTypeSpecificParameters =
        Lude.Nothing,
      marker = Lude.Nothing,
      parameters = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of parameters specific to a particular cache node type. Each element in the list contains detailed information about one parameter.
--
-- /Note:/ Consider using 'cacheNodeTypeSpecificParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprsCacheNodeTypeSpecificParameters :: Lens.Lens' DescribeCacheParametersResponse (Lude.Maybe [CacheNodeTypeSpecificParameter])
dcprsCacheNodeTypeSpecificParameters = Lens.lens (cacheNodeTypeSpecificParameters :: DescribeCacheParametersResponse -> Lude.Maybe [CacheNodeTypeSpecificParameter]) (\s a -> s {cacheNodeTypeSpecificParameters = a} :: DescribeCacheParametersResponse)
{-# DEPRECATED dcprsCacheNodeTypeSpecificParameters "Use generic-lens or generic-optics with 'cacheNodeTypeSpecificParameters' instead." #-}

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprsMarker :: Lens.Lens' DescribeCacheParametersResponse (Lude.Maybe Lude.Text)
dcprsMarker = Lens.lens (marker :: DescribeCacheParametersResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeCacheParametersResponse)
{-# DEPRECATED dcprsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of 'Parameter' instances.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprsParameters :: Lens.Lens' DescribeCacheParametersResponse (Lude.Maybe [Parameter])
dcprsParameters = Lens.lens (parameters :: DescribeCacheParametersResponse -> Lude.Maybe [Parameter]) (\s a -> s {parameters = a} :: DescribeCacheParametersResponse)
{-# DEPRECATED dcprsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprsResponseStatus :: Lens.Lens' DescribeCacheParametersResponse Lude.Int
dcprsResponseStatus = Lens.lens (responseStatus :: DescribeCacheParametersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCacheParametersResponse)
{-# DEPRECATED dcprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
