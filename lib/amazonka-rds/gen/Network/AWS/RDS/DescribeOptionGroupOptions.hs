{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeOptionGroupOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes all available options.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeOptionGroupOptions
  ( -- * Creating a request
    DescribeOptionGroupOptions (..),
    mkDescribeOptionGroupOptions,

    -- ** Request lenses
    dogoFilters,
    dogoEngineName,
    dogoMajorEngineVersion,
    dogoMarker,
    dogoMaxRecords,

    -- * Destructuring the response
    DescribeOptionGroupOptionsResponse (..),
    mkDescribeOptionGroupOptionsResponse,

    -- ** Response lenses
    dogorsOptionGroupOptions,
    dogorsMarker,
    dogorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeOptionGroupOptions' smart constructor.
data DescribeOptionGroupOptions = DescribeOptionGroupOptions'
  { -- | This parameter isn't currently supported.
    filters :: Lude.Maybe [Filter],
    -- | A required parameter. Options available for the given engine name are described.
    engineName :: Lude.Text,
    -- | If specified, filters the results to include only options for the specified major engine version.
    majorEngineVersion :: Lude.Maybe Lude.Text,
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOptionGroupOptions' with the minimum fields required to make a request.
--
-- * 'filters' - This parameter isn't currently supported.
-- * 'engineName' - A required parameter. Options available for the given engine name are described.
-- * 'majorEngineVersion' - If specified, filters the results to include only options for the specified major engine version.
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
mkDescribeOptionGroupOptions ::
  -- | 'engineName'
  Lude.Text ->
  DescribeOptionGroupOptions
mkDescribeOptionGroupOptions pEngineName_ =
  DescribeOptionGroupOptions'
    { filters = Lude.Nothing,
      engineName = pEngineName_,
      majorEngineVersion = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogoFilters :: Lens.Lens' DescribeOptionGroupOptions (Lude.Maybe [Filter])
dogoFilters = Lens.lens (filters :: DescribeOptionGroupOptions -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeOptionGroupOptions)
{-# DEPRECATED dogoFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | A required parameter. Options available for the given engine name are described.
--
-- /Note:/ Consider using 'engineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogoEngineName :: Lens.Lens' DescribeOptionGroupOptions Lude.Text
dogoEngineName = Lens.lens (engineName :: DescribeOptionGroupOptions -> Lude.Text) (\s a -> s {engineName = a} :: DescribeOptionGroupOptions)
{-# DEPRECATED dogoEngineName "Use generic-lens or generic-optics with 'engineName' instead." #-}

-- | If specified, filters the results to include only options for the specified major engine version.
--
-- /Note:/ Consider using 'majorEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogoMajorEngineVersion :: Lens.Lens' DescribeOptionGroupOptions (Lude.Maybe Lude.Text)
dogoMajorEngineVersion = Lens.lens (majorEngineVersion :: DescribeOptionGroupOptions -> Lude.Maybe Lude.Text) (\s a -> s {majorEngineVersion = a} :: DescribeOptionGroupOptions)
{-# DEPRECATED dogoMajorEngineVersion "Use generic-lens or generic-optics with 'majorEngineVersion' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogoMarker :: Lens.Lens' DescribeOptionGroupOptions (Lude.Maybe Lude.Text)
dogoMarker = Lens.lens (marker :: DescribeOptionGroupOptions -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeOptionGroupOptions)
{-# DEPRECATED dogoMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogoMaxRecords :: Lens.Lens' DescribeOptionGroupOptions (Lude.Maybe Lude.Int)
dogoMaxRecords = Lens.lens (maxRecords :: DescribeOptionGroupOptions -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeOptionGroupOptions)
{-# DEPRECATED dogoMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeOptionGroupOptions where
  page rq rs
    | Page.stop (rs Lens.^. dogorsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dogorsOptionGroupOptions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dogoMarker Lens..~ rs Lens.^. dogorsMarker

instance Lude.AWSRequest DescribeOptionGroupOptions where
  type
    Rs DescribeOptionGroupOptions =
      DescribeOptionGroupOptionsResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeOptionGroupOptionsResult"
      ( \s h x ->
          DescribeOptionGroupOptionsResponse'
            Lude.<$> ( x Lude..@? "OptionGroupOptions" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "OptionGroupOption")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeOptionGroupOptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeOptionGroupOptions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeOptionGroupOptions where
  toQuery DescribeOptionGroupOptions' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeOptionGroupOptions" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "EngineName" Lude.=: engineName,
        "MajorEngineVersion" Lude.=: majorEngineVersion,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- |
--
-- /See:/ 'mkDescribeOptionGroupOptionsResponse' smart constructor.
data DescribeOptionGroupOptionsResponse = DescribeOptionGroupOptionsResponse'
  { optionGroupOptions :: Lude.Maybe [OptionGroupOption],
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOptionGroupOptionsResponse' with the minimum fields required to make a request.
--
-- * 'optionGroupOptions' -
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'responseStatus' - The response status code.
mkDescribeOptionGroupOptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeOptionGroupOptionsResponse
mkDescribeOptionGroupOptionsResponse pResponseStatus_ =
  DescribeOptionGroupOptionsResponse'
    { optionGroupOptions =
        Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'optionGroupOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogorsOptionGroupOptions :: Lens.Lens' DescribeOptionGroupOptionsResponse (Lude.Maybe [OptionGroupOption])
dogorsOptionGroupOptions = Lens.lens (optionGroupOptions :: DescribeOptionGroupOptionsResponse -> Lude.Maybe [OptionGroupOption]) (\s a -> s {optionGroupOptions = a} :: DescribeOptionGroupOptionsResponse)
{-# DEPRECATED dogorsOptionGroupOptions "Use generic-lens or generic-optics with 'optionGroupOptions' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogorsMarker :: Lens.Lens' DescribeOptionGroupOptionsResponse (Lude.Maybe Lude.Text)
dogorsMarker = Lens.lens (marker :: DescribeOptionGroupOptionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeOptionGroupOptionsResponse)
{-# DEPRECATED dogorsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogorsResponseStatus :: Lens.Lens' DescribeOptionGroupOptionsResponse Lude.Int
dogorsResponseStatus = Lens.lens (responseStatus :: DescribeOptionGroupOptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeOptionGroupOptionsResponse)
{-# DEPRECATED dogorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
