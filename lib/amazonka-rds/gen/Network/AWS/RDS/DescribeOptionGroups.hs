{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeOptionGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available option groups.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeOptionGroups
  ( -- * Creating a request
    DescribeOptionGroups (..),
    mkDescribeOptionGroups,

    -- ** Request lenses
    dogFilters,
    dogEngineName,
    dogMajorEngineVersion,
    dogMarker,
    dogMaxRecords,
    dogOptionGroupName,

    -- * Destructuring the response
    DescribeOptionGroupsResponse (..),
    mkDescribeOptionGroupsResponse,

    -- ** Response lenses
    dogrsMarker,
    dogrsOptionGroupsList,
    dogrsResponseStatus,
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
-- /See:/ 'mkDescribeOptionGroups' smart constructor.
data DescribeOptionGroups = DescribeOptionGroups'
  { filters ::
      Lude.Maybe [Filter],
    engineName :: Lude.Maybe Lude.Text,
    majorEngineVersion :: Lude.Maybe Lude.Text,
    marker :: Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int,
    optionGroupName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOptionGroups' with the minimum fields required to make a request.
--
-- * 'engineName' - Filters the list of option groups to only include groups associated with a specific database engine.
-- * 'filters' - This parameter isn't currently supported.
-- * 'majorEngineVersion' - Filters the list of option groups to only include groups associated with a specific database engine version. If specified, then EngineName must also be specified.
-- * 'marker' - An optional pagination token provided by a previous DescribeOptionGroups request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
-- * 'optionGroupName' - The name of the option group to describe. Can't be supplied together with EngineName or MajorEngineVersion.
mkDescribeOptionGroups ::
  DescribeOptionGroups
mkDescribeOptionGroups =
  DescribeOptionGroups'
    { filters = Lude.Nothing,
      engineName = Lude.Nothing,
      majorEngineVersion = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      optionGroupName = Lude.Nothing
    }

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogFilters :: Lens.Lens' DescribeOptionGroups (Lude.Maybe [Filter])
dogFilters = Lens.lens (filters :: DescribeOptionGroups -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeOptionGroups)
{-# DEPRECATED dogFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | Filters the list of option groups to only include groups associated with a specific database engine.
--
-- /Note:/ Consider using 'engineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogEngineName :: Lens.Lens' DescribeOptionGroups (Lude.Maybe Lude.Text)
dogEngineName = Lens.lens (engineName :: DescribeOptionGroups -> Lude.Maybe Lude.Text) (\s a -> s {engineName = a} :: DescribeOptionGroups)
{-# DEPRECATED dogEngineName "Use generic-lens or generic-optics with 'engineName' instead." #-}

-- | Filters the list of option groups to only include groups associated with a specific database engine version. If specified, then EngineName must also be specified.
--
-- /Note:/ Consider using 'majorEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogMajorEngineVersion :: Lens.Lens' DescribeOptionGroups (Lude.Maybe Lude.Text)
dogMajorEngineVersion = Lens.lens (majorEngineVersion :: DescribeOptionGroups -> Lude.Maybe Lude.Text) (\s a -> s {majorEngineVersion = a} :: DescribeOptionGroups)
{-# DEPRECATED dogMajorEngineVersion "Use generic-lens or generic-optics with 'majorEngineVersion' instead." #-}

-- | An optional pagination token provided by a previous DescribeOptionGroups request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogMarker :: Lens.Lens' DescribeOptionGroups (Lude.Maybe Lude.Text)
dogMarker = Lens.lens (marker :: DescribeOptionGroups -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeOptionGroups)
{-# DEPRECATED dogMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogMaxRecords :: Lens.Lens' DescribeOptionGroups (Lude.Maybe Lude.Int)
dogMaxRecords = Lens.lens (maxRecords :: DescribeOptionGroups -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeOptionGroups)
{-# DEPRECATED dogMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The name of the option group to describe. Can't be supplied together with EngineName or MajorEngineVersion.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogOptionGroupName :: Lens.Lens' DescribeOptionGroups (Lude.Maybe Lude.Text)
dogOptionGroupName = Lens.lens (optionGroupName :: DescribeOptionGroups -> Lude.Maybe Lude.Text) (\s a -> s {optionGroupName = a} :: DescribeOptionGroups)
{-# DEPRECATED dogOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

instance Page.AWSPager DescribeOptionGroups where
  page rq rs
    | Page.stop (rs Lens.^. dogrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dogrsOptionGroupsList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& dogMarker Lens..~ rs Lens.^. dogrsMarker

instance Lude.AWSRequest DescribeOptionGroups where
  type Rs DescribeOptionGroups = DescribeOptionGroupsResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeOptionGroupsResult"
      ( \s h x ->
          DescribeOptionGroupsResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "OptionGroupsList" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "OptionGroup")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeOptionGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeOptionGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeOptionGroups where
  toQuery DescribeOptionGroups' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeOptionGroups" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "EngineName" Lude.=: engineName,
        "MajorEngineVersion" Lude.=: majorEngineVersion,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "OptionGroupName" Lude.=: optionGroupName
      ]

-- | List of option groups.
--
-- /See:/ 'mkDescribeOptionGroupsResponse' smart constructor.
data DescribeOptionGroupsResponse = DescribeOptionGroupsResponse'
  { marker ::
      Lude.Maybe Lude.Text,
    optionGroupsList ::
      Lude.Maybe [OptionGroup],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOptionGroupsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'optionGroupsList' - List of option groups.
-- * 'responseStatus' - The response status code.
mkDescribeOptionGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeOptionGroupsResponse
mkDescribeOptionGroupsResponse pResponseStatus_ =
  DescribeOptionGroupsResponse'
    { marker = Lude.Nothing,
      optionGroupsList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogrsMarker :: Lens.Lens' DescribeOptionGroupsResponse (Lude.Maybe Lude.Text)
dogrsMarker = Lens.lens (marker :: DescribeOptionGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeOptionGroupsResponse)
{-# DEPRECATED dogrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | List of option groups.
--
-- /Note:/ Consider using 'optionGroupsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogrsOptionGroupsList :: Lens.Lens' DescribeOptionGroupsResponse (Lude.Maybe [OptionGroup])
dogrsOptionGroupsList = Lens.lens (optionGroupsList :: DescribeOptionGroupsResponse -> Lude.Maybe [OptionGroup]) (\s a -> s {optionGroupsList = a} :: DescribeOptionGroupsResponse)
{-# DEPRECATED dogrsOptionGroupsList "Use generic-lens or generic-optics with 'optionGroupsList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dogrsResponseStatus :: Lens.Lens' DescribeOptionGroupsResponse Lude.Int
dogrsResponseStatus = Lens.lens (responseStatus :: DescribeOptionGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeOptionGroupsResponse)
{-# DEPRECATED dogrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
