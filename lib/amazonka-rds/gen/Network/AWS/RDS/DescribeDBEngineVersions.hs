{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBEngineVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the available DB engines.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBEngineVersions
  ( -- * Creating a request
    DescribeDBEngineVersions (..),
    mkDescribeDBEngineVersions,

    -- ** Request lenses
    ddevEngineVersion,
    ddevListSupportedTimezones,
    ddevDefaultOnly,
    ddevIncludeAll,
    ddevFilters,
    ddevEngine,
    ddevDBParameterGroupFamily,
    ddevListSupportedCharacterSets,
    ddevMarker,
    ddevMaxRecords,

    -- * Destructuring the response
    DescribeDBEngineVersionsResponse (..),
    mkDescribeDBEngineVersionsResponse,

    -- ** Response lenses
    ddevrsMarker,
    ddevrsDBEngineVersions,
    ddevrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeDBEngineVersions' smart constructor.
data DescribeDBEngineVersions = DescribeDBEngineVersions'
  { -- | The database engine version to return.
    --
    -- Example: @5.1.49@
    engineVersion :: Lude.Maybe Lude.Text,
    -- | A value that indicates whether to list the supported time zones for each engine version.
    --
    -- If this parameter is enabled and the requested engine supports the @TimeZone@ parameter for @CreateDBInstance@ , the response includes a list of supported time zones for each engine version.
    listSupportedTimezones :: Lude.Maybe Lude.Bool,
    -- | A value that indicates whether only the default version of the specified engine or engine and major version combination is returned.
    defaultOnly :: Lude.Maybe Lude.Bool,
    -- | A value that indicates whether to include engine versions that aren't available in the list. The default is to list only available engine versions.
    includeAll :: Lude.Maybe Lude.Bool,
    -- | This parameter isn't currently supported.
    filters :: Lude.Maybe [Filter],
    -- | The database engine to return.
    engine :: Lude.Maybe Lude.Text,
    -- | The name of a specific DB parameter group family to return details for.
    --
    -- Constraints:
    --
    --     * If supplied, must match an existing DBParameterGroupFamily.
    dbParameterGroupFamily :: Lude.Maybe Lude.Text,
    -- | A value that indicates whether to list the supported character sets for each engine version.
    --
    -- If this parameter is enabled and the requested engine supports the @CharacterSetName@ parameter for @CreateDBInstance@ , the response includes a list of supported character sets for each engine version.
    listSupportedCharacterSets :: Lude.Maybe Lude.Bool,
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of records to include in the response. If more than the @MaxRecords@ value is available, a pagination token called a marker is included in the response so you can retrieve the remaining results.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBEngineVersions' with the minimum fields required to make a request.
--
-- * 'engineVersion' - The database engine version to return.
--
-- Example: @5.1.49@
-- * 'listSupportedTimezones' - A value that indicates whether to list the supported time zones for each engine version.
--
-- If this parameter is enabled and the requested engine supports the @TimeZone@ parameter for @CreateDBInstance@ , the response includes a list of supported time zones for each engine version.
-- * 'defaultOnly' - A value that indicates whether only the default version of the specified engine or engine and major version combination is returned.
-- * 'includeAll' - A value that indicates whether to include engine versions that aren't available in the list. The default is to list only available engine versions.
-- * 'filters' - This parameter isn't currently supported.
-- * 'engine' - The database engine to return.
-- * 'dbParameterGroupFamily' - The name of a specific DB parameter group family to return details for.
--
-- Constraints:
--
--     * If supplied, must match an existing DBParameterGroupFamily.
--
--
-- * 'listSupportedCharacterSets' - A value that indicates whether to list the supported character sets for each engine version.
--
-- If this parameter is enabled and the requested engine supports the @CharacterSetName@ parameter for @CreateDBInstance@ , the response includes a list of supported character sets for each engine version.
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more than the @MaxRecords@ value is available, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
mkDescribeDBEngineVersions ::
  DescribeDBEngineVersions
mkDescribeDBEngineVersions =
  DescribeDBEngineVersions'
    { engineVersion = Lude.Nothing,
      listSupportedTimezones = Lude.Nothing,
      defaultOnly = Lude.Nothing,
      includeAll = Lude.Nothing,
      filters = Lude.Nothing,
      engine = Lude.Nothing,
      dbParameterGroupFamily = Lude.Nothing,
      listSupportedCharacterSets = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | The database engine version to return.
--
-- Example: @5.1.49@
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddevEngineVersion :: Lens.Lens' DescribeDBEngineVersions (Lude.Maybe Lude.Text)
ddevEngineVersion = Lens.lens (engineVersion :: DescribeDBEngineVersions -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: DescribeDBEngineVersions)
{-# DEPRECATED ddevEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | A value that indicates whether to list the supported time zones for each engine version.
--
-- If this parameter is enabled and the requested engine supports the @TimeZone@ parameter for @CreateDBInstance@ , the response includes a list of supported time zones for each engine version.
--
-- /Note:/ Consider using 'listSupportedTimezones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddevListSupportedTimezones :: Lens.Lens' DescribeDBEngineVersions (Lude.Maybe Lude.Bool)
ddevListSupportedTimezones = Lens.lens (listSupportedTimezones :: DescribeDBEngineVersions -> Lude.Maybe Lude.Bool) (\s a -> s {listSupportedTimezones = a} :: DescribeDBEngineVersions)
{-# DEPRECATED ddevListSupportedTimezones "Use generic-lens or generic-optics with 'listSupportedTimezones' instead." #-}

-- | A value that indicates whether only the default version of the specified engine or engine and major version combination is returned.
--
-- /Note:/ Consider using 'defaultOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddevDefaultOnly :: Lens.Lens' DescribeDBEngineVersions (Lude.Maybe Lude.Bool)
ddevDefaultOnly = Lens.lens (defaultOnly :: DescribeDBEngineVersions -> Lude.Maybe Lude.Bool) (\s a -> s {defaultOnly = a} :: DescribeDBEngineVersions)
{-# DEPRECATED ddevDefaultOnly "Use generic-lens or generic-optics with 'defaultOnly' instead." #-}

-- | A value that indicates whether to include engine versions that aren't available in the list. The default is to list only available engine versions.
--
-- /Note:/ Consider using 'includeAll' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddevIncludeAll :: Lens.Lens' DescribeDBEngineVersions (Lude.Maybe Lude.Bool)
ddevIncludeAll = Lens.lens (includeAll :: DescribeDBEngineVersions -> Lude.Maybe Lude.Bool) (\s a -> s {includeAll = a} :: DescribeDBEngineVersions)
{-# DEPRECATED ddevIncludeAll "Use generic-lens or generic-optics with 'includeAll' instead." #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddevFilters :: Lens.Lens' DescribeDBEngineVersions (Lude.Maybe [Filter])
ddevFilters = Lens.lens (filters :: DescribeDBEngineVersions -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeDBEngineVersions)
{-# DEPRECATED ddevFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The database engine to return.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddevEngine :: Lens.Lens' DescribeDBEngineVersions (Lude.Maybe Lude.Text)
ddevEngine = Lens.lens (engine :: DescribeDBEngineVersions -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: DescribeDBEngineVersions)
{-# DEPRECATED ddevEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The name of a specific DB parameter group family to return details for.
--
-- Constraints:
--
--     * If supplied, must match an existing DBParameterGroupFamily.
--
--
--
-- /Note:/ Consider using 'dbParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddevDBParameterGroupFamily :: Lens.Lens' DescribeDBEngineVersions (Lude.Maybe Lude.Text)
ddevDBParameterGroupFamily = Lens.lens (dbParameterGroupFamily :: DescribeDBEngineVersions -> Lude.Maybe Lude.Text) (\s a -> s {dbParameterGroupFamily = a} :: DescribeDBEngineVersions)
{-# DEPRECATED ddevDBParameterGroupFamily "Use generic-lens or generic-optics with 'dbParameterGroupFamily' instead." #-}

-- | A value that indicates whether to list the supported character sets for each engine version.
--
-- If this parameter is enabled and the requested engine supports the @CharacterSetName@ parameter for @CreateDBInstance@ , the response includes a list of supported character sets for each engine version.
--
-- /Note:/ Consider using 'listSupportedCharacterSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddevListSupportedCharacterSets :: Lens.Lens' DescribeDBEngineVersions (Lude.Maybe Lude.Bool)
ddevListSupportedCharacterSets = Lens.lens (listSupportedCharacterSets :: DescribeDBEngineVersions -> Lude.Maybe Lude.Bool) (\s a -> s {listSupportedCharacterSets = a} :: DescribeDBEngineVersions)
{-# DEPRECATED ddevListSupportedCharacterSets "Use generic-lens or generic-optics with 'listSupportedCharacterSets' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddevMarker :: Lens.Lens' DescribeDBEngineVersions (Lude.Maybe Lude.Text)
ddevMarker = Lens.lens (marker :: DescribeDBEngineVersions -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBEngineVersions)
{-# DEPRECATED ddevMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more than the @MaxRecords@ value is available, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddevMaxRecords :: Lens.Lens' DescribeDBEngineVersions (Lude.Maybe Lude.Int)
ddevMaxRecords = Lens.lens (maxRecords :: DescribeDBEngineVersions -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeDBEngineVersions)
{-# DEPRECATED ddevMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeDBEngineVersions where
  page rq rs
    | Page.stop (rs Lens.^. ddevrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. ddevrsDBEngineVersions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddevMarker Lens..~ rs Lens.^. ddevrsMarker

instance Lude.AWSRequest DescribeDBEngineVersions where
  type Rs DescribeDBEngineVersions = DescribeDBEngineVersionsResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeDBEngineVersionsResult"
      ( \s h x ->
          DescribeDBEngineVersionsResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "DBEngineVersions" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "DBEngineVersion")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDBEngineVersions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDBEngineVersions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDBEngineVersions where
  toQuery DescribeDBEngineVersions' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeDBEngineVersions" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "EngineVersion" Lude.=: engineVersion,
        "ListSupportedTimezones" Lude.=: listSupportedTimezones,
        "DefaultOnly" Lude.=: defaultOnly,
        "IncludeAll" Lude.=: includeAll,
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "Engine" Lude.=: engine,
        "DBParameterGroupFamily" Lude.=: dbParameterGroupFamily,
        "ListSupportedCharacterSets" Lude.=: listSupportedCharacterSets,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | Contains the result of a successful invocation of the @DescribeDBEngineVersions@ action.
--
-- /See:/ 'mkDescribeDBEngineVersionsResponse' smart constructor.
data DescribeDBEngineVersionsResponse = DescribeDBEngineVersionsResponse'
  { -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | A list of @DBEngineVersion@ elements.
    dbEngineVersions :: Lude.Maybe [DBEngineVersion],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBEngineVersionsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'dbEngineVersions' - A list of @DBEngineVersion@ elements.
-- * 'responseStatus' - The response status code.
mkDescribeDBEngineVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDBEngineVersionsResponse
mkDescribeDBEngineVersionsResponse pResponseStatus_ =
  DescribeDBEngineVersionsResponse'
    { marker = Lude.Nothing,
      dbEngineVersions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddevrsMarker :: Lens.Lens' DescribeDBEngineVersionsResponse (Lude.Maybe Lude.Text)
ddevrsMarker = Lens.lens (marker :: DescribeDBEngineVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBEngineVersionsResponse)
{-# DEPRECATED ddevrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of @DBEngineVersion@ elements.
--
-- /Note:/ Consider using 'dbEngineVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddevrsDBEngineVersions :: Lens.Lens' DescribeDBEngineVersionsResponse (Lude.Maybe [DBEngineVersion])
ddevrsDBEngineVersions = Lens.lens (dbEngineVersions :: DescribeDBEngineVersionsResponse -> Lude.Maybe [DBEngineVersion]) (\s a -> s {dbEngineVersions = a} :: DescribeDBEngineVersionsResponse)
{-# DEPRECATED ddevrsDBEngineVersions "Use generic-lens or generic-optics with 'dbEngineVersions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddevrsResponseStatus :: Lens.Lens' DescribeDBEngineVersionsResponse Lude.Int
ddevrsResponseStatus = Lens.lens (responseStatus :: DescribeDBEngineVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDBEngineVersionsResponse)
{-# DEPRECATED ddevrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
