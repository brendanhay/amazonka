{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeOptionGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available option groups.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeOptionGroups
  ( -- * Creating a Request
    DescribeOptionGroups (..),
    newDescribeOptionGroups,

    -- * Request Lenses
    describeOptionGroups_engineName,
    describeOptionGroups_optionGroupName,
    describeOptionGroups_majorEngineVersion,
    describeOptionGroups_filters,
    describeOptionGroups_marker,
    describeOptionGroups_maxRecords,

    -- * Destructuring the Response
    DescribeOptionGroupsResponse (..),
    newDescribeOptionGroupsResponse,

    -- * Response Lenses
    describeOptionGroupsResponse_optionGroupsList,
    describeOptionGroupsResponse_marker,
    describeOptionGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeOptionGroups' smart constructor.
data DescribeOptionGroups = DescribeOptionGroups'
  { -- | Filters the list of option groups to only include groups associated with
    -- a specific database engine.
    --
    -- Valid Values:
    --
    -- -   @mariadb@
    --
    -- -   @mysql@
    --
    -- -   @oracle-ee@
    --
    -- -   @oracle-se2@
    --
    -- -   @oracle-se1@
    --
    -- -   @oracle-se@
    --
    -- -   @postgres@
    --
    -- -   @sqlserver-ee@
    --
    -- -   @sqlserver-se@
    --
    -- -   @sqlserver-ex@
    --
    -- -   @sqlserver-web@
    engineName :: Core.Maybe Core.Text,
    -- | The name of the option group to describe. Can\'t be supplied together
    -- with EngineName or MajorEngineVersion.
    optionGroupName :: Core.Maybe Core.Text,
    -- | Filters the list of option groups to only include groups associated with
    -- a specific database engine version. If specified, then EngineName must
    -- also be specified.
    majorEngineVersion :: Core.Maybe Core.Text,
    -- | This parameter isn\'t currently supported.
    filters :: Core.Maybe [Filter],
    -- | An optional pagination token provided by a previous DescribeOptionGroups
    -- request. If this parameter is specified, the response includes only
    -- records beyond the marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that you can retrieve the
    -- remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeOptionGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineName', 'describeOptionGroups_engineName' - Filters the list of option groups to only include groups associated with
-- a specific database engine.
--
-- Valid Values:
--
-- -   @mariadb@
--
-- -   @mysql@
--
-- -   @oracle-ee@
--
-- -   @oracle-se2@
--
-- -   @oracle-se1@
--
-- -   @oracle-se@
--
-- -   @postgres@
--
-- -   @sqlserver-ee@
--
-- -   @sqlserver-se@
--
-- -   @sqlserver-ex@
--
-- -   @sqlserver-web@
--
-- 'optionGroupName', 'describeOptionGroups_optionGroupName' - The name of the option group to describe. Can\'t be supplied together
-- with EngineName or MajorEngineVersion.
--
-- 'majorEngineVersion', 'describeOptionGroups_majorEngineVersion' - Filters the list of option groups to only include groups associated with
-- a specific database engine version. If specified, then EngineName must
-- also be specified.
--
-- 'filters', 'describeOptionGroups_filters' - This parameter isn\'t currently supported.
--
-- 'marker', 'describeOptionGroups_marker' - An optional pagination token provided by a previous DescribeOptionGroups
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeOptionGroups_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeOptionGroups ::
  DescribeOptionGroups
newDescribeOptionGroups =
  DescribeOptionGroups'
    { engineName = Core.Nothing,
      optionGroupName = Core.Nothing,
      majorEngineVersion = Core.Nothing,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | Filters the list of option groups to only include groups associated with
-- a specific database engine.
--
-- Valid Values:
--
-- -   @mariadb@
--
-- -   @mysql@
--
-- -   @oracle-ee@
--
-- -   @oracle-se2@
--
-- -   @oracle-se1@
--
-- -   @oracle-se@
--
-- -   @postgres@
--
-- -   @sqlserver-ee@
--
-- -   @sqlserver-se@
--
-- -   @sqlserver-ex@
--
-- -   @sqlserver-web@
describeOptionGroups_engineName :: Lens.Lens' DescribeOptionGroups (Core.Maybe Core.Text)
describeOptionGroups_engineName = Lens.lens (\DescribeOptionGroups' {engineName} -> engineName) (\s@DescribeOptionGroups' {} a -> s {engineName = a} :: DescribeOptionGroups)

-- | The name of the option group to describe. Can\'t be supplied together
-- with EngineName or MajorEngineVersion.
describeOptionGroups_optionGroupName :: Lens.Lens' DescribeOptionGroups (Core.Maybe Core.Text)
describeOptionGroups_optionGroupName = Lens.lens (\DescribeOptionGroups' {optionGroupName} -> optionGroupName) (\s@DescribeOptionGroups' {} a -> s {optionGroupName = a} :: DescribeOptionGroups)

-- | Filters the list of option groups to only include groups associated with
-- a specific database engine version. If specified, then EngineName must
-- also be specified.
describeOptionGroups_majorEngineVersion :: Lens.Lens' DescribeOptionGroups (Core.Maybe Core.Text)
describeOptionGroups_majorEngineVersion = Lens.lens (\DescribeOptionGroups' {majorEngineVersion} -> majorEngineVersion) (\s@DescribeOptionGroups' {} a -> s {majorEngineVersion = a} :: DescribeOptionGroups)

-- | This parameter isn\'t currently supported.
describeOptionGroups_filters :: Lens.Lens' DescribeOptionGroups (Core.Maybe [Filter])
describeOptionGroups_filters = Lens.lens (\DescribeOptionGroups' {filters} -> filters) (\s@DescribeOptionGroups' {} a -> s {filters = a} :: DescribeOptionGroups) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous DescribeOptionGroups
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by @MaxRecords@.
describeOptionGroups_marker :: Lens.Lens' DescribeOptionGroups (Core.Maybe Core.Text)
describeOptionGroups_marker = Lens.lens (\DescribeOptionGroups' {marker} -> marker) (\s@DescribeOptionGroups' {} a -> s {marker = a} :: DescribeOptionGroups)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeOptionGroups_maxRecords :: Lens.Lens' DescribeOptionGroups (Core.Maybe Core.Int)
describeOptionGroups_maxRecords = Lens.lens (\DescribeOptionGroups' {maxRecords} -> maxRecords) (\s@DescribeOptionGroups' {} a -> s {maxRecords = a} :: DescribeOptionGroups)

instance Core.AWSPager DescribeOptionGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeOptionGroupsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeOptionGroupsResponse_optionGroupsList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeOptionGroups_marker
          Lens..~ rs
          Lens.^? describeOptionGroupsResponse_marker Core.. Lens._Just

instance Core.AWSRequest DescribeOptionGroups where
  type
    AWSResponse DescribeOptionGroups =
      DescribeOptionGroupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeOptionGroupsResult"
      ( \s h x ->
          DescribeOptionGroupsResponse'
            Core.<$> ( x Core..@? "OptionGroupsList" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "OptionGroup")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeOptionGroups

instance Core.NFData DescribeOptionGroups

instance Core.ToHeaders DescribeOptionGroups where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeOptionGroups where
  toPath = Core.const "/"

instance Core.ToQuery DescribeOptionGroups where
  toQuery DescribeOptionGroups' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeOptionGroups" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "EngineName" Core.=: engineName,
        "OptionGroupName" Core.=: optionGroupName,
        "MajorEngineVersion" Core.=: majorEngineVersion,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Core.<$> filters),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | List of option groups.
--
-- /See:/ 'newDescribeOptionGroupsResponse' smart constructor.
data DescribeOptionGroupsResponse = DescribeOptionGroupsResponse'
  { -- | List of option groups.
    optionGroupsList :: Core.Maybe [OptionGroup],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeOptionGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optionGroupsList', 'describeOptionGroupsResponse_optionGroupsList' - List of option groups.
--
-- 'marker', 'describeOptionGroupsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeOptionGroupsResponse_httpStatus' - The response's http status code.
newDescribeOptionGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeOptionGroupsResponse
newDescribeOptionGroupsResponse pHttpStatus_ =
  DescribeOptionGroupsResponse'
    { optionGroupsList =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of option groups.
describeOptionGroupsResponse_optionGroupsList :: Lens.Lens' DescribeOptionGroupsResponse (Core.Maybe [OptionGroup])
describeOptionGroupsResponse_optionGroupsList = Lens.lens (\DescribeOptionGroupsResponse' {optionGroupsList} -> optionGroupsList) (\s@DescribeOptionGroupsResponse' {} a -> s {optionGroupsList = a} :: DescribeOptionGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeOptionGroupsResponse_marker :: Lens.Lens' DescribeOptionGroupsResponse (Core.Maybe Core.Text)
describeOptionGroupsResponse_marker = Lens.lens (\DescribeOptionGroupsResponse' {marker} -> marker) (\s@DescribeOptionGroupsResponse' {} a -> s {marker = a} :: DescribeOptionGroupsResponse)

-- | The response's http status code.
describeOptionGroupsResponse_httpStatus :: Lens.Lens' DescribeOptionGroupsResponse Core.Int
describeOptionGroupsResponse_httpStatus = Lens.lens (\DescribeOptionGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeOptionGroupsResponse' {} a -> s {httpStatus = a} :: DescribeOptionGroupsResponse)

instance Core.NFData DescribeOptionGroupsResponse
