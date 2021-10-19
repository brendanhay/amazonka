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
    describeOptionGroups_filters,
    describeOptionGroups_engineName,
    describeOptionGroups_majorEngineVersion,
    describeOptionGroups_marker,
    describeOptionGroups_maxRecords,
    describeOptionGroups_optionGroupName,

    -- * Destructuring the Response
    DescribeOptionGroupsResponse (..),
    newDescribeOptionGroupsResponse,

    -- * Response Lenses
    describeOptionGroupsResponse_marker,
    describeOptionGroupsResponse_optionGroupsList,
    describeOptionGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeOptionGroups' smart constructor.
data DescribeOptionGroups = DescribeOptionGroups'
  { -- | This parameter isn\'t currently supported.
    filters :: Prelude.Maybe [Filter],
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
    -- -   @oracle-ee-cdb@
    --
    -- -   @oracle-se2@
    --
    -- -   @oracle-se2-cdb@
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
    engineName :: Prelude.Maybe Prelude.Text,
    -- | Filters the list of option groups to only include groups associated with
    -- a specific database engine version. If specified, then EngineName must
    -- also be specified.
    majorEngineVersion :: Prelude.Maybe Prelude.Text,
    -- | An optional pagination token provided by a previous DescribeOptionGroups
    -- request. If this parameter is specified, the response includes only
    -- records beyond the marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that you can retrieve the
    -- remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The name of the option group to describe. Can\'t be supplied together
    -- with EngineName or MajorEngineVersion.
    optionGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOptionGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeOptionGroups_filters' - This parameter isn\'t currently supported.
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
-- -   @oracle-ee-cdb@
--
-- -   @oracle-se2@
--
-- -   @oracle-se2-cdb@
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
-- 'majorEngineVersion', 'describeOptionGroups_majorEngineVersion' - Filters the list of option groups to only include groups associated with
-- a specific database engine version. If specified, then EngineName must
-- also be specified.
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
--
-- 'optionGroupName', 'describeOptionGroups_optionGroupName' - The name of the option group to describe. Can\'t be supplied together
-- with EngineName or MajorEngineVersion.
newDescribeOptionGroups ::
  DescribeOptionGroups
newDescribeOptionGroups =
  DescribeOptionGroups'
    { filters = Prelude.Nothing,
      engineName = Prelude.Nothing,
      majorEngineVersion = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      optionGroupName = Prelude.Nothing
    }

-- | This parameter isn\'t currently supported.
describeOptionGroups_filters :: Lens.Lens' DescribeOptionGroups (Prelude.Maybe [Filter])
describeOptionGroups_filters = Lens.lens (\DescribeOptionGroups' {filters} -> filters) (\s@DescribeOptionGroups' {} a -> s {filters = a} :: DescribeOptionGroups) Prelude.. Lens.mapping Lens.coerced

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
-- -   @oracle-ee-cdb@
--
-- -   @oracle-se2@
--
-- -   @oracle-se2-cdb@
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
describeOptionGroups_engineName :: Lens.Lens' DescribeOptionGroups (Prelude.Maybe Prelude.Text)
describeOptionGroups_engineName = Lens.lens (\DescribeOptionGroups' {engineName} -> engineName) (\s@DescribeOptionGroups' {} a -> s {engineName = a} :: DescribeOptionGroups)

-- | Filters the list of option groups to only include groups associated with
-- a specific database engine version. If specified, then EngineName must
-- also be specified.
describeOptionGroups_majorEngineVersion :: Lens.Lens' DescribeOptionGroups (Prelude.Maybe Prelude.Text)
describeOptionGroups_majorEngineVersion = Lens.lens (\DescribeOptionGroups' {majorEngineVersion} -> majorEngineVersion) (\s@DescribeOptionGroups' {} a -> s {majorEngineVersion = a} :: DescribeOptionGroups)

-- | An optional pagination token provided by a previous DescribeOptionGroups
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by @MaxRecords@.
describeOptionGroups_marker :: Lens.Lens' DescribeOptionGroups (Prelude.Maybe Prelude.Text)
describeOptionGroups_marker = Lens.lens (\DescribeOptionGroups' {marker} -> marker) (\s@DescribeOptionGroups' {} a -> s {marker = a} :: DescribeOptionGroups)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeOptionGroups_maxRecords :: Lens.Lens' DescribeOptionGroups (Prelude.Maybe Prelude.Int)
describeOptionGroups_maxRecords = Lens.lens (\DescribeOptionGroups' {maxRecords} -> maxRecords) (\s@DescribeOptionGroups' {} a -> s {maxRecords = a} :: DescribeOptionGroups)

-- | The name of the option group to describe. Can\'t be supplied together
-- with EngineName or MajorEngineVersion.
describeOptionGroups_optionGroupName :: Lens.Lens' DescribeOptionGroups (Prelude.Maybe Prelude.Text)
describeOptionGroups_optionGroupName = Lens.lens (\DescribeOptionGroups' {optionGroupName} -> optionGroupName) (\s@DescribeOptionGroups' {} a -> s {optionGroupName = a} :: DescribeOptionGroups)

instance Core.AWSPager DescribeOptionGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeOptionGroupsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeOptionGroupsResponse_optionGroupsList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeOptionGroups_marker
          Lens..~ rs
          Lens.^? describeOptionGroupsResponse_marker
            Prelude.. Lens._Just

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
            Prelude.<$> (x Core..@? "Marker")
            Prelude.<*> ( x Core..@? "OptionGroupsList"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "OptionGroup")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeOptionGroups

instance Prelude.NFData DescribeOptionGroups

instance Core.ToHeaders DescribeOptionGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeOptionGroups where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeOptionGroups where
  toQuery DescribeOptionGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeOptionGroups" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Prelude.<$> filters),
        "EngineName" Core.=: engineName,
        "MajorEngineVersion" Core.=: majorEngineVersion,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords,
        "OptionGroupName" Core.=: optionGroupName
      ]

-- | List of option groups.
--
-- /See:/ 'newDescribeOptionGroupsResponse' smart constructor.
data DescribeOptionGroupsResponse = DescribeOptionGroupsResponse'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | List of option groups.
    optionGroupsList :: Prelude.Maybe [OptionGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOptionGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeOptionGroupsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'optionGroupsList', 'describeOptionGroupsResponse_optionGroupsList' - List of option groups.
--
-- 'httpStatus', 'describeOptionGroupsResponse_httpStatus' - The response's http status code.
newDescribeOptionGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeOptionGroupsResponse
newDescribeOptionGroupsResponse pHttpStatus_ =
  DescribeOptionGroupsResponse'
    { marker =
        Prelude.Nothing,
      optionGroupsList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeOptionGroupsResponse_marker :: Lens.Lens' DescribeOptionGroupsResponse (Prelude.Maybe Prelude.Text)
describeOptionGroupsResponse_marker = Lens.lens (\DescribeOptionGroupsResponse' {marker} -> marker) (\s@DescribeOptionGroupsResponse' {} a -> s {marker = a} :: DescribeOptionGroupsResponse)

-- | List of option groups.
describeOptionGroupsResponse_optionGroupsList :: Lens.Lens' DescribeOptionGroupsResponse (Prelude.Maybe [OptionGroup])
describeOptionGroupsResponse_optionGroupsList = Lens.lens (\DescribeOptionGroupsResponse' {optionGroupsList} -> optionGroupsList) (\s@DescribeOptionGroupsResponse' {} a -> s {optionGroupsList = a} :: DescribeOptionGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeOptionGroupsResponse_httpStatus :: Lens.Lens' DescribeOptionGroupsResponse Prelude.Int
describeOptionGroupsResponse_httpStatus = Lens.lens (\DescribeOptionGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeOptionGroupsResponse' {} a -> s {httpStatus = a} :: DescribeOptionGroupsResponse)

instance Prelude.NFData DescribeOptionGroupsResponse
