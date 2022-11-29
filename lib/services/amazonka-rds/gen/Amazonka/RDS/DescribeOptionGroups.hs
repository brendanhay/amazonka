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
-- Module      : Amazonka.RDS.DescribeOptionGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available option groups.
--
-- This operation returns paginated results.
module Amazonka.RDS.DescribeOptionGroups
  ( -- * Creating a Request
    DescribeOptionGroups (..),
    newDescribeOptionGroups,

    -- * Request Lenses
    describeOptionGroups_engineName,
    describeOptionGroups_marker,
    describeOptionGroups_filters,
    describeOptionGroups_optionGroupName,
    describeOptionGroups_maxRecords,
    describeOptionGroups_majorEngineVersion,

    -- * Destructuring the Response
    DescribeOptionGroupsResponse (..),
    newDescribeOptionGroupsResponse,

    -- * Response Lenses
    describeOptionGroupsResponse_optionGroupsList,
    describeOptionGroupsResponse_marker,
    describeOptionGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
    -- | An optional pagination token provided by a previous DescribeOptionGroups
    -- request. If this parameter is specified, the response includes only
    -- records beyond the marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | This parameter isn\'t currently supported.
    filters :: Prelude.Maybe [Filter],
    -- | The name of the option group to describe. Can\'t be supplied together
    -- with EngineName or MajorEngineVersion.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that you can retrieve the
    -- remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | Filters the list of option groups to only include groups associated with
    -- a specific database engine version. If specified, then EngineName must
    -- also be specified.
    majorEngineVersion :: Prelude.Maybe Prelude.Text
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
-- 'marker', 'describeOptionGroups_marker' - An optional pagination token provided by a previous DescribeOptionGroups
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by @MaxRecords@.
--
-- 'filters', 'describeOptionGroups_filters' - This parameter isn\'t currently supported.
--
-- 'optionGroupName', 'describeOptionGroups_optionGroupName' - The name of the option group to describe. Can\'t be supplied together
-- with EngineName or MajorEngineVersion.
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
-- 'majorEngineVersion', 'describeOptionGroups_majorEngineVersion' - Filters the list of option groups to only include groups associated with
-- a specific database engine version. If specified, then EngineName must
-- also be specified.
newDescribeOptionGroups ::
  DescribeOptionGroups
newDescribeOptionGroups =
  DescribeOptionGroups'
    { engineName = Prelude.Nothing,
      marker = Prelude.Nothing,
      filters = Prelude.Nothing,
      optionGroupName = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      majorEngineVersion = Prelude.Nothing
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

-- | An optional pagination token provided by a previous DescribeOptionGroups
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by @MaxRecords@.
describeOptionGroups_marker :: Lens.Lens' DescribeOptionGroups (Prelude.Maybe Prelude.Text)
describeOptionGroups_marker = Lens.lens (\DescribeOptionGroups' {marker} -> marker) (\s@DescribeOptionGroups' {} a -> s {marker = a} :: DescribeOptionGroups)

-- | This parameter isn\'t currently supported.
describeOptionGroups_filters :: Lens.Lens' DescribeOptionGroups (Prelude.Maybe [Filter])
describeOptionGroups_filters = Lens.lens (\DescribeOptionGroups' {filters} -> filters) (\s@DescribeOptionGroups' {} a -> s {filters = a} :: DescribeOptionGroups) Prelude.. Lens.mapping Lens.coerced

-- | The name of the option group to describe. Can\'t be supplied together
-- with EngineName or MajorEngineVersion.
describeOptionGroups_optionGroupName :: Lens.Lens' DescribeOptionGroups (Prelude.Maybe Prelude.Text)
describeOptionGroups_optionGroupName = Lens.lens (\DescribeOptionGroups' {optionGroupName} -> optionGroupName) (\s@DescribeOptionGroups' {} a -> s {optionGroupName = a} :: DescribeOptionGroups)

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

-- | Filters the list of option groups to only include groups associated with
-- a specific database engine version. If specified, then EngineName must
-- also be specified.
describeOptionGroups_majorEngineVersion :: Lens.Lens' DescribeOptionGroups (Prelude.Maybe Prelude.Text)
describeOptionGroups_majorEngineVersion = Lens.lens (\DescribeOptionGroups' {majorEngineVersion} -> majorEngineVersion) (\s@DescribeOptionGroups' {} a -> s {majorEngineVersion = a} :: DescribeOptionGroups)

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeOptionGroupsResult"
      ( \s h x ->
          DescribeOptionGroupsResponse'
            Prelude.<$> ( x Core..@? "OptionGroupsList"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "OptionGroup")
                        )
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeOptionGroups where
  hashWithSalt _salt DescribeOptionGroups' {..} =
    _salt `Prelude.hashWithSalt` engineName
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` majorEngineVersion

instance Prelude.NFData DescribeOptionGroups where
  rnf DescribeOptionGroups' {..} =
    Prelude.rnf engineName
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf optionGroupName
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf majorEngineVersion

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
        "EngineName" Core.=: engineName,
        "Marker" Core.=: marker,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Prelude.<$> filters),
        "OptionGroupName" Core.=: optionGroupName,
        "MaxRecords" Core.=: maxRecords,
        "MajorEngineVersion" Core.=: majorEngineVersion
      ]

-- | List of option groups.
--
-- /See:/ 'newDescribeOptionGroupsResponse' smart constructor.
data DescribeOptionGroupsResponse = DescribeOptionGroupsResponse'
  { -- | List of option groups.
    optionGroupsList :: Prelude.Maybe [OptionGroup],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
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
-- 'optionGroupsList', 'describeOptionGroupsResponse_optionGroupsList' - List of option groups.
--
-- 'marker', 'describeOptionGroupsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeOptionGroupsResponse_httpStatus' - The response's http status code.
newDescribeOptionGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeOptionGroupsResponse
newDescribeOptionGroupsResponse pHttpStatus_ =
  DescribeOptionGroupsResponse'
    { optionGroupsList =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of option groups.
describeOptionGroupsResponse_optionGroupsList :: Lens.Lens' DescribeOptionGroupsResponse (Prelude.Maybe [OptionGroup])
describeOptionGroupsResponse_optionGroupsList = Lens.lens (\DescribeOptionGroupsResponse' {optionGroupsList} -> optionGroupsList) (\s@DescribeOptionGroupsResponse' {} a -> s {optionGroupsList = a} :: DescribeOptionGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeOptionGroupsResponse_marker :: Lens.Lens' DescribeOptionGroupsResponse (Prelude.Maybe Prelude.Text)
describeOptionGroupsResponse_marker = Lens.lens (\DescribeOptionGroupsResponse' {marker} -> marker) (\s@DescribeOptionGroupsResponse' {} a -> s {marker = a} :: DescribeOptionGroupsResponse)

-- | The response's http status code.
describeOptionGroupsResponse_httpStatus :: Lens.Lens' DescribeOptionGroupsResponse Prelude.Int
describeOptionGroupsResponse_httpStatus = Lens.lens (\DescribeOptionGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeOptionGroupsResponse' {} a -> s {httpStatus = a} :: DescribeOptionGroupsResponse)

instance Prelude.NFData DescribeOptionGroupsResponse where
  rnf DescribeOptionGroupsResponse' {..} =
    Prelude.rnf optionGroupsList
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
