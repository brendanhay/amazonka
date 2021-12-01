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
-- Module      : Amazonka.DocumentDB.DescribeDBEngineVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the available engines.
--
-- This operation returns paginated results.
module Amazonka.DocumentDB.DescribeDBEngineVersions
  ( -- * Creating a Request
    DescribeDBEngineVersions (..),
    newDescribeDBEngineVersions,

    -- * Request Lenses
    describeDBEngineVersions_engineVersion,
    describeDBEngineVersions_listSupportedTimezones,
    describeDBEngineVersions_defaultOnly,
    describeDBEngineVersions_filters,
    describeDBEngineVersions_engine,
    describeDBEngineVersions_dbParameterGroupFamily,
    describeDBEngineVersions_listSupportedCharacterSets,
    describeDBEngineVersions_marker,
    describeDBEngineVersions_maxRecords,

    -- * Destructuring the Response
    DescribeDBEngineVersionsResponse (..),
    newDescribeDBEngineVersionsResponse,

    -- * Response Lenses
    describeDBEngineVersionsResponse_marker,
    describeDBEngineVersionsResponse_dbEngineVersions,
    describeDBEngineVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.DocumentDB.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input to DescribeDBEngineVersions.
--
-- /See:/ 'newDescribeDBEngineVersions' smart constructor.
data DescribeDBEngineVersions = DescribeDBEngineVersions'
  { -- | The database engine version to return.
    --
    -- Example: @3.6.0@
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | If this parameter is specified and the requested engine supports the
    -- @TimeZone@ parameter for @CreateDBInstance@, the response includes a
    -- list of supported time zones for each engine version.
    listSupportedTimezones :: Prelude.Maybe Prelude.Bool,
    -- | Indicates that only the default version of the specified engine or
    -- engine and major version combination is returned.
    defaultOnly :: Prelude.Maybe Prelude.Bool,
    -- | This parameter is not currently supported.
    filters :: Prelude.Maybe [Filter],
    -- | The database engine to return.
    engine :: Prelude.Maybe Prelude.Text,
    -- | The name of a specific parameter group family to return details for.
    --
    -- Constraints:
    --
    -- -   If provided, must match an existing @DBParameterGroupFamily@.
    dbParameterGroupFamily :: Prelude.Maybe Prelude.Text,
    -- | If this parameter is specified and the requested engine supports the
    -- @CharacterSetName@ parameter for @CreateDBInstance@, the response
    -- includes a list of supported character sets for each engine version.
    listSupportedCharacterSets :: Prelude.Maybe Prelude.Bool,
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- (marker) is included in the response so that the remaining results can
    -- be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDBEngineVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineVersion', 'describeDBEngineVersions_engineVersion' - The database engine version to return.
--
-- Example: @3.6.0@
--
-- 'listSupportedTimezones', 'describeDBEngineVersions_listSupportedTimezones' - If this parameter is specified and the requested engine supports the
-- @TimeZone@ parameter for @CreateDBInstance@, the response includes a
-- list of supported time zones for each engine version.
--
-- 'defaultOnly', 'describeDBEngineVersions_defaultOnly' - Indicates that only the default version of the specified engine or
-- engine and major version combination is returned.
--
-- 'filters', 'describeDBEngineVersions_filters' - This parameter is not currently supported.
--
-- 'engine', 'describeDBEngineVersions_engine' - The database engine to return.
--
-- 'dbParameterGroupFamily', 'describeDBEngineVersions_dbParameterGroupFamily' - The name of a specific parameter group family to return details for.
--
-- Constraints:
--
-- -   If provided, must match an existing @DBParameterGroupFamily@.
--
-- 'listSupportedCharacterSets', 'describeDBEngineVersions_listSupportedCharacterSets' - If this parameter is specified and the requested engine supports the
-- @CharacterSetName@ parameter for @CreateDBInstance@, the response
-- includes a list of supported character sets for each engine version.
--
-- 'marker', 'describeDBEngineVersions_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeDBEngineVersions_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- (marker) is included in the response so that the remaining results can
-- be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeDBEngineVersions ::
  DescribeDBEngineVersions
newDescribeDBEngineVersions =
  DescribeDBEngineVersions'
    { engineVersion =
        Prelude.Nothing,
      listSupportedTimezones = Prelude.Nothing,
      defaultOnly = Prelude.Nothing,
      filters = Prelude.Nothing,
      engine = Prelude.Nothing,
      dbParameterGroupFamily = Prelude.Nothing,
      listSupportedCharacterSets = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The database engine version to return.
--
-- Example: @3.6.0@
describeDBEngineVersions_engineVersion :: Lens.Lens' DescribeDBEngineVersions (Prelude.Maybe Prelude.Text)
describeDBEngineVersions_engineVersion = Lens.lens (\DescribeDBEngineVersions' {engineVersion} -> engineVersion) (\s@DescribeDBEngineVersions' {} a -> s {engineVersion = a} :: DescribeDBEngineVersions)

-- | If this parameter is specified and the requested engine supports the
-- @TimeZone@ parameter for @CreateDBInstance@, the response includes a
-- list of supported time zones for each engine version.
describeDBEngineVersions_listSupportedTimezones :: Lens.Lens' DescribeDBEngineVersions (Prelude.Maybe Prelude.Bool)
describeDBEngineVersions_listSupportedTimezones = Lens.lens (\DescribeDBEngineVersions' {listSupportedTimezones} -> listSupportedTimezones) (\s@DescribeDBEngineVersions' {} a -> s {listSupportedTimezones = a} :: DescribeDBEngineVersions)

-- | Indicates that only the default version of the specified engine or
-- engine and major version combination is returned.
describeDBEngineVersions_defaultOnly :: Lens.Lens' DescribeDBEngineVersions (Prelude.Maybe Prelude.Bool)
describeDBEngineVersions_defaultOnly = Lens.lens (\DescribeDBEngineVersions' {defaultOnly} -> defaultOnly) (\s@DescribeDBEngineVersions' {} a -> s {defaultOnly = a} :: DescribeDBEngineVersions)

-- | This parameter is not currently supported.
describeDBEngineVersions_filters :: Lens.Lens' DescribeDBEngineVersions (Prelude.Maybe [Filter])
describeDBEngineVersions_filters = Lens.lens (\DescribeDBEngineVersions' {filters} -> filters) (\s@DescribeDBEngineVersions' {} a -> s {filters = a} :: DescribeDBEngineVersions) Prelude.. Lens.mapping Lens.coerced

-- | The database engine to return.
describeDBEngineVersions_engine :: Lens.Lens' DescribeDBEngineVersions (Prelude.Maybe Prelude.Text)
describeDBEngineVersions_engine = Lens.lens (\DescribeDBEngineVersions' {engine} -> engine) (\s@DescribeDBEngineVersions' {} a -> s {engine = a} :: DescribeDBEngineVersions)

-- | The name of a specific parameter group family to return details for.
--
-- Constraints:
--
-- -   If provided, must match an existing @DBParameterGroupFamily@.
describeDBEngineVersions_dbParameterGroupFamily :: Lens.Lens' DescribeDBEngineVersions (Prelude.Maybe Prelude.Text)
describeDBEngineVersions_dbParameterGroupFamily = Lens.lens (\DescribeDBEngineVersions' {dbParameterGroupFamily} -> dbParameterGroupFamily) (\s@DescribeDBEngineVersions' {} a -> s {dbParameterGroupFamily = a} :: DescribeDBEngineVersions)

-- | If this parameter is specified and the requested engine supports the
-- @CharacterSetName@ parameter for @CreateDBInstance@, the response
-- includes a list of supported character sets for each engine version.
describeDBEngineVersions_listSupportedCharacterSets :: Lens.Lens' DescribeDBEngineVersions (Prelude.Maybe Prelude.Bool)
describeDBEngineVersions_listSupportedCharacterSets = Lens.lens (\DescribeDBEngineVersions' {listSupportedCharacterSets} -> listSupportedCharacterSets) (\s@DescribeDBEngineVersions' {} a -> s {listSupportedCharacterSets = a} :: DescribeDBEngineVersions)

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBEngineVersions_marker :: Lens.Lens' DescribeDBEngineVersions (Prelude.Maybe Prelude.Text)
describeDBEngineVersions_marker = Lens.lens (\DescribeDBEngineVersions' {marker} -> marker) (\s@DescribeDBEngineVersions' {} a -> s {marker = a} :: DescribeDBEngineVersions)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- (marker) is included in the response so that the remaining results can
-- be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBEngineVersions_maxRecords :: Lens.Lens' DescribeDBEngineVersions (Prelude.Maybe Prelude.Int)
describeDBEngineVersions_maxRecords = Lens.lens (\DescribeDBEngineVersions' {maxRecords} -> maxRecords) (\s@DescribeDBEngineVersions' {} a -> s {maxRecords = a} :: DescribeDBEngineVersions)

instance Core.AWSPager DescribeDBEngineVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBEngineVersionsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBEngineVersionsResponse_dbEngineVersions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeDBEngineVersions_marker
          Lens..~ rs
          Lens.^? describeDBEngineVersionsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeDBEngineVersions where
  type
    AWSResponse DescribeDBEngineVersions =
      DescribeDBEngineVersionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeDBEngineVersionsResult"
      ( \s h x ->
          DescribeDBEngineVersionsResponse'
            Prelude.<$> (x Core..@? "Marker")
            Prelude.<*> ( x Core..@? "DBEngineVersions"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "DBEngineVersion")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDBEngineVersions where
  hashWithSalt salt' DescribeDBEngineVersions' {..} =
    salt' `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` listSupportedCharacterSets
      `Prelude.hashWithSalt` dbParameterGroupFamily
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` defaultOnly
      `Prelude.hashWithSalt` listSupportedTimezones
      `Prelude.hashWithSalt` engineVersion

instance Prelude.NFData DescribeDBEngineVersions where
  rnf DescribeDBEngineVersions' {..} =
    Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf listSupportedCharacterSets
      `Prelude.seq` Prelude.rnf dbParameterGroupFamily
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf defaultOnly
      `Prelude.seq` Prelude.rnf listSupportedTimezones

instance Core.ToHeaders DescribeDBEngineVersions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeDBEngineVersions where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeDBEngineVersions where
  toQuery DescribeDBEngineVersions' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeDBEngineVersions" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "EngineVersion" Core.=: engineVersion,
        "ListSupportedTimezones"
          Core.=: listSupportedTimezones,
        "DefaultOnly" Core.=: defaultOnly,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Prelude.<$> filters),
        "Engine" Core.=: engine,
        "DBParameterGroupFamily"
          Core.=: dbParameterGroupFamily,
        "ListSupportedCharacterSets"
          Core.=: listSupportedCharacterSets,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | Represents the output of DescribeDBEngineVersions.
--
-- /See:/ 'newDescribeDBEngineVersionsResponse' smart constructor.
data DescribeDBEngineVersionsResponse = DescribeDBEngineVersionsResponse'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Detailed information about one or more engine versions.
    dbEngineVersions :: Prelude.Maybe [DBEngineVersion],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDBEngineVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeDBEngineVersionsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'dbEngineVersions', 'describeDBEngineVersionsResponse_dbEngineVersions' - Detailed information about one or more engine versions.
--
-- 'httpStatus', 'describeDBEngineVersionsResponse_httpStatus' - The response's http status code.
newDescribeDBEngineVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDBEngineVersionsResponse
newDescribeDBEngineVersionsResponse pHttpStatus_ =
  DescribeDBEngineVersionsResponse'
    { marker =
        Prelude.Nothing,
      dbEngineVersions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBEngineVersionsResponse_marker :: Lens.Lens' DescribeDBEngineVersionsResponse (Prelude.Maybe Prelude.Text)
describeDBEngineVersionsResponse_marker = Lens.lens (\DescribeDBEngineVersionsResponse' {marker} -> marker) (\s@DescribeDBEngineVersionsResponse' {} a -> s {marker = a} :: DescribeDBEngineVersionsResponse)

-- | Detailed information about one or more engine versions.
describeDBEngineVersionsResponse_dbEngineVersions :: Lens.Lens' DescribeDBEngineVersionsResponse (Prelude.Maybe [DBEngineVersion])
describeDBEngineVersionsResponse_dbEngineVersions = Lens.lens (\DescribeDBEngineVersionsResponse' {dbEngineVersions} -> dbEngineVersions) (\s@DescribeDBEngineVersionsResponse' {} a -> s {dbEngineVersions = a} :: DescribeDBEngineVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeDBEngineVersionsResponse_httpStatus :: Lens.Lens' DescribeDBEngineVersionsResponse Prelude.Int
describeDBEngineVersionsResponse_httpStatus = Lens.lens (\DescribeDBEngineVersionsResponse' {httpStatus} -> httpStatus) (\s@DescribeDBEngineVersionsResponse' {} a -> s {httpStatus = a} :: DescribeDBEngineVersionsResponse)

instance
  Prelude.NFData
    DescribeDBEngineVersionsResponse
  where
  rnf DescribeDBEngineVersionsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf dbEngineVersions
