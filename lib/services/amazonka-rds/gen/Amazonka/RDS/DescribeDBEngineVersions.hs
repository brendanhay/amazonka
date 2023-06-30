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
-- Module      : Amazonka.RDS.DescribeDBEngineVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the available DB engines.
--
-- This operation returns paginated results.
module Amazonka.RDS.DescribeDBEngineVersions
  ( -- * Creating a Request
    DescribeDBEngineVersions (..),
    newDescribeDBEngineVersions,

    -- * Request Lenses
    describeDBEngineVersions_dbParameterGroupFamily,
    describeDBEngineVersions_defaultOnly,
    describeDBEngineVersions_engine,
    describeDBEngineVersions_engineVersion,
    describeDBEngineVersions_filters,
    describeDBEngineVersions_includeAll,
    describeDBEngineVersions_listSupportedCharacterSets,
    describeDBEngineVersions_listSupportedTimezones,
    describeDBEngineVersions_marker,
    describeDBEngineVersions_maxRecords,

    -- * Destructuring the Response
    DescribeDBEngineVersionsResponse (..),
    newDescribeDBEngineVersionsResponse,

    -- * Response Lenses
    describeDBEngineVersionsResponse_dbEngineVersions,
    describeDBEngineVersionsResponse_marker,
    describeDBEngineVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDBEngineVersions' smart constructor.
data DescribeDBEngineVersions = DescribeDBEngineVersions'
  { -- | The name of a specific DB parameter group family to return details for.
    --
    -- Constraints:
    --
    -- -   If supplied, must match an existing DBParameterGroupFamily.
    dbParameterGroupFamily :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether only the default version of the specified
    -- engine or engine and major version combination is returned.
    defaultOnly :: Prelude.Maybe Prelude.Bool,
    -- | The database engine to return.
    --
    -- Valid Values:
    --
    -- -   @aurora@ (for MySQL 5.6-compatible Aurora)
    --
    -- -   @aurora-mysql@ (for MySQL 5.7-compatible and MySQL 8.0-compatible
    --     Aurora)
    --
    -- -   @aurora-postgresql@
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
    engine :: Prelude.Maybe Prelude.Text,
    -- | The database engine version to return.
    --
    -- Example: @5.1.49@
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | A filter that specifies one or more DB engine versions to describe.
    --
    -- Supported filters:
    --
    -- -   @db-parameter-group-family@ - Accepts parameter groups family names.
    --     The results list only includes information about the DB engine
    --     versions for these parameter group families.
    --
    -- -   @engine@ - Accepts engine names. The results list only includes
    --     information about the DB engine versions for these engines.
    --
    -- -   @engine-mode@ - Accepts DB engine modes. The results list only
    --     includes information about the DB engine versions for these engine
    --     modes. Valid DB engine modes are the following:
    --
    --     -   @global@
    --
    --     -   @multimaster@
    --
    --     -   @parallelquery@
    --
    --     -   @provisioned@
    --
    --     -   @serverless@
    --
    -- -   @engine-version@ - Accepts engine versions. The results list only
    --     includes information about the DB engine versions for these engine
    --     versions.
    --
    -- -   @status@ - Accepts engine version statuses. The results list only
    --     includes information about the DB engine versions for these
    --     statuses. Valid statuses are the following:
    --
    --     -   @available@
    --
    --     -   @deprecated@
    filters :: Prelude.Maybe [Filter],
    -- | A value that indicates whether to include engine versions that aren\'t
    -- available in the list. The default is to list only available engine
    -- versions.
    includeAll :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether to list the supported character sets for
    -- each engine version.
    --
    -- If this parameter is enabled and the requested engine supports the
    -- @CharacterSetName@ parameter for @CreateDBInstance@, the response
    -- includes a list of supported character sets for each engine version.
    --
    -- For RDS Custom, the default is not to list supported character sets. If
    -- you set @ListSupportedCharacterSets@ to @true@, RDS Custom returns no
    -- results.
    listSupportedCharacterSets :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether to list the supported time zones for each
    -- engine version.
    --
    -- If this parameter is enabled and the requested engine supports the
    -- @TimeZone@ parameter for @CreateDBInstance@, the response includes a
    -- list of supported time zones for each engine version.
    --
    -- For RDS Custom, the default is not to list supported time zones. If you
    -- set @ListSupportedTimezones@ to @true@, RDS Custom returns no results.
    listSupportedTimezones :: Prelude.Maybe Prelude.Bool,
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more than
    -- the @MaxRecords@ value is available, a pagination token called a marker
    -- is included in the response so you can retrieve the remaining results.
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
-- 'dbParameterGroupFamily', 'describeDBEngineVersions_dbParameterGroupFamily' - The name of a specific DB parameter group family to return details for.
--
-- Constraints:
--
-- -   If supplied, must match an existing DBParameterGroupFamily.
--
-- 'defaultOnly', 'describeDBEngineVersions_defaultOnly' - A value that indicates whether only the default version of the specified
-- engine or engine and major version combination is returned.
--
-- 'engine', 'describeDBEngineVersions_engine' - The database engine to return.
--
-- Valid Values:
--
-- -   @aurora@ (for MySQL 5.6-compatible Aurora)
--
-- -   @aurora-mysql@ (for MySQL 5.7-compatible and MySQL 8.0-compatible
--     Aurora)
--
-- -   @aurora-postgresql@
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
-- 'engineVersion', 'describeDBEngineVersions_engineVersion' - The database engine version to return.
--
-- Example: @5.1.49@
--
-- 'filters', 'describeDBEngineVersions_filters' - A filter that specifies one or more DB engine versions to describe.
--
-- Supported filters:
--
-- -   @db-parameter-group-family@ - Accepts parameter groups family names.
--     The results list only includes information about the DB engine
--     versions for these parameter group families.
--
-- -   @engine@ - Accepts engine names. The results list only includes
--     information about the DB engine versions for these engines.
--
-- -   @engine-mode@ - Accepts DB engine modes. The results list only
--     includes information about the DB engine versions for these engine
--     modes. Valid DB engine modes are the following:
--
--     -   @global@
--
--     -   @multimaster@
--
--     -   @parallelquery@
--
--     -   @provisioned@
--
--     -   @serverless@
--
-- -   @engine-version@ - Accepts engine versions. The results list only
--     includes information about the DB engine versions for these engine
--     versions.
--
-- -   @status@ - Accepts engine version statuses. The results list only
--     includes information about the DB engine versions for these
--     statuses. Valid statuses are the following:
--
--     -   @available@
--
--     -   @deprecated@
--
-- 'includeAll', 'describeDBEngineVersions_includeAll' - A value that indicates whether to include engine versions that aren\'t
-- available in the list. The default is to list only available engine
-- versions.
--
-- 'listSupportedCharacterSets', 'describeDBEngineVersions_listSupportedCharacterSets' - A value that indicates whether to list the supported character sets for
-- each engine version.
--
-- If this parameter is enabled and the requested engine supports the
-- @CharacterSetName@ parameter for @CreateDBInstance@, the response
-- includes a list of supported character sets for each engine version.
--
-- For RDS Custom, the default is not to list supported character sets. If
-- you set @ListSupportedCharacterSets@ to @true@, RDS Custom returns no
-- results.
--
-- 'listSupportedTimezones', 'describeDBEngineVersions_listSupportedTimezones' - A value that indicates whether to list the supported time zones for each
-- engine version.
--
-- If this parameter is enabled and the requested engine supports the
-- @TimeZone@ parameter for @CreateDBInstance@, the response includes a
-- list of supported time zones for each engine version.
--
-- For RDS Custom, the default is not to list supported time zones. If you
-- set @ListSupportedTimezones@ to @true@, RDS Custom returns no results.
--
-- 'marker', 'describeDBEngineVersions_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeDBEngineVersions_maxRecords' - The maximum number of records to include in the response. If more than
-- the @MaxRecords@ value is available, a pagination token called a marker
-- is included in the response so you can retrieve the remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeDBEngineVersions ::
  DescribeDBEngineVersions
newDescribeDBEngineVersions =
  DescribeDBEngineVersions'
    { dbParameterGroupFamily =
        Prelude.Nothing,
      defaultOnly = Prelude.Nothing,
      engine = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      filters = Prelude.Nothing,
      includeAll = Prelude.Nothing,
      listSupportedCharacterSets = Prelude.Nothing,
      listSupportedTimezones = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The name of a specific DB parameter group family to return details for.
--
-- Constraints:
--
-- -   If supplied, must match an existing DBParameterGroupFamily.
describeDBEngineVersions_dbParameterGroupFamily :: Lens.Lens' DescribeDBEngineVersions (Prelude.Maybe Prelude.Text)
describeDBEngineVersions_dbParameterGroupFamily = Lens.lens (\DescribeDBEngineVersions' {dbParameterGroupFamily} -> dbParameterGroupFamily) (\s@DescribeDBEngineVersions' {} a -> s {dbParameterGroupFamily = a} :: DescribeDBEngineVersions)

-- | A value that indicates whether only the default version of the specified
-- engine or engine and major version combination is returned.
describeDBEngineVersions_defaultOnly :: Lens.Lens' DescribeDBEngineVersions (Prelude.Maybe Prelude.Bool)
describeDBEngineVersions_defaultOnly = Lens.lens (\DescribeDBEngineVersions' {defaultOnly} -> defaultOnly) (\s@DescribeDBEngineVersions' {} a -> s {defaultOnly = a} :: DescribeDBEngineVersions)

-- | The database engine to return.
--
-- Valid Values:
--
-- -   @aurora@ (for MySQL 5.6-compatible Aurora)
--
-- -   @aurora-mysql@ (for MySQL 5.7-compatible and MySQL 8.0-compatible
--     Aurora)
--
-- -   @aurora-postgresql@
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
describeDBEngineVersions_engine :: Lens.Lens' DescribeDBEngineVersions (Prelude.Maybe Prelude.Text)
describeDBEngineVersions_engine = Lens.lens (\DescribeDBEngineVersions' {engine} -> engine) (\s@DescribeDBEngineVersions' {} a -> s {engine = a} :: DescribeDBEngineVersions)

-- | The database engine version to return.
--
-- Example: @5.1.49@
describeDBEngineVersions_engineVersion :: Lens.Lens' DescribeDBEngineVersions (Prelude.Maybe Prelude.Text)
describeDBEngineVersions_engineVersion = Lens.lens (\DescribeDBEngineVersions' {engineVersion} -> engineVersion) (\s@DescribeDBEngineVersions' {} a -> s {engineVersion = a} :: DescribeDBEngineVersions)

-- | A filter that specifies one or more DB engine versions to describe.
--
-- Supported filters:
--
-- -   @db-parameter-group-family@ - Accepts parameter groups family names.
--     The results list only includes information about the DB engine
--     versions for these parameter group families.
--
-- -   @engine@ - Accepts engine names. The results list only includes
--     information about the DB engine versions for these engines.
--
-- -   @engine-mode@ - Accepts DB engine modes. The results list only
--     includes information about the DB engine versions for these engine
--     modes. Valid DB engine modes are the following:
--
--     -   @global@
--
--     -   @multimaster@
--
--     -   @parallelquery@
--
--     -   @provisioned@
--
--     -   @serverless@
--
-- -   @engine-version@ - Accepts engine versions. The results list only
--     includes information about the DB engine versions for these engine
--     versions.
--
-- -   @status@ - Accepts engine version statuses. The results list only
--     includes information about the DB engine versions for these
--     statuses. Valid statuses are the following:
--
--     -   @available@
--
--     -   @deprecated@
describeDBEngineVersions_filters :: Lens.Lens' DescribeDBEngineVersions (Prelude.Maybe [Filter])
describeDBEngineVersions_filters = Lens.lens (\DescribeDBEngineVersions' {filters} -> filters) (\s@DescribeDBEngineVersions' {} a -> s {filters = a} :: DescribeDBEngineVersions) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates whether to include engine versions that aren\'t
-- available in the list. The default is to list only available engine
-- versions.
describeDBEngineVersions_includeAll :: Lens.Lens' DescribeDBEngineVersions (Prelude.Maybe Prelude.Bool)
describeDBEngineVersions_includeAll = Lens.lens (\DescribeDBEngineVersions' {includeAll} -> includeAll) (\s@DescribeDBEngineVersions' {} a -> s {includeAll = a} :: DescribeDBEngineVersions)

-- | A value that indicates whether to list the supported character sets for
-- each engine version.
--
-- If this parameter is enabled and the requested engine supports the
-- @CharacterSetName@ parameter for @CreateDBInstance@, the response
-- includes a list of supported character sets for each engine version.
--
-- For RDS Custom, the default is not to list supported character sets. If
-- you set @ListSupportedCharacterSets@ to @true@, RDS Custom returns no
-- results.
describeDBEngineVersions_listSupportedCharacterSets :: Lens.Lens' DescribeDBEngineVersions (Prelude.Maybe Prelude.Bool)
describeDBEngineVersions_listSupportedCharacterSets = Lens.lens (\DescribeDBEngineVersions' {listSupportedCharacterSets} -> listSupportedCharacterSets) (\s@DescribeDBEngineVersions' {} a -> s {listSupportedCharacterSets = a} :: DescribeDBEngineVersions)

-- | A value that indicates whether to list the supported time zones for each
-- engine version.
--
-- If this parameter is enabled and the requested engine supports the
-- @TimeZone@ parameter for @CreateDBInstance@, the response includes a
-- list of supported time zones for each engine version.
--
-- For RDS Custom, the default is not to list supported time zones. If you
-- set @ListSupportedTimezones@ to @true@, RDS Custom returns no results.
describeDBEngineVersions_listSupportedTimezones :: Lens.Lens' DescribeDBEngineVersions (Prelude.Maybe Prelude.Bool)
describeDBEngineVersions_listSupportedTimezones = Lens.lens (\DescribeDBEngineVersions' {listSupportedTimezones} -> listSupportedTimezones) (\s@DescribeDBEngineVersions' {} a -> s {listSupportedTimezones = a} :: DescribeDBEngineVersions)

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBEngineVersions_marker :: Lens.Lens' DescribeDBEngineVersions (Prelude.Maybe Prelude.Text)
describeDBEngineVersions_marker = Lens.lens (\DescribeDBEngineVersions' {marker} -> marker) (\s@DescribeDBEngineVersions' {} a -> s {marker = a} :: DescribeDBEngineVersions)

-- | The maximum number of records to include in the response. If more than
-- the @MaxRecords@ value is available, a pagination token called a marker
-- is included in the response so you can retrieve the remaining results.
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
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeDBEngineVersions_marker
          Lens..~ rs
          Lens.^? describeDBEngineVersionsResponse_marker
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeDBEngineVersions where
  type
    AWSResponse DescribeDBEngineVersions =
      DescribeDBEngineVersionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeDBEngineVersionsResult"
      ( \s h x ->
          DescribeDBEngineVersionsResponse'
            Prelude.<$> ( x
                            Data..@? "DBEngineVersions"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "DBEngineVersion")
                        )
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDBEngineVersions where
  hashWithSalt _salt DescribeDBEngineVersions' {..} =
    _salt
      `Prelude.hashWithSalt` dbParameterGroupFamily
      `Prelude.hashWithSalt` defaultOnly
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` includeAll
      `Prelude.hashWithSalt` listSupportedCharacterSets
      `Prelude.hashWithSalt` listSupportedTimezones
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords

instance Prelude.NFData DescribeDBEngineVersions where
  rnf DescribeDBEngineVersions' {..} =
    Prelude.rnf dbParameterGroupFamily
      `Prelude.seq` Prelude.rnf defaultOnly
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf includeAll
      `Prelude.seq` Prelude.rnf listSupportedCharacterSets
      `Prelude.seq` Prelude.rnf listSupportedTimezones
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords

instance Data.ToHeaders DescribeDBEngineVersions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDBEngineVersions where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDBEngineVersions where
  toQuery DescribeDBEngineVersions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeDBEngineVersions" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBParameterGroupFamily"
          Data.=: dbParameterGroupFamily,
        "DefaultOnly" Data.=: defaultOnly,
        "Engine" Data.=: engine,
        "EngineVersion" Data.=: engineVersion,
        "Filters"
          Data.=: Data.toQuery
            (Data.toQueryList "Filter" Prelude.<$> filters),
        "IncludeAll" Data.=: includeAll,
        "ListSupportedCharacterSets"
          Data.=: listSupportedCharacterSets,
        "ListSupportedTimezones"
          Data.=: listSupportedTimezones,
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords
      ]

-- | Contains the result of a successful invocation of the
-- @DescribeDBEngineVersions@ action.
--
-- /See:/ 'newDescribeDBEngineVersionsResponse' smart constructor.
data DescribeDBEngineVersionsResponse = DescribeDBEngineVersionsResponse'
  { -- | A list of @DBEngineVersion@ elements.
    dbEngineVersions :: Prelude.Maybe [DBEngineVersion],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
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
-- 'dbEngineVersions', 'describeDBEngineVersionsResponse_dbEngineVersions' - A list of @DBEngineVersion@ elements.
--
-- 'marker', 'describeDBEngineVersionsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeDBEngineVersionsResponse_httpStatus' - The response's http status code.
newDescribeDBEngineVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDBEngineVersionsResponse
newDescribeDBEngineVersionsResponse pHttpStatus_ =
  DescribeDBEngineVersionsResponse'
    { dbEngineVersions =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @DBEngineVersion@ elements.
describeDBEngineVersionsResponse_dbEngineVersions :: Lens.Lens' DescribeDBEngineVersionsResponse (Prelude.Maybe [DBEngineVersion])
describeDBEngineVersionsResponse_dbEngineVersions = Lens.lens (\DescribeDBEngineVersionsResponse' {dbEngineVersions} -> dbEngineVersions) (\s@DescribeDBEngineVersionsResponse' {} a -> s {dbEngineVersions = a} :: DescribeDBEngineVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBEngineVersionsResponse_marker :: Lens.Lens' DescribeDBEngineVersionsResponse (Prelude.Maybe Prelude.Text)
describeDBEngineVersionsResponse_marker = Lens.lens (\DescribeDBEngineVersionsResponse' {marker} -> marker) (\s@DescribeDBEngineVersionsResponse' {} a -> s {marker = a} :: DescribeDBEngineVersionsResponse)

-- | The response's http status code.
describeDBEngineVersionsResponse_httpStatus :: Lens.Lens' DescribeDBEngineVersionsResponse Prelude.Int
describeDBEngineVersionsResponse_httpStatus = Lens.lens (\DescribeDBEngineVersionsResponse' {httpStatus} -> httpStatus) (\s@DescribeDBEngineVersionsResponse' {} a -> s {httpStatus = a} :: DescribeDBEngineVersionsResponse)

instance
  Prelude.NFData
    DescribeDBEngineVersionsResponse
  where
  rnf DescribeDBEngineVersionsResponse' {..} =
    Prelude.rnf dbEngineVersions
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
