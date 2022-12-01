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
-- Module      : Amazonka.RDS.DescribeOptionGroupOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes all available options.
--
-- This operation returns paginated results.
module Amazonka.RDS.DescribeOptionGroupOptions
  ( -- * Creating a Request
    DescribeOptionGroupOptions (..),
    newDescribeOptionGroupOptions,

    -- * Request Lenses
    describeOptionGroupOptions_marker,
    describeOptionGroupOptions_filters,
    describeOptionGroupOptions_maxRecords,
    describeOptionGroupOptions_majorEngineVersion,
    describeOptionGroupOptions_engineName,

    -- * Destructuring the Response
    DescribeOptionGroupOptionsResponse (..),
    newDescribeOptionGroupOptionsResponse,

    -- * Response Lenses
    describeOptionGroupOptionsResponse_optionGroupOptions,
    describeOptionGroupOptionsResponse_marker,
    describeOptionGroupOptionsResponse_httpStatus,
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
-- /See:/ 'newDescribeOptionGroupOptions' smart constructor.
data DescribeOptionGroupOptions = DescribeOptionGroupOptions'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | This parameter isn\'t currently supported.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that you can retrieve the
    -- remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | If specified, filters the results to include only options for the
    -- specified major engine version.
    majorEngineVersion :: Prelude.Maybe Prelude.Text,
    -- | A required parameter. Options available for the given engine name are
    -- described.
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
    engineName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOptionGroupOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeOptionGroupOptions_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'filters', 'describeOptionGroupOptions_filters' - This parameter isn\'t currently supported.
--
-- 'maxRecords', 'describeOptionGroupOptions_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'majorEngineVersion', 'describeOptionGroupOptions_majorEngineVersion' - If specified, filters the results to include only options for the
-- specified major engine version.
--
-- 'engineName', 'describeOptionGroupOptions_engineName' - A required parameter. Options available for the given engine name are
-- described.
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
newDescribeOptionGroupOptions ::
  -- | 'engineName'
  Prelude.Text ->
  DescribeOptionGroupOptions
newDescribeOptionGroupOptions pEngineName_ =
  DescribeOptionGroupOptions'
    { marker =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      majorEngineVersion = Prelude.Nothing,
      engineName = pEngineName_
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeOptionGroupOptions_marker :: Lens.Lens' DescribeOptionGroupOptions (Prelude.Maybe Prelude.Text)
describeOptionGroupOptions_marker = Lens.lens (\DescribeOptionGroupOptions' {marker} -> marker) (\s@DescribeOptionGroupOptions' {} a -> s {marker = a} :: DescribeOptionGroupOptions)

-- | This parameter isn\'t currently supported.
describeOptionGroupOptions_filters :: Lens.Lens' DescribeOptionGroupOptions (Prelude.Maybe [Filter])
describeOptionGroupOptions_filters = Lens.lens (\DescribeOptionGroupOptions' {filters} -> filters) (\s@DescribeOptionGroupOptions' {} a -> s {filters = a} :: DescribeOptionGroupOptions) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeOptionGroupOptions_maxRecords :: Lens.Lens' DescribeOptionGroupOptions (Prelude.Maybe Prelude.Int)
describeOptionGroupOptions_maxRecords = Lens.lens (\DescribeOptionGroupOptions' {maxRecords} -> maxRecords) (\s@DescribeOptionGroupOptions' {} a -> s {maxRecords = a} :: DescribeOptionGroupOptions)

-- | If specified, filters the results to include only options for the
-- specified major engine version.
describeOptionGroupOptions_majorEngineVersion :: Lens.Lens' DescribeOptionGroupOptions (Prelude.Maybe Prelude.Text)
describeOptionGroupOptions_majorEngineVersion = Lens.lens (\DescribeOptionGroupOptions' {majorEngineVersion} -> majorEngineVersion) (\s@DescribeOptionGroupOptions' {} a -> s {majorEngineVersion = a} :: DescribeOptionGroupOptions)

-- | A required parameter. Options available for the given engine name are
-- described.
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
describeOptionGroupOptions_engineName :: Lens.Lens' DescribeOptionGroupOptions Prelude.Text
describeOptionGroupOptions_engineName = Lens.lens (\DescribeOptionGroupOptions' {engineName} -> engineName) (\s@DescribeOptionGroupOptions' {} a -> s {engineName = a} :: DescribeOptionGroupOptions)

instance Core.AWSPager DescribeOptionGroupOptions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeOptionGroupOptionsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeOptionGroupOptionsResponse_optionGroupOptions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeOptionGroupOptions_marker
          Lens..~ rs
          Lens.^? describeOptionGroupOptionsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeOptionGroupOptions where
  type
    AWSResponse DescribeOptionGroupOptions =
      DescribeOptionGroupOptionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeOptionGroupOptionsResult"
      ( \s h x ->
          DescribeOptionGroupOptionsResponse'
            Prelude.<$> ( x Core..@? "OptionGroupOptions"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "OptionGroupOption")
                        )
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeOptionGroupOptions where
  hashWithSalt _salt DescribeOptionGroupOptions' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` majorEngineVersion
      `Prelude.hashWithSalt` engineName

instance Prelude.NFData DescribeOptionGroupOptions where
  rnf DescribeOptionGroupOptions' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf majorEngineVersion
      `Prelude.seq` Prelude.rnf engineName

instance Core.ToHeaders DescribeOptionGroupOptions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeOptionGroupOptions where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeOptionGroupOptions where
  toQuery DescribeOptionGroupOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeOptionGroupOptions" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "Marker" Core.=: marker,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Prelude.<$> filters),
        "MaxRecords" Core.=: maxRecords,
        "MajorEngineVersion" Core.=: majorEngineVersion,
        "EngineName" Core.=: engineName
      ]

-- |
--
-- /See:/ 'newDescribeOptionGroupOptionsResponse' smart constructor.
data DescribeOptionGroupOptionsResponse = DescribeOptionGroupOptionsResponse'
  { optionGroupOptions :: Prelude.Maybe [OptionGroupOption],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOptionGroupOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optionGroupOptions', 'describeOptionGroupOptionsResponse_optionGroupOptions' - Undocumented member.
--
-- 'marker', 'describeOptionGroupOptionsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeOptionGroupOptionsResponse_httpStatus' - The response's http status code.
newDescribeOptionGroupOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeOptionGroupOptionsResponse
newDescribeOptionGroupOptionsResponse pHttpStatus_ =
  DescribeOptionGroupOptionsResponse'
    { optionGroupOptions =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeOptionGroupOptionsResponse_optionGroupOptions :: Lens.Lens' DescribeOptionGroupOptionsResponse (Prelude.Maybe [OptionGroupOption])
describeOptionGroupOptionsResponse_optionGroupOptions = Lens.lens (\DescribeOptionGroupOptionsResponse' {optionGroupOptions} -> optionGroupOptions) (\s@DescribeOptionGroupOptionsResponse' {} a -> s {optionGroupOptions = a} :: DescribeOptionGroupOptionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeOptionGroupOptionsResponse_marker :: Lens.Lens' DescribeOptionGroupOptionsResponse (Prelude.Maybe Prelude.Text)
describeOptionGroupOptionsResponse_marker = Lens.lens (\DescribeOptionGroupOptionsResponse' {marker} -> marker) (\s@DescribeOptionGroupOptionsResponse' {} a -> s {marker = a} :: DescribeOptionGroupOptionsResponse)

-- | The response's http status code.
describeOptionGroupOptionsResponse_httpStatus :: Lens.Lens' DescribeOptionGroupOptionsResponse Prelude.Int
describeOptionGroupOptionsResponse_httpStatus = Lens.lens (\DescribeOptionGroupOptionsResponse' {httpStatus} -> httpStatus) (\s@DescribeOptionGroupOptionsResponse' {} a -> s {httpStatus = a} :: DescribeOptionGroupOptionsResponse)

instance
  Prelude.NFData
    DescribeOptionGroupOptionsResponse
  where
  rnf DescribeOptionGroupOptionsResponse' {..} =
    Prelude.rnf optionGroupOptions
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
