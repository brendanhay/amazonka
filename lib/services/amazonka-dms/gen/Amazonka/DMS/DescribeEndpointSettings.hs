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
-- Module      : Amazonka.DMS.DescribeEndpointSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the possible endpoint settings available when
-- you create an endpoint for a specific database engine.
module Amazonka.DMS.DescribeEndpointSettings
  ( -- * Creating a Request
    DescribeEndpointSettings (..),
    newDescribeEndpointSettings,

    -- * Request Lenses
    describeEndpointSettings_marker,
    describeEndpointSettings_maxRecords,
    describeEndpointSettings_engineName,

    -- * Destructuring the Response
    DescribeEndpointSettingsResponse (..),
    newDescribeEndpointSettingsResponse,

    -- * Response Lenses
    describeEndpointSettingsResponse_endpointSettings,
    describeEndpointSettingsResponse_marker,
    describeEndpointSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEndpointSettings' smart constructor.
data DescribeEndpointSettings = DescribeEndpointSettings'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The databse engine used for your source or target endpoint.
    engineName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEndpointSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeEndpointSettings_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeEndpointSettings_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- 'engineName', 'describeEndpointSettings_engineName' - The databse engine used for your source or target endpoint.
newDescribeEndpointSettings ::
  -- | 'engineName'
  Prelude.Text ->
  DescribeEndpointSettings
newDescribeEndpointSettings pEngineName_ =
  DescribeEndpointSettings'
    { marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      engineName = pEngineName_
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeEndpointSettings_marker :: Lens.Lens' DescribeEndpointSettings (Prelude.Maybe Prelude.Text)
describeEndpointSettings_marker = Lens.lens (\DescribeEndpointSettings' {marker} -> marker) (\s@DescribeEndpointSettings' {} a -> s {marker = a} :: DescribeEndpointSettings)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
describeEndpointSettings_maxRecords :: Lens.Lens' DescribeEndpointSettings (Prelude.Maybe Prelude.Int)
describeEndpointSettings_maxRecords = Lens.lens (\DescribeEndpointSettings' {maxRecords} -> maxRecords) (\s@DescribeEndpointSettings' {} a -> s {maxRecords = a} :: DescribeEndpointSettings)

-- | The databse engine used for your source or target endpoint.
describeEndpointSettings_engineName :: Lens.Lens' DescribeEndpointSettings Prelude.Text
describeEndpointSettings_engineName = Lens.lens (\DescribeEndpointSettings' {engineName} -> engineName) (\s@DescribeEndpointSettings' {} a -> s {engineName = a} :: DescribeEndpointSettings)

instance Core.AWSRequest DescribeEndpointSettings where
  type
    AWSResponse DescribeEndpointSettings =
      DescribeEndpointSettingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEndpointSettingsResponse'
            Prelude.<$> ( x Data..?> "EndpointSettings"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEndpointSettings where
  hashWithSalt _salt DescribeEndpointSettings' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` engineName

instance Prelude.NFData DescribeEndpointSettings where
  rnf DescribeEndpointSettings' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf engineName

instance Data.ToHeaders DescribeEndpointSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.DescribeEndpointSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeEndpointSettings where
  toJSON DescribeEndpointSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Marker" Data..=) Prelude.<$> marker,
            ("MaxRecords" Data..=) Prelude.<$> maxRecords,
            Prelude.Just ("EngineName" Data..= engineName)
          ]
      )

instance Data.ToPath DescribeEndpointSettings where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEndpointSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEndpointSettingsResponse' smart constructor.
data DescribeEndpointSettingsResponse = DescribeEndpointSettingsResponse'
  { -- | Descriptions of the endpoint settings available for your source or
    -- target database engine.
    endpointSettings :: Prelude.Maybe [EndpointSetting],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEndpointSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointSettings', 'describeEndpointSettingsResponse_endpointSettings' - Descriptions of the endpoint settings available for your source or
-- target database engine.
--
-- 'marker', 'describeEndpointSettingsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeEndpointSettingsResponse_httpStatus' - The response's http status code.
newDescribeEndpointSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEndpointSettingsResponse
newDescribeEndpointSettingsResponse pHttpStatus_ =
  DescribeEndpointSettingsResponse'
    { endpointSettings =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Descriptions of the endpoint settings available for your source or
-- target database engine.
describeEndpointSettingsResponse_endpointSettings :: Lens.Lens' DescribeEndpointSettingsResponse (Prelude.Maybe [EndpointSetting])
describeEndpointSettingsResponse_endpointSettings = Lens.lens (\DescribeEndpointSettingsResponse' {endpointSettings} -> endpointSettings) (\s@DescribeEndpointSettingsResponse' {} a -> s {endpointSettings = a} :: DescribeEndpointSettingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeEndpointSettingsResponse_marker :: Lens.Lens' DescribeEndpointSettingsResponse (Prelude.Maybe Prelude.Text)
describeEndpointSettingsResponse_marker = Lens.lens (\DescribeEndpointSettingsResponse' {marker} -> marker) (\s@DescribeEndpointSettingsResponse' {} a -> s {marker = a} :: DescribeEndpointSettingsResponse)

-- | The response's http status code.
describeEndpointSettingsResponse_httpStatus :: Lens.Lens' DescribeEndpointSettingsResponse Prelude.Int
describeEndpointSettingsResponse_httpStatus = Lens.lens (\DescribeEndpointSettingsResponse' {httpStatus} -> httpStatus) (\s@DescribeEndpointSettingsResponse' {} a -> s {httpStatus = a} :: DescribeEndpointSettingsResponse)

instance
  Prelude.NFData
    DescribeEndpointSettingsResponse
  where
  rnf DescribeEndpointSettingsResponse' {..} =
    Prelude.rnf endpointSettings
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
