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
-- Module      : Amazonka.IoTSiteWise.DescribeTimeSeries
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a time series (data stream).
--
-- To identify a time series, do one of the following:
--
-- -   If the time series isn\'t associated with an asset property, specify
--     the @alias@ of the time series.
--
-- -   If the time series is associated with an asset property, specify one
--     of the following:
--
--     -   The @alias@ of the time series.
--
--     -   The @assetId@ and @propertyId@ that identifies the asset
--         property.
module Amazonka.IoTSiteWise.DescribeTimeSeries
  ( -- * Creating a Request
    DescribeTimeSeries (..),
    newDescribeTimeSeries,

    -- * Request Lenses
    describeTimeSeries_alias,
    describeTimeSeries_assetId,
    describeTimeSeries_propertyId,

    -- * Destructuring the Response
    DescribeTimeSeriesResponse (..),
    newDescribeTimeSeriesResponse,

    -- * Response Lenses
    describeTimeSeriesResponse_alias,
    describeTimeSeriesResponse_assetId,
    describeTimeSeriesResponse_dataTypeSpec,
    describeTimeSeriesResponse_propertyId,
    describeTimeSeriesResponse_httpStatus,
    describeTimeSeriesResponse_timeSeriesId,
    describeTimeSeriesResponse_dataType,
    describeTimeSeriesResponse_timeSeriesCreationDate,
    describeTimeSeriesResponse_timeSeriesLastUpdateDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTimeSeries' smart constructor.
data DescribeTimeSeries = DescribeTimeSeries'
  { -- | The alias that identifies the time series.
    alias :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset in which the asset property was created.
    assetId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset property.
    propertyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTimeSeries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alias', 'describeTimeSeries_alias' - The alias that identifies the time series.
--
-- 'assetId', 'describeTimeSeries_assetId' - The ID of the asset in which the asset property was created.
--
-- 'propertyId', 'describeTimeSeries_propertyId' - The ID of the asset property.
newDescribeTimeSeries ::
  DescribeTimeSeries
newDescribeTimeSeries =
  DescribeTimeSeries'
    { alias = Prelude.Nothing,
      assetId = Prelude.Nothing,
      propertyId = Prelude.Nothing
    }

-- | The alias that identifies the time series.
describeTimeSeries_alias :: Lens.Lens' DescribeTimeSeries (Prelude.Maybe Prelude.Text)
describeTimeSeries_alias = Lens.lens (\DescribeTimeSeries' {alias} -> alias) (\s@DescribeTimeSeries' {} a -> s {alias = a} :: DescribeTimeSeries)

-- | The ID of the asset in which the asset property was created.
describeTimeSeries_assetId :: Lens.Lens' DescribeTimeSeries (Prelude.Maybe Prelude.Text)
describeTimeSeries_assetId = Lens.lens (\DescribeTimeSeries' {assetId} -> assetId) (\s@DescribeTimeSeries' {} a -> s {assetId = a} :: DescribeTimeSeries)

-- | The ID of the asset property.
describeTimeSeries_propertyId :: Lens.Lens' DescribeTimeSeries (Prelude.Maybe Prelude.Text)
describeTimeSeries_propertyId = Lens.lens (\DescribeTimeSeries' {propertyId} -> propertyId) (\s@DescribeTimeSeries' {} a -> s {propertyId = a} :: DescribeTimeSeries)

instance Core.AWSRequest DescribeTimeSeries where
  type
    AWSResponse DescribeTimeSeries =
      DescribeTimeSeriesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTimeSeriesResponse'
            Prelude.<$> (x Data..?> "alias")
            Prelude.<*> (x Data..?> "assetId")
            Prelude.<*> (x Data..?> "dataTypeSpec")
            Prelude.<*> (x Data..?> "propertyId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "timeSeriesId")
            Prelude.<*> (x Data..:> "dataType")
            Prelude.<*> (x Data..:> "timeSeriesCreationDate")
            Prelude.<*> (x Data..:> "timeSeriesLastUpdateDate")
      )

instance Prelude.Hashable DescribeTimeSeries where
  hashWithSalt _salt DescribeTimeSeries' {..} =
    _salt `Prelude.hashWithSalt` alias
      `Prelude.hashWithSalt` assetId
      `Prelude.hashWithSalt` propertyId

instance Prelude.NFData DescribeTimeSeries where
  rnf DescribeTimeSeries' {..} =
    Prelude.rnf alias
      `Prelude.seq` Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf propertyId

instance Data.ToHeaders DescribeTimeSeries where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeTimeSeries where
  toPath = Prelude.const "/timeseries/describe/"

instance Data.ToQuery DescribeTimeSeries where
  toQuery DescribeTimeSeries' {..} =
    Prelude.mconcat
      [ "alias" Data.=: alias,
        "assetId" Data.=: assetId,
        "propertyId" Data.=: propertyId
      ]

-- | /See:/ 'newDescribeTimeSeriesResponse' smart constructor.
data DescribeTimeSeriesResponse = DescribeTimeSeriesResponse'
  { -- | The alias that identifies the time series.
    alias :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset in which the asset property was created.
    assetId :: Prelude.Maybe Prelude.Text,
    -- | The data type of the structure for this time series. This parameter is
    -- required for time series that have the @STRUCT@ data type.
    --
    -- The options for this parameter depend on the type of the composite model
    -- in which you created the asset property that is associated with your
    -- time series. Use @AWS\/ALARM_STATE@ for alarm state in alarm composite
    -- models.
    dataTypeSpec :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset property.
    propertyId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the time series.
    timeSeriesId :: Prelude.Text,
    -- | The data type of the time series.
    --
    -- If you specify @STRUCT@, you must also specify @dataTypeSpec@ to
    -- identify the type of the structure for this time series.
    dataType :: PropertyDataType,
    -- | The date that the time series was created, in Unix epoch time.
    timeSeriesCreationDate :: Data.POSIX,
    -- | The date that the time series was last updated, in Unix epoch time.
    timeSeriesLastUpdateDate :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTimeSeriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alias', 'describeTimeSeriesResponse_alias' - The alias that identifies the time series.
--
-- 'assetId', 'describeTimeSeriesResponse_assetId' - The ID of the asset in which the asset property was created.
--
-- 'dataTypeSpec', 'describeTimeSeriesResponse_dataTypeSpec' - The data type of the structure for this time series. This parameter is
-- required for time series that have the @STRUCT@ data type.
--
-- The options for this parameter depend on the type of the composite model
-- in which you created the asset property that is associated with your
-- time series. Use @AWS\/ALARM_STATE@ for alarm state in alarm composite
-- models.
--
-- 'propertyId', 'describeTimeSeriesResponse_propertyId' - The ID of the asset property.
--
-- 'httpStatus', 'describeTimeSeriesResponse_httpStatus' - The response's http status code.
--
-- 'timeSeriesId', 'describeTimeSeriesResponse_timeSeriesId' - The ID of the time series.
--
-- 'dataType', 'describeTimeSeriesResponse_dataType' - The data type of the time series.
--
-- If you specify @STRUCT@, you must also specify @dataTypeSpec@ to
-- identify the type of the structure for this time series.
--
-- 'timeSeriesCreationDate', 'describeTimeSeriesResponse_timeSeriesCreationDate' - The date that the time series was created, in Unix epoch time.
--
-- 'timeSeriesLastUpdateDate', 'describeTimeSeriesResponse_timeSeriesLastUpdateDate' - The date that the time series was last updated, in Unix epoch time.
newDescribeTimeSeriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'timeSeriesId'
  Prelude.Text ->
  -- | 'dataType'
  PropertyDataType ->
  -- | 'timeSeriesCreationDate'
  Prelude.UTCTime ->
  -- | 'timeSeriesLastUpdateDate'
  Prelude.UTCTime ->
  DescribeTimeSeriesResponse
newDescribeTimeSeriesResponse
  pHttpStatus_
  pTimeSeriesId_
  pDataType_
  pTimeSeriesCreationDate_
  pTimeSeriesLastUpdateDate_ =
    DescribeTimeSeriesResponse'
      { alias =
          Prelude.Nothing,
        assetId = Prelude.Nothing,
        dataTypeSpec = Prelude.Nothing,
        propertyId = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        timeSeriesId = pTimeSeriesId_,
        dataType = pDataType_,
        timeSeriesCreationDate =
          Data._Time Lens.# pTimeSeriesCreationDate_,
        timeSeriesLastUpdateDate =
          Data._Time Lens.# pTimeSeriesLastUpdateDate_
      }

-- | The alias that identifies the time series.
describeTimeSeriesResponse_alias :: Lens.Lens' DescribeTimeSeriesResponse (Prelude.Maybe Prelude.Text)
describeTimeSeriesResponse_alias = Lens.lens (\DescribeTimeSeriesResponse' {alias} -> alias) (\s@DescribeTimeSeriesResponse' {} a -> s {alias = a} :: DescribeTimeSeriesResponse)

-- | The ID of the asset in which the asset property was created.
describeTimeSeriesResponse_assetId :: Lens.Lens' DescribeTimeSeriesResponse (Prelude.Maybe Prelude.Text)
describeTimeSeriesResponse_assetId = Lens.lens (\DescribeTimeSeriesResponse' {assetId} -> assetId) (\s@DescribeTimeSeriesResponse' {} a -> s {assetId = a} :: DescribeTimeSeriesResponse)

-- | The data type of the structure for this time series. This parameter is
-- required for time series that have the @STRUCT@ data type.
--
-- The options for this parameter depend on the type of the composite model
-- in which you created the asset property that is associated with your
-- time series. Use @AWS\/ALARM_STATE@ for alarm state in alarm composite
-- models.
describeTimeSeriesResponse_dataTypeSpec :: Lens.Lens' DescribeTimeSeriesResponse (Prelude.Maybe Prelude.Text)
describeTimeSeriesResponse_dataTypeSpec = Lens.lens (\DescribeTimeSeriesResponse' {dataTypeSpec} -> dataTypeSpec) (\s@DescribeTimeSeriesResponse' {} a -> s {dataTypeSpec = a} :: DescribeTimeSeriesResponse)

-- | The ID of the asset property.
describeTimeSeriesResponse_propertyId :: Lens.Lens' DescribeTimeSeriesResponse (Prelude.Maybe Prelude.Text)
describeTimeSeriesResponse_propertyId = Lens.lens (\DescribeTimeSeriesResponse' {propertyId} -> propertyId) (\s@DescribeTimeSeriesResponse' {} a -> s {propertyId = a} :: DescribeTimeSeriesResponse)

-- | The response's http status code.
describeTimeSeriesResponse_httpStatus :: Lens.Lens' DescribeTimeSeriesResponse Prelude.Int
describeTimeSeriesResponse_httpStatus = Lens.lens (\DescribeTimeSeriesResponse' {httpStatus} -> httpStatus) (\s@DescribeTimeSeriesResponse' {} a -> s {httpStatus = a} :: DescribeTimeSeriesResponse)

-- | The ID of the time series.
describeTimeSeriesResponse_timeSeriesId :: Lens.Lens' DescribeTimeSeriesResponse Prelude.Text
describeTimeSeriesResponse_timeSeriesId = Lens.lens (\DescribeTimeSeriesResponse' {timeSeriesId} -> timeSeriesId) (\s@DescribeTimeSeriesResponse' {} a -> s {timeSeriesId = a} :: DescribeTimeSeriesResponse)

-- | The data type of the time series.
--
-- If you specify @STRUCT@, you must also specify @dataTypeSpec@ to
-- identify the type of the structure for this time series.
describeTimeSeriesResponse_dataType :: Lens.Lens' DescribeTimeSeriesResponse PropertyDataType
describeTimeSeriesResponse_dataType = Lens.lens (\DescribeTimeSeriesResponse' {dataType} -> dataType) (\s@DescribeTimeSeriesResponse' {} a -> s {dataType = a} :: DescribeTimeSeriesResponse)

-- | The date that the time series was created, in Unix epoch time.
describeTimeSeriesResponse_timeSeriesCreationDate :: Lens.Lens' DescribeTimeSeriesResponse Prelude.UTCTime
describeTimeSeriesResponse_timeSeriesCreationDate = Lens.lens (\DescribeTimeSeriesResponse' {timeSeriesCreationDate} -> timeSeriesCreationDate) (\s@DescribeTimeSeriesResponse' {} a -> s {timeSeriesCreationDate = a} :: DescribeTimeSeriesResponse) Prelude.. Data._Time

-- | The date that the time series was last updated, in Unix epoch time.
describeTimeSeriesResponse_timeSeriesLastUpdateDate :: Lens.Lens' DescribeTimeSeriesResponse Prelude.UTCTime
describeTimeSeriesResponse_timeSeriesLastUpdateDate = Lens.lens (\DescribeTimeSeriesResponse' {timeSeriesLastUpdateDate} -> timeSeriesLastUpdateDate) (\s@DescribeTimeSeriesResponse' {} a -> s {timeSeriesLastUpdateDate = a} :: DescribeTimeSeriesResponse) Prelude.. Data._Time

instance Prelude.NFData DescribeTimeSeriesResponse where
  rnf DescribeTimeSeriesResponse' {..} =
    Prelude.rnf alias
      `Prelude.seq` Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf dataTypeSpec
      `Prelude.seq` Prelude.rnf propertyId
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf timeSeriesId
      `Prelude.seq` Prelude.rnf dataType
      `Prelude.seq` Prelude.rnf timeSeriesCreationDate
      `Prelude.seq` Prelude.rnf timeSeriesLastUpdateDate
