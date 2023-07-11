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
-- Module      : Amazonka.KinesisVideo.UpdateDataRetention
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Increases or decreases the stream\'s data retention period by the value
-- that you specify. To indicate whether you want to increase or decrease
-- the data retention period, specify the @Operation@ parameter in the
-- request body. In the request, you must specify either the @StreamName@
-- or the @StreamARN@.
--
-- The retention period that you specify replaces the current value.
--
-- This operation requires permission for the
-- @KinesisVideo:UpdateDataRetention@ action.
--
-- Changing the data retention period affects the data in the stream as
-- follows:
--
-- -   If the data retention period is increased, existing data is retained
--     for the new retention period. For example, if the data retention
--     period is increased from one hour to seven hours, all existing data
--     is retained for seven hours.
--
-- -   If the data retention period is decreased, existing data is retained
--     for the new retention period. For example, if the data retention
--     period is decreased from seven hours to one hour, all existing data
--     is retained for one hour, and any data older than one hour is
--     deleted immediately.
module Amazonka.KinesisVideo.UpdateDataRetention
  ( -- * Creating a Request
    UpdateDataRetention (..),
    newUpdateDataRetention,

    -- * Request Lenses
    updateDataRetention_streamARN,
    updateDataRetention_streamName,
    updateDataRetention_currentVersion,
    updateDataRetention_operation,
    updateDataRetention_dataRetentionChangeInHours,

    -- * Destructuring the Response
    UpdateDataRetentionResponse (..),
    newUpdateDataRetentionResponse,

    -- * Response Lenses
    updateDataRetentionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDataRetention' smart constructor.
data UpdateDataRetention = UpdateDataRetention'
  { -- | The Amazon Resource Name (ARN) of the stream whose retention period you
    -- want to change.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream whose retention period you want to change.
    streamName :: Prelude.Maybe Prelude.Text,
    -- | The version of the stream whose retention period you want to change. To
    -- get the version, call either the @DescribeStream@ or the @ListStreams@
    -- API.
    currentVersion :: Prelude.Text,
    -- | Indicates whether you want to increase or decrease the retention period.
    operation :: UpdateDataRetentionOperation,
    -- | The retention period, in hours. The value you specify replaces the
    -- current value. The maximum value for this parameter is 87600 (ten
    -- years).
    dataRetentionChangeInHours :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataRetention' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamARN', 'updateDataRetention_streamARN' - The Amazon Resource Name (ARN) of the stream whose retention period you
-- want to change.
--
-- 'streamName', 'updateDataRetention_streamName' - The name of the stream whose retention period you want to change.
--
-- 'currentVersion', 'updateDataRetention_currentVersion' - The version of the stream whose retention period you want to change. To
-- get the version, call either the @DescribeStream@ or the @ListStreams@
-- API.
--
-- 'operation', 'updateDataRetention_operation' - Indicates whether you want to increase or decrease the retention period.
--
-- 'dataRetentionChangeInHours', 'updateDataRetention_dataRetentionChangeInHours' - The retention period, in hours. The value you specify replaces the
-- current value. The maximum value for this parameter is 87600 (ten
-- years).
newUpdateDataRetention ::
  -- | 'currentVersion'
  Prelude.Text ->
  -- | 'operation'
  UpdateDataRetentionOperation ->
  -- | 'dataRetentionChangeInHours'
  Prelude.Natural ->
  UpdateDataRetention
newUpdateDataRetention
  pCurrentVersion_
  pOperation_
  pDataRetentionChangeInHours_ =
    UpdateDataRetention'
      { streamARN = Prelude.Nothing,
        streamName = Prelude.Nothing,
        currentVersion = pCurrentVersion_,
        operation = pOperation_,
        dataRetentionChangeInHours =
          pDataRetentionChangeInHours_
      }

-- | The Amazon Resource Name (ARN) of the stream whose retention period you
-- want to change.
updateDataRetention_streamARN :: Lens.Lens' UpdateDataRetention (Prelude.Maybe Prelude.Text)
updateDataRetention_streamARN = Lens.lens (\UpdateDataRetention' {streamARN} -> streamARN) (\s@UpdateDataRetention' {} a -> s {streamARN = a} :: UpdateDataRetention)

-- | The name of the stream whose retention period you want to change.
updateDataRetention_streamName :: Lens.Lens' UpdateDataRetention (Prelude.Maybe Prelude.Text)
updateDataRetention_streamName = Lens.lens (\UpdateDataRetention' {streamName} -> streamName) (\s@UpdateDataRetention' {} a -> s {streamName = a} :: UpdateDataRetention)

-- | The version of the stream whose retention period you want to change. To
-- get the version, call either the @DescribeStream@ or the @ListStreams@
-- API.
updateDataRetention_currentVersion :: Lens.Lens' UpdateDataRetention Prelude.Text
updateDataRetention_currentVersion = Lens.lens (\UpdateDataRetention' {currentVersion} -> currentVersion) (\s@UpdateDataRetention' {} a -> s {currentVersion = a} :: UpdateDataRetention)

-- | Indicates whether you want to increase or decrease the retention period.
updateDataRetention_operation :: Lens.Lens' UpdateDataRetention UpdateDataRetentionOperation
updateDataRetention_operation = Lens.lens (\UpdateDataRetention' {operation} -> operation) (\s@UpdateDataRetention' {} a -> s {operation = a} :: UpdateDataRetention)

-- | The retention period, in hours. The value you specify replaces the
-- current value. The maximum value for this parameter is 87600 (ten
-- years).
updateDataRetention_dataRetentionChangeInHours :: Lens.Lens' UpdateDataRetention Prelude.Natural
updateDataRetention_dataRetentionChangeInHours = Lens.lens (\UpdateDataRetention' {dataRetentionChangeInHours} -> dataRetentionChangeInHours) (\s@UpdateDataRetention' {} a -> s {dataRetentionChangeInHours = a} :: UpdateDataRetention)

instance Core.AWSRequest UpdateDataRetention where
  type
    AWSResponse UpdateDataRetention =
      UpdateDataRetentionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDataRetentionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDataRetention where
  hashWithSalt _salt UpdateDataRetention' {..} =
    _salt
      `Prelude.hashWithSalt` streamARN
      `Prelude.hashWithSalt` streamName
      `Prelude.hashWithSalt` currentVersion
      `Prelude.hashWithSalt` operation
      `Prelude.hashWithSalt` dataRetentionChangeInHours

instance Prelude.NFData UpdateDataRetention where
  rnf UpdateDataRetention' {..} =
    Prelude.rnf streamARN
      `Prelude.seq` Prelude.rnf streamName
      `Prelude.seq` Prelude.rnf currentVersion
      `Prelude.seq` Prelude.rnf operation
      `Prelude.seq` Prelude.rnf dataRetentionChangeInHours

instance Data.ToHeaders UpdateDataRetention where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateDataRetention where
  toJSON UpdateDataRetention' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StreamARN" Data..=) Prelude.<$> streamARN,
            ("StreamName" Data..=) Prelude.<$> streamName,
            Prelude.Just
              ("CurrentVersion" Data..= currentVersion),
            Prelude.Just ("Operation" Data..= operation),
            Prelude.Just
              ( "DataRetentionChangeInHours"
                  Data..= dataRetentionChangeInHours
              )
          ]
      )

instance Data.ToPath UpdateDataRetention where
  toPath = Prelude.const "/updateDataRetention"

instance Data.ToQuery UpdateDataRetention where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDataRetentionResponse' smart constructor.
data UpdateDataRetentionResponse = UpdateDataRetentionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataRetentionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDataRetentionResponse_httpStatus' - The response's http status code.
newUpdateDataRetentionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDataRetentionResponse
newUpdateDataRetentionResponse pHttpStatus_ =
  UpdateDataRetentionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateDataRetentionResponse_httpStatus :: Lens.Lens' UpdateDataRetentionResponse Prelude.Int
updateDataRetentionResponse_httpStatus = Lens.lens (\UpdateDataRetentionResponse' {httpStatus} -> httpStatus) (\s@UpdateDataRetentionResponse' {} a -> s {httpStatus = a} :: UpdateDataRetentionResponse)

instance Prelude.NFData UpdateDataRetentionResponse where
  rnf UpdateDataRetentionResponse' {..} =
    Prelude.rnf httpStatus
