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
-- Module      : Network.AWS.KinesisVideo.UpdateDataRetention
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.KinesisVideo.UpdateDataRetention
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

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateDataRetention' smart constructor.
data UpdateDataRetention = UpdateDataRetention'
  { -- | The Amazon Resource Name (ARN) of the stream whose retention period you
    -- want to change.
    streamARN :: Core.Maybe Core.Text,
    -- | The name of the stream whose retention period you want to change.
    streamName :: Core.Maybe Core.Text,
    -- | The version of the stream whose retention period you want to change. To
    -- get the version, call either the @DescribeStream@ or the @ListStreams@
    -- API.
    currentVersion :: Core.Text,
    -- | Indicates whether you want to increase or decrease the retention period.
    operation :: UpdateDataRetentionOperation,
    -- | The retention period, in hours. The value you specify replaces the
    -- current value. The maximum value for this parameter is 87600 (ten
    -- years).
    dataRetentionChangeInHours :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'operation'
  UpdateDataRetentionOperation ->
  -- | 'dataRetentionChangeInHours'
  Core.Natural ->
  UpdateDataRetention
newUpdateDataRetention
  pCurrentVersion_
  pOperation_
  pDataRetentionChangeInHours_ =
    UpdateDataRetention'
      { streamARN = Core.Nothing,
        streamName = Core.Nothing,
        currentVersion = pCurrentVersion_,
        operation = pOperation_,
        dataRetentionChangeInHours =
          pDataRetentionChangeInHours_
      }

-- | The Amazon Resource Name (ARN) of the stream whose retention period you
-- want to change.
updateDataRetention_streamARN :: Lens.Lens' UpdateDataRetention (Core.Maybe Core.Text)
updateDataRetention_streamARN = Lens.lens (\UpdateDataRetention' {streamARN} -> streamARN) (\s@UpdateDataRetention' {} a -> s {streamARN = a} :: UpdateDataRetention)

-- | The name of the stream whose retention period you want to change.
updateDataRetention_streamName :: Lens.Lens' UpdateDataRetention (Core.Maybe Core.Text)
updateDataRetention_streamName = Lens.lens (\UpdateDataRetention' {streamName} -> streamName) (\s@UpdateDataRetention' {} a -> s {streamName = a} :: UpdateDataRetention)

-- | The version of the stream whose retention period you want to change. To
-- get the version, call either the @DescribeStream@ or the @ListStreams@
-- API.
updateDataRetention_currentVersion :: Lens.Lens' UpdateDataRetention Core.Text
updateDataRetention_currentVersion = Lens.lens (\UpdateDataRetention' {currentVersion} -> currentVersion) (\s@UpdateDataRetention' {} a -> s {currentVersion = a} :: UpdateDataRetention)

-- | Indicates whether you want to increase or decrease the retention period.
updateDataRetention_operation :: Lens.Lens' UpdateDataRetention UpdateDataRetentionOperation
updateDataRetention_operation = Lens.lens (\UpdateDataRetention' {operation} -> operation) (\s@UpdateDataRetention' {} a -> s {operation = a} :: UpdateDataRetention)

-- | The retention period, in hours. The value you specify replaces the
-- current value. The maximum value for this parameter is 87600 (ten
-- years).
updateDataRetention_dataRetentionChangeInHours :: Lens.Lens' UpdateDataRetention Core.Natural
updateDataRetention_dataRetentionChangeInHours = Lens.lens (\UpdateDataRetention' {dataRetentionChangeInHours} -> dataRetentionChangeInHours) (\s@UpdateDataRetention' {} a -> s {dataRetentionChangeInHours = a} :: UpdateDataRetention)

instance Core.AWSRequest UpdateDataRetention where
  type
    AWSResponse UpdateDataRetention =
      UpdateDataRetentionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDataRetentionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateDataRetention

instance Core.NFData UpdateDataRetention

instance Core.ToHeaders UpdateDataRetention where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON UpdateDataRetention where
  toJSON UpdateDataRetention' {..} =
    Core.object
      ( Core.catMaybes
          [ ("StreamARN" Core..=) Core.<$> streamARN,
            ("StreamName" Core..=) Core.<$> streamName,
            Core.Just ("CurrentVersion" Core..= currentVersion),
            Core.Just ("Operation" Core..= operation),
            Core.Just
              ( "DataRetentionChangeInHours"
                  Core..= dataRetentionChangeInHours
              )
          ]
      )

instance Core.ToPath UpdateDataRetention where
  toPath = Core.const "/updateDataRetention"

instance Core.ToQuery UpdateDataRetention where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateDataRetentionResponse' smart constructor.
data UpdateDataRetentionResponse = UpdateDataRetentionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateDataRetentionResponse
newUpdateDataRetentionResponse pHttpStatus_ =
  UpdateDataRetentionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateDataRetentionResponse_httpStatus :: Lens.Lens' UpdateDataRetentionResponse Core.Int
updateDataRetentionResponse_httpStatus = Lens.lens (\UpdateDataRetentionResponse' {httpStatus} -> httpStatus) (\s@UpdateDataRetentionResponse' {} a -> s {httpStatus = a} :: UpdateDataRetentionResponse)

instance Core.NFData UpdateDataRetentionResponse
