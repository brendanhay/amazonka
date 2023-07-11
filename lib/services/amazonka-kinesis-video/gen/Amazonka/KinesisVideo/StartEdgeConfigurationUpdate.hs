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
-- Module      : Amazonka.KinesisVideo.StartEdgeConfigurationUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An asynchronous API that updates a stream’s existing edge configuration.
-- The Kinesis Video Stream will sync the stream’s edge configuration with
-- the Edge Agent IoT Greengrass component that runs on an IoT Hub Device,
-- setup at your premise. The time to sync can vary and depends on the
-- connectivity of the Hub Device. The @SyncStatus@ will be updated as the
-- edge configuration is acknowledged, and synced with the Edge Agent.
--
-- If this API is invoked for the first time, a new edge configuration will
-- be created for the stream, and the sync status will be set to @SYNCING@.
-- You will have to wait for the sync status to reach a terminal state such
-- as: @IN_SYNC@, or @SYNC_FAILED@, before using this API again. If you
-- invoke this API during the syncing process, a @ResourceInUseException@
-- will be thrown. The connectivity of the stream’s edge configuration and
-- the Edge Agent will be retried for 15 minutes. After 15 minutes, the
-- status will transition into the @SYNC_FAILED@ state.
module Amazonka.KinesisVideo.StartEdgeConfigurationUpdate
  ( -- * Creating a Request
    StartEdgeConfigurationUpdate (..),
    newStartEdgeConfigurationUpdate,

    -- * Request Lenses
    startEdgeConfigurationUpdate_streamARN,
    startEdgeConfigurationUpdate_streamName,
    startEdgeConfigurationUpdate_edgeConfig,

    -- * Destructuring the Response
    StartEdgeConfigurationUpdateResponse (..),
    newStartEdgeConfigurationUpdateResponse,

    -- * Response Lenses
    startEdgeConfigurationUpdateResponse_creationTime,
    startEdgeConfigurationUpdateResponse_edgeConfig,
    startEdgeConfigurationUpdateResponse_failedStatusDetails,
    startEdgeConfigurationUpdateResponse_lastUpdatedTime,
    startEdgeConfigurationUpdateResponse_streamARN,
    startEdgeConfigurationUpdateResponse_streamName,
    startEdgeConfigurationUpdateResponse_syncStatus,
    startEdgeConfigurationUpdateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartEdgeConfigurationUpdate' smart constructor.
data StartEdgeConfigurationUpdate = StartEdgeConfigurationUpdate'
  { -- | The Amazon Resource Name (ARN) of the stream. Specify either the
    -- @StreamName@ or the @StreamARN@.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream whose edge configuration you want to update.
    -- Specify either the @StreamName@ or the @StreamARN@.
    streamName :: Prelude.Maybe Prelude.Text,
    -- | The edge configuration details required to invoke the update process.
    edgeConfig :: EdgeConfig
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartEdgeConfigurationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamARN', 'startEdgeConfigurationUpdate_streamARN' - The Amazon Resource Name (ARN) of the stream. Specify either the
-- @StreamName@ or the @StreamARN@.
--
-- 'streamName', 'startEdgeConfigurationUpdate_streamName' - The name of the stream whose edge configuration you want to update.
-- Specify either the @StreamName@ or the @StreamARN@.
--
-- 'edgeConfig', 'startEdgeConfigurationUpdate_edgeConfig' - The edge configuration details required to invoke the update process.
newStartEdgeConfigurationUpdate ::
  -- | 'edgeConfig'
  EdgeConfig ->
  StartEdgeConfigurationUpdate
newStartEdgeConfigurationUpdate pEdgeConfig_ =
  StartEdgeConfigurationUpdate'
    { streamARN =
        Prelude.Nothing,
      streamName = Prelude.Nothing,
      edgeConfig = pEdgeConfig_
    }

-- | The Amazon Resource Name (ARN) of the stream. Specify either the
-- @StreamName@ or the @StreamARN@.
startEdgeConfigurationUpdate_streamARN :: Lens.Lens' StartEdgeConfigurationUpdate (Prelude.Maybe Prelude.Text)
startEdgeConfigurationUpdate_streamARN = Lens.lens (\StartEdgeConfigurationUpdate' {streamARN} -> streamARN) (\s@StartEdgeConfigurationUpdate' {} a -> s {streamARN = a} :: StartEdgeConfigurationUpdate)

-- | The name of the stream whose edge configuration you want to update.
-- Specify either the @StreamName@ or the @StreamARN@.
startEdgeConfigurationUpdate_streamName :: Lens.Lens' StartEdgeConfigurationUpdate (Prelude.Maybe Prelude.Text)
startEdgeConfigurationUpdate_streamName = Lens.lens (\StartEdgeConfigurationUpdate' {streamName} -> streamName) (\s@StartEdgeConfigurationUpdate' {} a -> s {streamName = a} :: StartEdgeConfigurationUpdate)

-- | The edge configuration details required to invoke the update process.
startEdgeConfigurationUpdate_edgeConfig :: Lens.Lens' StartEdgeConfigurationUpdate EdgeConfig
startEdgeConfigurationUpdate_edgeConfig = Lens.lens (\StartEdgeConfigurationUpdate' {edgeConfig} -> edgeConfig) (\s@StartEdgeConfigurationUpdate' {} a -> s {edgeConfig = a} :: StartEdgeConfigurationUpdate)

instance Core.AWSRequest StartEdgeConfigurationUpdate where
  type
    AWSResponse StartEdgeConfigurationUpdate =
      StartEdgeConfigurationUpdateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartEdgeConfigurationUpdateResponse'
            Prelude.<$> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "EdgeConfig")
            Prelude.<*> (x Data..?> "FailedStatusDetails")
            Prelude.<*> (x Data..?> "LastUpdatedTime")
            Prelude.<*> (x Data..?> "StreamARN")
            Prelude.<*> (x Data..?> "StreamName")
            Prelude.<*> (x Data..?> "SyncStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartEdgeConfigurationUpdate
  where
  hashWithSalt _salt StartEdgeConfigurationUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` streamARN
      `Prelude.hashWithSalt` streamName
      `Prelude.hashWithSalt` edgeConfig

instance Prelude.NFData StartEdgeConfigurationUpdate where
  rnf StartEdgeConfigurationUpdate' {..} =
    Prelude.rnf streamARN
      `Prelude.seq` Prelude.rnf streamName
      `Prelude.seq` Prelude.rnf edgeConfig

instance Data.ToHeaders StartEdgeConfigurationUpdate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON StartEdgeConfigurationUpdate where
  toJSON StartEdgeConfigurationUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StreamARN" Data..=) Prelude.<$> streamARN,
            ("StreamName" Data..=) Prelude.<$> streamName,
            Prelude.Just ("EdgeConfig" Data..= edgeConfig)
          ]
      )

instance Data.ToPath StartEdgeConfigurationUpdate where
  toPath =
    Prelude.const "/startEdgeConfigurationUpdate"

instance Data.ToQuery StartEdgeConfigurationUpdate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartEdgeConfigurationUpdateResponse' smart constructor.
data StartEdgeConfigurationUpdateResponse = StartEdgeConfigurationUpdateResponse'
  { -- | The timestamp at which a stream’s edge configuration was first created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | A description of the stream\'s edge configuration that will be used to
    -- sync with the Edge Agent IoT Greengrass component. The Edge Agent
    -- component will run on an IoT Hub Device setup at your premise.
    edgeConfig :: Prelude.Maybe EdgeConfig,
    -- | A description of the generated failure status.
    failedStatusDetails :: Prelude.Maybe Prelude.Text,
    -- | The timestamp at which a stream’s edge configuration was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the stream.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream from which the edge configuration was updated.
    streamName :: Prelude.Maybe Prelude.Text,
    -- | The current sync status of the stream\'s edge configuration. When you
    -- invoke this API, the sync status will be set to the @SYNCING@ state. Use
    -- the @DescribeEdgeConfiguration@ API to get the latest status of the edge
    -- configuration.
    syncStatus :: Prelude.Maybe SyncStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartEdgeConfigurationUpdateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'startEdgeConfigurationUpdateResponse_creationTime' - The timestamp at which a stream’s edge configuration was first created.
--
-- 'edgeConfig', 'startEdgeConfigurationUpdateResponse_edgeConfig' - A description of the stream\'s edge configuration that will be used to
-- sync with the Edge Agent IoT Greengrass component. The Edge Agent
-- component will run on an IoT Hub Device setup at your premise.
--
-- 'failedStatusDetails', 'startEdgeConfigurationUpdateResponse_failedStatusDetails' - A description of the generated failure status.
--
-- 'lastUpdatedTime', 'startEdgeConfigurationUpdateResponse_lastUpdatedTime' - The timestamp at which a stream’s edge configuration was last updated.
--
-- 'streamARN', 'startEdgeConfigurationUpdateResponse_streamARN' - The Amazon Resource Name (ARN) of the stream.
--
-- 'streamName', 'startEdgeConfigurationUpdateResponse_streamName' - The name of the stream from which the edge configuration was updated.
--
-- 'syncStatus', 'startEdgeConfigurationUpdateResponse_syncStatus' - The current sync status of the stream\'s edge configuration. When you
-- invoke this API, the sync status will be set to the @SYNCING@ state. Use
-- the @DescribeEdgeConfiguration@ API to get the latest status of the edge
-- configuration.
--
-- 'httpStatus', 'startEdgeConfigurationUpdateResponse_httpStatus' - The response's http status code.
newStartEdgeConfigurationUpdateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartEdgeConfigurationUpdateResponse
newStartEdgeConfigurationUpdateResponse pHttpStatus_ =
  StartEdgeConfigurationUpdateResponse'
    { creationTime =
        Prelude.Nothing,
      edgeConfig = Prelude.Nothing,
      failedStatusDetails = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      streamARN = Prelude.Nothing,
      streamName = Prelude.Nothing,
      syncStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The timestamp at which a stream’s edge configuration was first created.
startEdgeConfigurationUpdateResponse_creationTime :: Lens.Lens' StartEdgeConfigurationUpdateResponse (Prelude.Maybe Prelude.UTCTime)
startEdgeConfigurationUpdateResponse_creationTime = Lens.lens (\StartEdgeConfigurationUpdateResponse' {creationTime} -> creationTime) (\s@StartEdgeConfigurationUpdateResponse' {} a -> s {creationTime = a} :: StartEdgeConfigurationUpdateResponse) Prelude.. Lens.mapping Data._Time

-- | A description of the stream\'s edge configuration that will be used to
-- sync with the Edge Agent IoT Greengrass component. The Edge Agent
-- component will run on an IoT Hub Device setup at your premise.
startEdgeConfigurationUpdateResponse_edgeConfig :: Lens.Lens' StartEdgeConfigurationUpdateResponse (Prelude.Maybe EdgeConfig)
startEdgeConfigurationUpdateResponse_edgeConfig = Lens.lens (\StartEdgeConfigurationUpdateResponse' {edgeConfig} -> edgeConfig) (\s@StartEdgeConfigurationUpdateResponse' {} a -> s {edgeConfig = a} :: StartEdgeConfigurationUpdateResponse)

-- | A description of the generated failure status.
startEdgeConfigurationUpdateResponse_failedStatusDetails :: Lens.Lens' StartEdgeConfigurationUpdateResponse (Prelude.Maybe Prelude.Text)
startEdgeConfigurationUpdateResponse_failedStatusDetails = Lens.lens (\StartEdgeConfigurationUpdateResponse' {failedStatusDetails} -> failedStatusDetails) (\s@StartEdgeConfigurationUpdateResponse' {} a -> s {failedStatusDetails = a} :: StartEdgeConfigurationUpdateResponse)

-- | The timestamp at which a stream’s edge configuration was last updated.
startEdgeConfigurationUpdateResponse_lastUpdatedTime :: Lens.Lens' StartEdgeConfigurationUpdateResponse (Prelude.Maybe Prelude.UTCTime)
startEdgeConfigurationUpdateResponse_lastUpdatedTime = Lens.lens (\StartEdgeConfigurationUpdateResponse' {lastUpdatedTime} -> lastUpdatedTime) (\s@StartEdgeConfigurationUpdateResponse' {} a -> s {lastUpdatedTime = a} :: StartEdgeConfigurationUpdateResponse) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the stream.
startEdgeConfigurationUpdateResponse_streamARN :: Lens.Lens' StartEdgeConfigurationUpdateResponse (Prelude.Maybe Prelude.Text)
startEdgeConfigurationUpdateResponse_streamARN = Lens.lens (\StartEdgeConfigurationUpdateResponse' {streamARN} -> streamARN) (\s@StartEdgeConfigurationUpdateResponse' {} a -> s {streamARN = a} :: StartEdgeConfigurationUpdateResponse)

-- | The name of the stream from which the edge configuration was updated.
startEdgeConfigurationUpdateResponse_streamName :: Lens.Lens' StartEdgeConfigurationUpdateResponse (Prelude.Maybe Prelude.Text)
startEdgeConfigurationUpdateResponse_streamName = Lens.lens (\StartEdgeConfigurationUpdateResponse' {streamName} -> streamName) (\s@StartEdgeConfigurationUpdateResponse' {} a -> s {streamName = a} :: StartEdgeConfigurationUpdateResponse)

-- | The current sync status of the stream\'s edge configuration. When you
-- invoke this API, the sync status will be set to the @SYNCING@ state. Use
-- the @DescribeEdgeConfiguration@ API to get the latest status of the edge
-- configuration.
startEdgeConfigurationUpdateResponse_syncStatus :: Lens.Lens' StartEdgeConfigurationUpdateResponse (Prelude.Maybe SyncStatus)
startEdgeConfigurationUpdateResponse_syncStatus = Lens.lens (\StartEdgeConfigurationUpdateResponse' {syncStatus} -> syncStatus) (\s@StartEdgeConfigurationUpdateResponse' {} a -> s {syncStatus = a} :: StartEdgeConfigurationUpdateResponse)

-- | The response's http status code.
startEdgeConfigurationUpdateResponse_httpStatus :: Lens.Lens' StartEdgeConfigurationUpdateResponse Prelude.Int
startEdgeConfigurationUpdateResponse_httpStatus = Lens.lens (\StartEdgeConfigurationUpdateResponse' {httpStatus} -> httpStatus) (\s@StartEdgeConfigurationUpdateResponse' {} a -> s {httpStatus = a} :: StartEdgeConfigurationUpdateResponse)

instance
  Prelude.NFData
    StartEdgeConfigurationUpdateResponse
  where
  rnf StartEdgeConfigurationUpdateResponse' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf edgeConfig
      `Prelude.seq` Prelude.rnf failedStatusDetails
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf streamARN
      `Prelude.seq` Prelude.rnf streamName
      `Prelude.seq` Prelude.rnf syncStatus
      `Prelude.seq` Prelude.rnf httpStatus
