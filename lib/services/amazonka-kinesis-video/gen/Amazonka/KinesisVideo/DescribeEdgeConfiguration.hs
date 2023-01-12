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
-- Module      : Amazonka.KinesisVideo.DescribeEdgeConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a stream’s edge configuration that was set using the
-- @StartEdgeConfigurationUpdate@ API. Use this API to get the status of
-- the configuration if the configuration is in sync with the Edge Agent.
module Amazonka.KinesisVideo.DescribeEdgeConfiguration
  ( -- * Creating a Request
    DescribeEdgeConfiguration (..),
    newDescribeEdgeConfiguration,

    -- * Request Lenses
    describeEdgeConfiguration_streamARN,
    describeEdgeConfiguration_streamName,

    -- * Destructuring the Response
    DescribeEdgeConfigurationResponse (..),
    newDescribeEdgeConfigurationResponse,

    -- * Response Lenses
    describeEdgeConfigurationResponse_creationTime,
    describeEdgeConfigurationResponse_edgeConfig,
    describeEdgeConfigurationResponse_failedStatusDetails,
    describeEdgeConfigurationResponse_lastUpdatedTime,
    describeEdgeConfigurationResponse_streamARN,
    describeEdgeConfigurationResponse_streamName,
    describeEdgeConfigurationResponse_syncStatus,
    describeEdgeConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEdgeConfiguration' smart constructor.
data DescribeEdgeConfiguration = DescribeEdgeConfiguration'
  { -- | The Amazon Resource Name (ARN) of the stream. Specify either the
    -- @StreamName@or the @StreamARN@.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream whose edge configuration you want to update.
    -- Specify either the @StreamName@ or the @StreamARN@.
    streamName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEdgeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamARN', 'describeEdgeConfiguration_streamARN' - The Amazon Resource Name (ARN) of the stream. Specify either the
-- @StreamName@or the @StreamARN@.
--
-- 'streamName', 'describeEdgeConfiguration_streamName' - The name of the stream whose edge configuration you want to update.
-- Specify either the @StreamName@ or the @StreamARN@.
newDescribeEdgeConfiguration ::
  DescribeEdgeConfiguration
newDescribeEdgeConfiguration =
  DescribeEdgeConfiguration'
    { streamARN =
        Prelude.Nothing,
      streamName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the stream. Specify either the
-- @StreamName@or the @StreamARN@.
describeEdgeConfiguration_streamARN :: Lens.Lens' DescribeEdgeConfiguration (Prelude.Maybe Prelude.Text)
describeEdgeConfiguration_streamARN = Lens.lens (\DescribeEdgeConfiguration' {streamARN} -> streamARN) (\s@DescribeEdgeConfiguration' {} a -> s {streamARN = a} :: DescribeEdgeConfiguration)

-- | The name of the stream whose edge configuration you want to update.
-- Specify either the @StreamName@ or the @StreamARN@.
describeEdgeConfiguration_streamName :: Lens.Lens' DescribeEdgeConfiguration (Prelude.Maybe Prelude.Text)
describeEdgeConfiguration_streamName = Lens.lens (\DescribeEdgeConfiguration' {streamName} -> streamName) (\s@DescribeEdgeConfiguration' {} a -> s {streamName = a} :: DescribeEdgeConfiguration)

instance Core.AWSRequest DescribeEdgeConfiguration where
  type
    AWSResponse DescribeEdgeConfiguration =
      DescribeEdgeConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEdgeConfigurationResponse'
            Prelude.<$> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "EdgeConfig")
            Prelude.<*> (x Data..?> "FailedStatusDetails")
            Prelude.<*> (x Data..?> "LastUpdatedTime")
            Prelude.<*> (x Data..?> "StreamARN")
            Prelude.<*> (x Data..?> "StreamName")
            Prelude.<*> (x Data..?> "SyncStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEdgeConfiguration where
  hashWithSalt _salt DescribeEdgeConfiguration' {..} =
    _salt `Prelude.hashWithSalt` streamARN
      `Prelude.hashWithSalt` streamName

instance Prelude.NFData DescribeEdgeConfiguration where
  rnf DescribeEdgeConfiguration' {..} =
    Prelude.rnf streamARN
      `Prelude.seq` Prelude.rnf streamName

instance Data.ToHeaders DescribeEdgeConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON DescribeEdgeConfiguration where
  toJSON DescribeEdgeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StreamARN" Data..=) Prelude.<$> streamARN,
            ("StreamName" Data..=) Prelude.<$> streamName
          ]
      )

instance Data.ToPath DescribeEdgeConfiguration where
  toPath = Prelude.const "/describeEdgeConfiguration"

instance Data.ToQuery DescribeEdgeConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEdgeConfigurationResponse' smart constructor.
data DescribeEdgeConfigurationResponse = DescribeEdgeConfigurationResponse'
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
    -- | The latest status of the edge configuration update.
    syncStatus :: Prelude.Maybe SyncStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEdgeConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeEdgeConfigurationResponse_creationTime' - The timestamp at which a stream’s edge configuration was first created.
--
-- 'edgeConfig', 'describeEdgeConfigurationResponse_edgeConfig' - A description of the stream\'s edge configuration that will be used to
-- sync with the Edge Agent IoT Greengrass component. The Edge Agent
-- component will run on an IoT Hub Device setup at your premise.
--
-- 'failedStatusDetails', 'describeEdgeConfigurationResponse_failedStatusDetails' - A description of the generated failure status.
--
-- 'lastUpdatedTime', 'describeEdgeConfigurationResponse_lastUpdatedTime' - The timestamp at which a stream’s edge configuration was last updated.
--
-- 'streamARN', 'describeEdgeConfigurationResponse_streamARN' - The Amazon Resource Name (ARN) of the stream.
--
-- 'streamName', 'describeEdgeConfigurationResponse_streamName' - The name of the stream from which the edge configuration was updated.
--
-- 'syncStatus', 'describeEdgeConfigurationResponse_syncStatus' - The latest status of the edge configuration update.
--
-- 'httpStatus', 'describeEdgeConfigurationResponse_httpStatus' - The response's http status code.
newDescribeEdgeConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEdgeConfigurationResponse
newDescribeEdgeConfigurationResponse pHttpStatus_ =
  DescribeEdgeConfigurationResponse'
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
describeEdgeConfigurationResponse_creationTime :: Lens.Lens' DescribeEdgeConfigurationResponse (Prelude.Maybe Prelude.UTCTime)
describeEdgeConfigurationResponse_creationTime = Lens.lens (\DescribeEdgeConfigurationResponse' {creationTime} -> creationTime) (\s@DescribeEdgeConfigurationResponse' {} a -> s {creationTime = a} :: DescribeEdgeConfigurationResponse) Prelude.. Lens.mapping Data._Time

-- | A description of the stream\'s edge configuration that will be used to
-- sync with the Edge Agent IoT Greengrass component. The Edge Agent
-- component will run on an IoT Hub Device setup at your premise.
describeEdgeConfigurationResponse_edgeConfig :: Lens.Lens' DescribeEdgeConfigurationResponse (Prelude.Maybe EdgeConfig)
describeEdgeConfigurationResponse_edgeConfig = Lens.lens (\DescribeEdgeConfigurationResponse' {edgeConfig} -> edgeConfig) (\s@DescribeEdgeConfigurationResponse' {} a -> s {edgeConfig = a} :: DescribeEdgeConfigurationResponse)

-- | A description of the generated failure status.
describeEdgeConfigurationResponse_failedStatusDetails :: Lens.Lens' DescribeEdgeConfigurationResponse (Prelude.Maybe Prelude.Text)
describeEdgeConfigurationResponse_failedStatusDetails = Lens.lens (\DescribeEdgeConfigurationResponse' {failedStatusDetails} -> failedStatusDetails) (\s@DescribeEdgeConfigurationResponse' {} a -> s {failedStatusDetails = a} :: DescribeEdgeConfigurationResponse)

-- | The timestamp at which a stream’s edge configuration was last updated.
describeEdgeConfigurationResponse_lastUpdatedTime :: Lens.Lens' DescribeEdgeConfigurationResponse (Prelude.Maybe Prelude.UTCTime)
describeEdgeConfigurationResponse_lastUpdatedTime = Lens.lens (\DescribeEdgeConfigurationResponse' {lastUpdatedTime} -> lastUpdatedTime) (\s@DescribeEdgeConfigurationResponse' {} a -> s {lastUpdatedTime = a} :: DescribeEdgeConfigurationResponse) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the stream.
describeEdgeConfigurationResponse_streamARN :: Lens.Lens' DescribeEdgeConfigurationResponse (Prelude.Maybe Prelude.Text)
describeEdgeConfigurationResponse_streamARN = Lens.lens (\DescribeEdgeConfigurationResponse' {streamARN} -> streamARN) (\s@DescribeEdgeConfigurationResponse' {} a -> s {streamARN = a} :: DescribeEdgeConfigurationResponse)

-- | The name of the stream from which the edge configuration was updated.
describeEdgeConfigurationResponse_streamName :: Lens.Lens' DescribeEdgeConfigurationResponse (Prelude.Maybe Prelude.Text)
describeEdgeConfigurationResponse_streamName = Lens.lens (\DescribeEdgeConfigurationResponse' {streamName} -> streamName) (\s@DescribeEdgeConfigurationResponse' {} a -> s {streamName = a} :: DescribeEdgeConfigurationResponse)

-- | The latest status of the edge configuration update.
describeEdgeConfigurationResponse_syncStatus :: Lens.Lens' DescribeEdgeConfigurationResponse (Prelude.Maybe SyncStatus)
describeEdgeConfigurationResponse_syncStatus = Lens.lens (\DescribeEdgeConfigurationResponse' {syncStatus} -> syncStatus) (\s@DescribeEdgeConfigurationResponse' {} a -> s {syncStatus = a} :: DescribeEdgeConfigurationResponse)

-- | The response's http status code.
describeEdgeConfigurationResponse_httpStatus :: Lens.Lens' DescribeEdgeConfigurationResponse Prelude.Int
describeEdgeConfigurationResponse_httpStatus = Lens.lens (\DescribeEdgeConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeEdgeConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeEdgeConfigurationResponse)

instance
  Prelude.NFData
    DescribeEdgeConfigurationResponse
  where
  rnf DescribeEdgeConfigurationResponse' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf edgeConfig
      `Prelude.seq` Prelude.rnf failedStatusDetails
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf streamARN
      `Prelude.seq` Prelude.rnf streamName
      `Prelude.seq` Prelude.rnf syncStatus
      `Prelude.seq` Prelude.rnf httpStatus
