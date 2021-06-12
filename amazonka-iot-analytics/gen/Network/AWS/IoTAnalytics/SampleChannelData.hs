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
-- Module      : Network.AWS.IoTAnalytics.SampleChannelData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a sample of messages from the specified channel ingested
-- during the specified timeframe. Up to 10 messages can be retrieved.
module Network.AWS.IoTAnalytics.SampleChannelData
  ( -- * Creating a Request
    SampleChannelData (..),
    newSampleChannelData,

    -- * Request Lenses
    sampleChannelData_maxMessages,
    sampleChannelData_startTime,
    sampleChannelData_endTime,
    sampleChannelData_channelName,

    -- * Destructuring the Response
    SampleChannelDataResponse (..),
    newSampleChannelDataResponse,

    -- * Response Lenses
    sampleChannelDataResponse_payloads,
    sampleChannelDataResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSampleChannelData' smart constructor.
data SampleChannelData = SampleChannelData'
  { -- | The number of sample messages to be retrieved. The limit is 10. The
    -- default is also 10.
    maxMessages :: Core.Maybe Core.Natural,
    -- | The start of the time window from which sample messages are retrieved.
    startTime :: Core.Maybe Core.POSIX,
    -- | The end of the time window from which sample messages are retrieved.
    endTime :: Core.Maybe Core.POSIX,
    -- | The name of the channel whose message samples are retrieved.
    channelName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SampleChannelData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxMessages', 'sampleChannelData_maxMessages' - The number of sample messages to be retrieved. The limit is 10. The
-- default is also 10.
--
-- 'startTime', 'sampleChannelData_startTime' - The start of the time window from which sample messages are retrieved.
--
-- 'endTime', 'sampleChannelData_endTime' - The end of the time window from which sample messages are retrieved.
--
-- 'channelName', 'sampleChannelData_channelName' - The name of the channel whose message samples are retrieved.
newSampleChannelData ::
  -- | 'channelName'
  Core.Text ->
  SampleChannelData
newSampleChannelData pChannelName_ =
  SampleChannelData'
    { maxMessages = Core.Nothing,
      startTime = Core.Nothing,
      endTime = Core.Nothing,
      channelName = pChannelName_
    }

-- | The number of sample messages to be retrieved. The limit is 10. The
-- default is also 10.
sampleChannelData_maxMessages :: Lens.Lens' SampleChannelData (Core.Maybe Core.Natural)
sampleChannelData_maxMessages = Lens.lens (\SampleChannelData' {maxMessages} -> maxMessages) (\s@SampleChannelData' {} a -> s {maxMessages = a} :: SampleChannelData)

-- | The start of the time window from which sample messages are retrieved.
sampleChannelData_startTime :: Lens.Lens' SampleChannelData (Core.Maybe Core.UTCTime)
sampleChannelData_startTime = Lens.lens (\SampleChannelData' {startTime} -> startTime) (\s@SampleChannelData' {} a -> s {startTime = a} :: SampleChannelData) Core.. Lens.mapping Core._Time

-- | The end of the time window from which sample messages are retrieved.
sampleChannelData_endTime :: Lens.Lens' SampleChannelData (Core.Maybe Core.UTCTime)
sampleChannelData_endTime = Lens.lens (\SampleChannelData' {endTime} -> endTime) (\s@SampleChannelData' {} a -> s {endTime = a} :: SampleChannelData) Core.. Lens.mapping Core._Time

-- | The name of the channel whose message samples are retrieved.
sampleChannelData_channelName :: Lens.Lens' SampleChannelData Core.Text
sampleChannelData_channelName = Lens.lens (\SampleChannelData' {channelName} -> channelName) (\s@SampleChannelData' {} a -> s {channelName = a} :: SampleChannelData)

instance Core.AWSRequest SampleChannelData where
  type
    AWSResponse SampleChannelData =
      SampleChannelDataResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SampleChannelDataResponse'
            Core.<$> (x Core..?> "payloads")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SampleChannelData

instance Core.NFData SampleChannelData

instance Core.ToHeaders SampleChannelData where
  toHeaders = Core.const Core.mempty

instance Core.ToPath SampleChannelData where
  toPath SampleChannelData' {..} =
    Core.mconcat
      ["/channels/", Core.toBS channelName, "/sample"]

instance Core.ToQuery SampleChannelData where
  toQuery SampleChannelData' {..} =
    Core.mconcat
      [ "maxMessages" Core.=: maxMessages,
        "startTime" Core.=: startTime,
        "endTime" Core.=: endTime
      ]

-- | /See:/ 'newSampleChannelDataResponse' smart constructor.
data SampleChannelDataResponse = SampleChannelDataResponse'
  { -- | The list of message samples. Each sample message is returned as a
    -- base64-encoded string.
    payloads :: Core.Maybe (Core.NonEmpty Core.Base64),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SampleChannelDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'payloads', 'sampleChannelDataResponse_payloads' - The list of message samples. Each sample message is returned as a
-- base64-encoded string.
--
-- 'httpStatus', 'sampleChannelDataResponse_httpStatus' - The response's http status code.
newSampleChannelDataResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SampleChannelDataResponse
newSampleChannelDataResponse pHttpStatus_ =
  SampleChannelDataResponse'
    { payloads = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of message samples. Each sample message is returned as a
-- base64-encoded string.
sampleChannelDataResponse_payloads :: Lens.Lens' SampleChannelDataResponse (Core.Maybe (Core.NonEmpty Core.ByteString))
sampleChannelDataResponse_payloads = Lens.lens (\SampleChannelDataResponse' {payloads} -> payloads) (\s@SampleChannelDataResponse' {} a -> s {payloads = a} :: SampleChannelDataResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
sampleChannelDataResponse_httpStatus :: Lens.Lens' SampleChannelDataResponse Core.Int
sampleChannelDataResponse_httpStatus = Lens.lens (\SampleChannelDataResponse' {httpStatus} -> httpStatus) (\s@SampleChannelDataResponse' {} a -> s {httpStatus = a} :: SampleChannelDataResponse)

instance Core.NFData SampleChannelDataResponse
