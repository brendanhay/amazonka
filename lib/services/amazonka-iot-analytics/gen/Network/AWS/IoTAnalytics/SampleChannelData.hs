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
    sampleChannelData_startTime,
    sampleChannelData_maxMessages,
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSampleChannelData' smart constructor.
data SampleChannelData = SampleChannelData'
  { -- | The start of the time window from which sample messages are retrieved.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The number of sample messages to be retrieved. The limit is 10. The
    -- default is also 10.
    maxMessages :: Prelude.Maybe Prelude.Natural,
    -- | The end of the time window from which sample messages are retrieved.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the channel whose message samples are retrieved.
    channelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SampleChannelData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTime', 'sampleChannelData_startTime' - The start of the time window from which sample messages are retrieved.
--
-- 'maxMessages', 'sampleChannelData_maxMessages' - The number of sample messages to be retrieved. The limit is 10. The
-- default is also 10.
--
-- 'endTime', 'sampleChannelData_endTime' - The end of the time window from which sample messages are retrieved.
--
-- 'channelName', 'sampleChannelData_channelName' - The name of the channel whose message samples are retrieved.
newSampleChannelData ::
  -- | 'channelName'
  Prelude.Text ->
  SampleChannelData
newSampleChannelData pChannelName_ =
  SampleChannelData'
    { startTime = Prelude.Nothing,
      maxMessages = Prelude.Nothing,
      endTime = Prelude.Nothing,
      channelName = pChannelName_
    }

-- | The start of the time window from which sample messages are retrieved.
sampleChannelData_startTime :: Lens.Lens' SampleChannelData (Prelude.Maybe Prelude.UTCTime)
sampleChannelData_startTime = Lens.lens (\SampleChannelData' {startTime} -> startTime) (\s@SampleChannelData' {} a -> s {startTime = a} :: SampleChannelData) Prelude.. Lens.mapping Core._Time

-- | The number of sample messages to be retrieved. The limit is 10. The
-- default is also 10.
sampleChannelData_maxMessages :: Lens.Lens' SampleChannelData (Prelude.Maybe Prelude.Natural)
sampleChannelData_maxMessages = Lens.lens (\SampleChannelData' {maxMessages} -> maxMessages) (\s@SampleChannelData' {} a -> s {maxMessages = a} :: SampleChannelData)

-- | The end of the time window from which sample messages are retrieved.
sampleChannelData_endTime :: Lens.Lens' SampleChannelData (Prelude.Maybe Prelude.UTCTime)
sampleChannelData_endTime = Lens.lens (\SampleChannelData' {endTime} -> endTime) (\s@SampleChannelData' {} a -> s {endTime = a} :: SampleChannelData) Prelude.. Lens.mapping Core._Time

-- | The name of the channel whose message samples are retrieved.
sampleChannelData_channelName :: Lens.Lens' SampleChannelData Prelude.Text
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
            Prelude.<$> (x Core..?> "payloads")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SampleChannelData

instance Prelude.NFData SampleChannelData

instance Core.ToHeaders SampleChannelData where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath SampleChannelData where
  toPath SampleChannelData' {..} =
    Prelude.mconcat
      ["/channels/", Core.toBS channelName, "/sample"]

instance Core.ToQuery SampleChannelData where
  toQuery SampleChannelData' {..} =
    Prelude.mconcat
      [ "startTime" Core.=: startTime,
        "maxMessages" Core.=: maxMessages,
        "endTime" Core.=: endTime
      ]

-- | /See:/ 'newSampleChannelDataResponse' smart constructor.
data SampleChannelDataResponse = SampleChannelDataResponse'
  { -- | The list of message samples. Each sample message is returned as a
    -- base64-encoded string.
    payloads :: Prelude.Maybe (Prelude.NonEmpty Core.Base64),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  SampleChannelDataResponse
newSampleChannelDataResponse pHttpStatus_ =
  SampleChannelDataResponse'
    { payloads =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of message samples. Each sample message is returned as a
-- base64-encoded string.
sampleChannelDataResponse_payloads :: Lens.Lens' SampleChannelDataResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.ByteString))
sampleChannelDataResponse_payloads = Lens.lens (\SampleChannelDataResponse' {payloads} -> payloads) (\s@SampleChannelDataResponse' {} a -> s {payloads = a} :: SampleChannelDataResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
sampleChannelDataResponse_httpStatus :: Lens.Lens' SampleChannelDataResponse Prelude.Int
sampleChannelDataResponse_httpStatus = Lens.lens (\SampleChannelDataResponse' {httpStatus} -> httpStatus) (\s@SampleChannelDataResponse' {} a -> s {httpStatus = a} :: SampleChannelDataResponse)

instance Prelude.NFData SampleChannelDataResponse
