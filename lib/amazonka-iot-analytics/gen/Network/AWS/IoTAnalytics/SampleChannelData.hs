{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.SampleChannelData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a sample of messages from the specified channel ingested during the specified timeframe. Up to 10 messages can be retrieved.
module Network.AWS.IoTAnalytics.SampleChannelData
  ( -- * Creating a request
    SampleChannelData (..),
    mkSampleChannelData,

    -- ** Request lenses
    scdStartTime,
    scdMaxMessages,
    scdEndTime,
    scdChannelName,

    -- * Destructuring the response
    SampleChannelDataResponse (..),
    mkSampleChannelDataResponse,

    -- ** Response lenses
    scdrsPayloads,
    scdrsResponseStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSampleChannelData' smart constructor.
data SampleChannelData = SampleChannelData'
  { startTime ::
      Lude.Maybe Lude.Timestamp,
    maxMessages :: Lude.Maybe Lude.Natural,
    endTime :: Lude.Maybe Lude.Timestamp,
    channelName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SampleChannelData' with the minimum fields required to make a request.
--
-- * 'channelName' - The name of the channel whose message samples are retrieved.
-- * 'endTime' - The end of the time window from which sample messages are retrieved.
-- * 'maxMessages' - The number of sample messages to be retrieved. The limit is 10. The default is also 10.
-- * 'startTime' - The start of the time window from which sample messages are retrieved.
mkSampleChannelData ::
  -- | 'channelName'
  Lude.Text ->
  SampleChannelData
mkSampleChannelData pChannelName_ =
  SampleChannelData'
    { startTime = Lude.Nothing,
      maxMessages = Lude.Nothing,
      endTime = Lude.Nothing,
      channelName = pChannelName_
    }

-- | The start of the time window from which sample messages are retrieved.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scdStartTime :: Lens.Lens' SampleChannelData (Lude.Maybe Lude.Timestamp)
scdStartTime = Lens.lens (startTime :: SampleChannelData -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: SampleChannelData)
{-# DEPRECATED scdStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The number of sample messages to be retrieved. The limit is 10. The default is also 10.
--
-- /Note:/ Consider using 'maxMessages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scdMaxMessages :: Lens.Lens' SampleChannelData (Lude.Maybe Lude.Natural)
scdMaxMessages = Lens.lens (maxMessages :: SampleChannelData -> Lude.Maybe Lude.Natural) (\s a -> s {maxMessages = a} :: SampleChannelData)
{-# DEPRECATED scdMaxMessages "Use generic-lens or generic-optics with 'maxMessages' instead." #-}

-- | The end of the time window from which sample messages are retrieved.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scdEndTime :: Lens.Lens' SampleChannelData (Lude.Maybe Lude.Timestamp)
scdEndTime = Lens.lens (endTime :: SampleChannelData -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: SampleChannelData)
{-# DEPRECATED scdEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The name of the channel whose message samples are retrieved.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scdChannelName :: Lens.Lens' SampleChannelData Lude.Text
scdChannelName = Lens.lens (channelName :: SampleChannelData -> Lude.Text) (\s a -> s {channelName = a} :: SampleChannelData)
{-# DEPRECATED scdChannelName "Use generic-lens or generic-optics with 'channelName' instead." #-}

instance Lude.AWSRequest SampleChannelData where
  type Rs SampleChannelData = SampleChannelDataResponse
  request = Req.get ioTAnalyticsService
  response =
    Res.receiveJSON
      ( \s h x ->
          SampleChannelDataResponse'
            Lude.<$> (x Lude..?> "payloads") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SampleChannelData where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SampleChannelData where
  toPath SampleChannelData' {..} =
    Lude.mconcat ["/channels/", Lude.toBS channelName, "/sample"]

instance Lude.ToQuery SampleChannelData where
  toQuery SampleChannelData' {..} =
    Lude.mconcat
      [ "startTime" Lude.=: startTime,
        "maxMessages" Lude.=: maxMessages,
        "endTime" Lude.=: endTime
      ]

-- | /See:/ 'mkSampleChannelDataResponse' smart constructor.
data SampleChannelDataResponse = SampleChannelDataResponse'
  { payloads ::
      Lude.Maybe (Lude.NonEmpty Lude.Base64),
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SampleChannelDataResponse' with the minimum fields required to make a request.
--
-- * 'payloads' - The list of message samples. Each sample message is returned as a base64-encoded string.
-- * 'responseStatus' - The response status code.
mkSampleChannelDataResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SampleChannelDataResponse
mkSampleChannelDataResponse pResponseStatus_ =
  SampleChannelDataResponse'
    { payloads = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of message samples. Each sample message is returned as a base64-encoded string.
--
-- /Note:/ Consider using 'payloads' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scdrsPayloads :: Lens.Lens' SampleChannelDataResponse (Lude.Maybe (Lude.NonEmpty Lude.Base64))
scdrsPayloads = Lens.lens (payloads :: SampleChannelDataResponse -> Lude.Maybe (Lude.NonEmpty Lude.Base64)) (\s a -> s {payloads = a} :: SampleChannelDataResponse)
{-# DEPRECATED scdrsPayloads "Use generic-lens or generic-optics with 'payloads' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scdrsResponseStatus :: Lens.Lens' SampleChannelDataResponse Lude.Int
scdrsResponseStatus = Lens.lens (responseStatus :: SampleChannelDataResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SampleChannelDataResponse)
{-# DEPRECATED scdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
