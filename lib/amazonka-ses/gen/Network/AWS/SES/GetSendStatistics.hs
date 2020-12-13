{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.GetSendStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides sending statistics for the current AWS Region. The result is a list of data points, representing the last two weeks of sending activity. Each data point in the list contains statistics for a 15-minute period of time.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.GetSendStatistics
  ( -- * Creating a request
    GetSendStatistics (..),
    mkGetSendStatistics,

    -- * Destructuring the response
    GetSendStatisticsResponse (..),
    mkGetSendStatisticsResponse,

    -- ** Response lenses
    gssrsSendDataPoints,
    gssrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | /See:/ 'mkGetSendStatistics' smart constructor.
data GetSendStatistics = GetSendStatistics'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSendStatistics' with the minimum fields required to make a request.
mkGetSendStatistics ::
  GetSendStatistics
mkGetSendStatistics = GetSendStatistics'

instance Lude.AWSRequest GetSendStatistics where
  type Rs GetSendStatistics = GetSendStatisticsResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "GetSendStatisticsResult"
      ( \s h x ->
          GetSendStatisticsResponse'
            Lude.<$> ( x Lude..@? "SendDataPoints" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSendStatistics where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetSendStatistics where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSendStatistics where
  toQuery =
    Lude.const
      ( Lude.mconcat
          [ "Action" Lude.=: ("GetSendStatistics" :: Lude.ByteString),
            "Version" Lude.=: ("2010-12-01" :: Lude.ByteString)
          ]
      )

-- | Represents a list of data points. This list contains aggregated data from the previous two weeks of your sending activity with Amazon SES.
--
-- /See:/ 'mkGetSendStatisticsResponse' smart constructor.
data GetSendStatisticsResponse = GetSendStatisticsResponse'
  { -- | A list of data points, each of which represents 15 minutes of activity.
    sendDataPoints :: Lude.Maybe [SendDataPoint],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSendStatisticsResponse' with the minimum fields required to make a request.
--
-- * 'sendDataPoints' - A list of data points, each of which represents 15 minutes of activity.
-- * 'responseStatus' - The response status code.
mkGetSendStatisticsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSendStatisticsResponse
mkGetSendStatisticsResponse pResponseStatus_ =
  GetSendStatisticsResponse'
    { sendDataPoints = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of data points, each of which represents 15 minutes of activity.
--
-- /Note:/ Consider using 'sendDataPoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssrsSendDataPoints :: Lens.Lens' GetSendStatisticsResponse (Lude.Maybe [SendDataPoint])
gssrsSendDataPoints = Lens.lens (sendDataPoints :: GetSendStatisticsResponse -> Lude.Maybe [SendDataPoint]) (\s a -> s {sendDataPoints = a} :: GetSendStatisticsResponse)
{-# DEPRECATED gssrsSendDataPoints "Use generic-lens or generic-optics with 'sendDataPoints' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssrsResponseStatus :: Lens.Lens' GetSendStatisticsResponse Lude.Int
gssrsResponseStatus = Lens.lens (responseStatus :: GetSendStatisticsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSendStatisticsResponse)
{-# DEPRECATED gssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
