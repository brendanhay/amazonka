{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.GetSendQuota
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides the sending limits for the Amazon SES account.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.GetSendQuota
  ( -- * Creating a request
    GetSendQuota (..),
    mkGetSendQuota,

    -- * Destructuring the response
    GetSendQuotaResponse (..),
    mkGetSendQuotaResponse,

    -- ** Response lenses
    gsqrsMaxSendRate,
    gsqrsSentLast24Hours,
    gsqrsMax24HourSend,
    gsqrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | /See:/ 'mkGetSendQuota' smart constructor.
data GetSendQuota = GetSendQuota'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSendQuota' with the minimum fields required to make a request.
mkGetSendQuota ::
  GetSendQuota
mkGetSendQuota = GetSendQuota'

instance Lude.AWSRequest GetSendQuota where
  type Rs GetSendQuota = GetSendQuotaResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "GetSendQuotaResult"
      ( \s h x ->
          GetSendQuotaResponse'
            Lude.<$> (x Lude..@? "MaxSendRate")
            Lude.<*> (x Lude..@? "SentLast24Hours")
            Lude.<*> (x Lude..@? "Max24HourSend")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSendQuota where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetSendQuota where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSendQuota where
  toQuery =
    Lude.const
      ( Lude.mconcat
          [ "Action" Lude.=: ("GetSendQuota" :: Lude.ByteString),
            "Version" Lude.=: ("2010-12-01" :: Lude.ByteString)
          ]
      )

-- | Represents your Amazon SES daily sending quota, maximum send rate, and the number of emails you have sent in the last 24 hours.
--
-- /See:/ 'mkGetSendQuotaResponse' smart constructor.
data GetSendQuotaResponse = GetSendQuotaResponse'
  { -- | The maximum number of emails that Amazon SES can accept from the user's account per second.
    maxSendRate :: Lude.Maybe Lude.Double,
    -- | The number of emails sent during the previous 24 hours.
    sentLast24Hours :: Lude.Maybe Lude.Double,
    -- | The maximum number of emails the user is allowed to send in a 24-hour interval. A value of -1 signifies an unlimited quota.
    max24HourSend :: Lude.Maybe Lude.Double,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSendQuotaResponse' with the minimum fields required to make a request.
--
-- * 'maxSendRate' - The maximum number of emails that Amazon SES can accept from the user's account per second.
-- * 'sentLast24Hours' - The number of emails sent during the previous 24 hours.
-- * 'max24HourSend' - The maximum number of emails the user is allowed to send in a 24-hour interval. A value of -1 signifies an unlimited quota.
-- * 'responseStatus' - The response status code.
mkGetSendQuotaResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSendQuotaResponse
mkGetSendQuotaResponse pResponseStatus_ =
  GetSendQuotaResponse'
    { maxSendRate = Lude.Nothing,
      sentLast24Hours = Lude.Nothing,
      max24HourSend = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The maximum number of emails that Amazon SES can accept from the user's account per second.
--
-- /Note:/ Consider using 'maxSendRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsqrsMaxSendRate :: Lens.Lens' GetSendQuotaResponse (Lude.Maybe Lude.Double)
gsqrsMaxSendRate = Lens.lens (maxSendRate :: GetSendQuotaResponse -> Lude.Maybe Lude.Double) (\s a -> s {maxSendRate = a} :: GetSendQuotaResponse)
{-# DEPRECATED gsqrsMaxSendRate "Use generic-lens or generic-optics with 'maxSendRate' instead." #-}

-- | The number of emails sent during the previous 24 hours.
--
-- /Note:/ Consider using 'sentLast24Hours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsqrsSentLast24Hours :: Lens.Lens' GetSendQuotaResponse (Lude.Maybe Lude.Double)
gsqrsSentLast24Hours = Lens.lens (sentLast24Hours :: GetSendQuotaResponse -> Lude.Maybe Lude.Double) (\s a -> s {sentLast24Hours = a} :: GetSendQuotaResponse)
{-# DEPRECATED gsqrsSentLast24Hours "Use generic-lens or generic-optics with 'sentLast24Hours' instead." #-}

-- | The maximum number of emails the user is allowed to send in a 24-hour interval. A value of -1 signifies an unlimited quota.
--
-- /Note:/ Consider using 'max24HourSend' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsqrsMax24HourSend :: Lens.Lens' GetSendQuotaResponse (Lude.Maybe Lude.Double)
gsqrsMax24HourSend = Lens.lens (max24HourSend :: GetSendQuotaResponse -> Lude.Maybe Lude.Double) (\s a -> s {max24HourSend = a} :: GetSendQuotaResponse)
{-# DEPRECATED gsqrsMax24HourSend "Use generic-lens or generic-optics with 'max24HourSend' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsqrsResponseStatus :: Lens.Lens' GetSendQuotaResponse Lude.Int
gsqrsResponseStatus = Lens.lens (responseStatus :: GetSendQuotaResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSendQuotaResponse)
{-# DEPRECATED gsqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
