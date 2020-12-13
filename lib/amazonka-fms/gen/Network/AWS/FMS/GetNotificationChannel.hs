{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.GetNotificationChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Information about the Amazon Simple Notification Service (SNS) topic that is used to record AWS Firewall Manager SNS logs.
module Network.AWS.FMS.GetNotificationChannel
  ( -- * Creating a request
    GetNotificationChannel (..),
    mkGetNotificationChannel,

    -- * Destructuring the response
    GetNotificationChannelResponse (..),
    mkGetNotificationChannelResponse,

    -- ** Response lenses
    gncrsSNSTopicARN,
    gncrsSNSRoleName,
    gncrsResponseStatus,
  )
where

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetNotificationChannel' smart constructor.
data GetNotificationChannel = GetNotificationChannel'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetNotificationChannel' with the minimum fields required to make a request.
mkGetNotificationChannel ::
  GetNotificationChannel
mkGetNotificationChannel = GetNotificationChannel'

instance Lude.AWSRequest GetNotificationChannel where
  type Rs GetNotificationChannel = GetNotificationChannelResponse
  request = Req.postJSON fmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetNotificationChannelResponse'
            Lude.<$> (x Lude..?> "SnsTopicArn")
            Lude.<*> (x Lude..?> "SnsRoleName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetNotificationChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSFMS_20180101.GetNotificationChannel" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetNotificationChannel where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath GetNotificationChannel where
  toPath = Lude.const "/"

instance Lude.ToQuery GetNotificationChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetNotificationChannelResponse' smart constructor.
data GetNotificationChannelResponse = GetNotificationChannelResponse'
  { -- | The SNS topic that records AWS Firewall Manager activity.
    snsTopicARN :: Lude.Maybe Lude.Text,
    -- | The IAM role that is used by AWS Firewall Manager to record activity to SNS.
    snsRoleName :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetNotificationChannelResponse' with the minimum fields required to make a request.
--
-- * 'snsTopicARN' - The SNS topic that records AWS Firewall Manager activity.
-- * 'snsRoleName' - The IAM role that is used by AWS Firewall Manager to record activity to SNS.
-- * 'responseStatus' - The response status code.
mkGetNotificationChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetNotificationChannelResponse
mkGetNotificationChannelResponse pResponseStatus_ =
  GetNotificationChannelResponse'
    { snsTopicARN = Lude.Nothing,
      snsRoleName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The SNS topic that records AWS Firewall Manager activity.
--
-- /Note:/ Consider using 'snsTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gncrsSNSTopicARN :: Lens.Lens' GetNotificationChannelResponse (Lude.Maybe Lude.Text)
gncrsSNSTopicARN = Lens.lens (snsTopicARN :: GetNotificationChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {snsTopicARN = a} :: GetNotificationChannelResponse)
{-# DEPRECATED gncrsSNSTopicARN "Use generic-lens or generic-optics with 'snsTopicARN' instead." #-}

-- | The IAM role that is used by AWS Firewall Manager to record activity to SNS.
--
-- /Note:/ Consider using 'snsRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gncrsSNSRoleName :: Lens.Lens' GetNotificationChannelResponse (Lude.Maybe Lude.Text)
gncrsSNSRoleName = Lens.lens (snsRoleName :: GetNotificationChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {snsRoleName = a} :: GetNotificationChannelResponse)
{-# DEPRECATED gncrsSNSRoleName "Use generic-lens or generic-optics with 'snsRoleName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gncrsResponseStatus :: Lens.Lens' GetNotificationChannelResponse Lude.Int
gncrsResponseStatus = Lens.lens (responseStatus :: GetNotificationChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetNotificationChannelResponse)
{-# DEPRECATED gncrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
