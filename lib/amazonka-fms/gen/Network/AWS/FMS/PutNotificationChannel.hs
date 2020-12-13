{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.PutNotificationChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Designates the IAM role and Amazon Simple Notification Service (SNS) topic that AWS Firewall Manager uses to record SNS logs.
--
-- To perform this action outside of the console, you must configure the SNS topic to allow the Firewall Manager role @AWSServiceRoleForFMS@ to publish SNS logs. For more information, see <https://docs.aws.amazon.com/waf/latest/developerguide/fms-api-permissions-ref.html Firewall Manager required permissions for API actions> in the /AWS Firewall Manager Developer Guide/ .
module Network.AWS.FMS.PutNotificationChannel
  ( -- * Creating a request
    PutNotificationChannel (..),
    mkPutNotificationChannel,

    -- ** Request lenses
    pncSNSTopicARN,
    pncSNSRoleName,

    -- * Destructuring the response
    PutNotificationChannelResponse (..),
    mkPutNotificationChannelResponse,
  )
where

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutNotificationChannel' smart constructor.
data PutNotificationChannel = PutNotificationChannel'
  { -- | The Amazon Resource Name (ARN) of the SNS topic that collects notifications from AWS Firewall Manager.
    snsTopicARN :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that allows Amazon SNS to record AWS Firewall Manager activity.
    snsRoleName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutNotificationChannel' with the minimum fields required to make a request.
--
-- * 'snsTopicARN' - The Amazon Resource Name (ARN) of the SNS topic that collects notifications from AWS Firewall Manager.
-- * 'snsRoleName' - The Amazon Resource Name (ARN) of the IAM role that allows Amazon SNS to record AWS Firewall Manager activity.
mkPutNotificationChannel ::
  -- | 'snsTopicARN'
  Lude.Text ->
  -- | 'snsRoleName'
  Lude.Text ->
  PutNotificationChannel
mkPutNotificationChannel pSNSTopicARN_ pSNSRoleName_ =
  PutNotificationChannel'
    { snsTopicARN = pSNSTopicARN_,
      snsRoleName = pSNSRoleName_
    }

-- | The Amazon Resource Name (ARN) of the SNS topic that collects notifications from AWS Firewall Manager.
--
-- /Note:/ Consider using 'snsTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pncSNSTopicARN :: Lens.Lens' PutNotificationChannel Lude.Text
pncSNSTopicARN = Lens.lens (snsTopicARN :: PutNotificationChannel -> Lude.Text) (\s a -> s {snsTopicARN = a} :: PutNotificationChannel)
{-# DEPRECATED pncSNSTopicARN "Use generic-lens or generic-optics with 'snsTopicARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role that allows Amazon SNS to record AWS Firewall Manager activity.
--
-- /Note:/ Consider using 'snsRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pncSNSRoleName :: Lens.Lens' PutNotificationChannel Lude.Text
pncSNSRoleName = Lens.lens (snsRoleName :: PutNotificationChannel -> Lude.Text) (\s a -> s {snsRoleName = a} :: PutNotificationChannel)
{-# DEPRECATED pncSNSRoleName "Use generic-lens or generic-optics with 'snsRoleName' instead." #-}

instance Lude.AWSRequest PutNotificationChannel where
  type Rs PutNotificationChannel = PutNotificationChannelResponse
  request = Req.postJSON fmsService
  response = Res.receiveNull PutNotificationChannelResponse'

instance Lude.ToHeaders PutNotificationChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSFMS_20180101.PutNotificationChannel" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutNotificationChannel where
  toJSON PutNotificationChannel' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SnsTopicArn" Lude..= snsTopicARN),
            Lude.Just ("SnsRoleName" Lude..= snsRoleName)
          ]
      )

instance Lude.ToPath PutNotificationChannel where
  toPath = Lude.const "/"

instance Lude.ToQuery PutNotificationChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutNotificationChannelResponse' smart constructor.
data PutNotificationChannelResponse = PutNotificationChannelResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutNotificationChannelResponse' with the minimum fields required to make a request.
mkPutNotificationChannelResponse ::
  PutNotificationChannelResponse
mkPutNotificationChannelResponse = PutNotificationChannelResponse'
