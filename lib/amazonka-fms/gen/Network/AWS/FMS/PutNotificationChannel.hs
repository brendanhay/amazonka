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
    pncSnsTopicArn,
    pncSnsRoleName,

    -- * Destructuring the response
    PutNotificationChannelResponse (..),
    mkPutNotificationChannelResponse,
  )
where

import qualified Network.AWS.FMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutNotificationChannel' smart constructor.
data PutNotificationChannel = PutNotificationChannel'
  { -- | The Amazon Resource Name (ARN) of the SNS topic that collects notifications from AWS Firewall Manager.
    snsTopicArn :: Types.SnsTopicArn,
    -- | The Amazon Resource Name (ARN) of the IAM role that allows Amazon SNS to record AWS Firewall Manager activity.
    snsRoleName :: Types.SnsRoleName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutNotificationChannel' value with any optional fields omitted.
mkPutNotificationChannel ::
  -- | 'snsTopicArn'
  Types.SnsTopicArn ->
  -- | 'snsRoleName'
  Types.SnsRoleName ->
  PutNotificationChannel
mkPutNotificationChannel snsTopicArn snsRoleName =
  PutNotificationChannel' {snsTopicArn, snsRoleName}

-- | The Amazon Resource Name (ARN) of the SNS topic that collects notifications from AWS Firewall Manager.
--
-- /Note:/ Consider using 'snsTopicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pncSnsTopicArn :: Lens.Lens' PutNotificationChannel Types.SnsTopicArn
pncSnsTopicArn = Lens.field @"snsTopicArn"
{-# DEPRECATED pncSnsTopicArn "Use generic-lens or generic-optics with 'snsTopicArn' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role that allows Amazon SNS to record AWS Firewall Manager activity.
--
-- /Note:/ Consider using 'snsRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pncSnsRoleName :: Lens.Lens' PutNotificationChannel Types.SnsRoleName
pncSnsRoleName = Lens.field @"snsRoleName"
{-# DEPRECATED pncSnsRoleName "Use generic-lens or generic-optics with 'snsRoleName' instead." #-}

instance Core.FromJSON PutNotificationChannel where
  toJSON PutNotificationChannel {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SnsTopicArn" Core..= snsTopicArn),
            Core.Just ("SnsRoleName" Core..= snsRoleName)
          ]
      )

instance Core.AWSRequest PutNotificationChannel where
  type Rs PutNotificationChannel = PutNotificationChannelResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSFMS_20180101.PutNotificationChannel")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull PutNotificationChannelResponse'

-- | /See:/ 'mkPutNotificationChannelResponse' smart constructor.
data PutNotificationChannelResponse = PutNotificationChannelResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutNotificationChannelResponse' value with any optional fields omitted.
mkPutNotificationChannelResponse ::
  PutNotificationChannelResponse
mkPutNotificationChannelResponse = PutNotificationChannelResponse'
