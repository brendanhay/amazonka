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
    gncrrsSnsRoleName,
    gncrrsSnsTopicArn,
    gncrrsResponseStatus,
  )
where

import qualified Network.AWS.FMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetNotificationChannel' smart constructor.
data GetNotificationChannel = GetNotificationChannel'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetNotificationChannel' value with any optional fields omitted.
mkGetNotificationChannel ::
  GetNotificationChannel
mkGetNotificationChannel = GetNotificationChannel'

instance Core.FromJSON GetNotificationChannel where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest GetNotificationChannel where
  type Rs GetNotificationChannel = GetNotificationChannelResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSFMS_20180101.GetNotificationChannel")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetNotificationChannelResponse'
            Core.<$> (x Core..:? "SnsRoleName")
            Core.<*> (x Core..:? "SnsTopicArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetNotificationChannelResponse' smart constructor.
data GetNotificationChannelResponse = GetNotificationChannelResponse'
  { -- | The IAM role that is used by AWS Firewall Manager to record activity to SNS.
    snsRoleName :: Core.Maybe Types.SnsRoleName,
    -- | The SNS topic that records AWS Firewall Manager activity.
    snsTopicArn :: Core.Maybe Types.SnsTopicArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetNotificationChannelResponse' value with any optional fields omitted.
mkGetNotificationChannelResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetNotificationChannelResponse
mkGetNotificationChannelResponse responseStatus =
  GetNotificationChannelResponse'
    { snsRoleName = Core.Nothing,
      snsTopicArn = Core.Nothing,
      responseStatus
    }

-- | The IAM role that is used by AWS Firewall Manager to record activity to SNS.
--
-- /Note:/ Consider using 'snsRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gncrrsSnsRoleName :: Lens.Lens' GetNotificationChannelResponse (Core.Maybe Types.SnsRoleName)
gncrrsSnsRoleName = Lens.field @"snsRoleName"
{-# DEPRECATED gncrrsSnsRoleName "Use generic-lens or generic-optics with 'snsRoleName' instead." #-}

-- | The SNS topic that records AWS Firewall Manager activity.
--
-- /Note:/ Consider using 'snsTopicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gncrrsSnsTopicArn :: Lens.Lens' GetNotificationChannelResponse (Core.Maybe Types.SnsTopicArn)
gncrrsSnsTopicArn = Lens.field @"snsTopicArn"
{-# DEPRECATED gncrrsSnsTopicArn "Use generic-lens or generic-optics with 'snsTopicArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gncrrsResponseStatus :: Lens.Lens' GetNotificationChannelResponse Core.Int
gncrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gncrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
