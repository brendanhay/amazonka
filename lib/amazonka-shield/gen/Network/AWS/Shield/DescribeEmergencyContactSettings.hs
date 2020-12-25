{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DescribeEmergencyContactSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A list of email addresses and phone numbers that the DDoS Response Team (DRT) can use to contact you if you have proactive engagement enabled, for escalations to the DRT and to initiate proactive customer support.
module Network.AWS.Shield.DescribeEmergencyContactSettings
  ( -- * Creating a request
    DescribeEmergencyContactSettings (..),
    mkDescribeEmergencyContactSettings,

    -- * Destructuring the response
    DescribeEmergencyContactSettingsResponse (..),
    mkDescribeEmergencyContactSettingsResponse,

    -- ** Response lenses
    decsrrsEmergencyContactList,
    decsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkDescribeEmergencyContactSettings' smart constructor.
data DescribeEmergencyContactSettings = DescribeEmergencyContactSettings'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEmergencyContactSettings' value with any optional fields omitted.
mkDescribeEmergencyContactSettings ::
  DescribeEmergencyContactSettings
mkDescribeEmergencyContactSettings =
  DescribeEmergencyContactSettings'

instance Core.FromJSON DescribeEmergencyContactSettings where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DescribeEmergencyContactSettings where
  type
    Rs DescribeEmergencyContactSettings =
      DescribeEmergencyContactSettingsResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSShield_20160616.DescribeEmergencyContactSettings"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEmergencyContactSettingsResponse'
            Core.<$> (x Core..:? "EmergencyContactList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeEmergencyContactSettingsResponse' smart constructor.
data DescribeEmergencyContactSettingsResponse = DescribeEmergencyContactSettingsResponse'
  { -- | A list of email addresses and phone numbers that the DDoS Response Team (DRT) can use to contact you if you have proactive engagement enabled, for escalations to the DRT and to initiate proactive customer support.
    emergencyContactList :: Core.Maybe [Types.EmergencyContact],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEmergencyContactSettingsResponse' value with any optional fields omitted.
mkDescribeEmergencyContactSettingsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeEmergencyContactSettingsResponse
mkDescribeEmergencyContactSettingsResponse responseStatus =
  DescribeEmergencyContactSettingsResponse'
    { emergencyContactList =
        Core.Nothing,
      responseStatus
    }

-- | A list of email addresses and phone numbers that the DDoS Response Team (DRT) can use to contact you if you have proactive engagement enabled, for escalations to the DRT and to initiate proactive customer support.
--
-- /Note:/ Consider using 'emergencyContactList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decsrrsEmergencyContactList :: Lens.Lens' DescribeEmergencyContactSettingsResponse (Core.Maybe [Types.EmergencyContact])
decsrrsEmergencyContactList = Lens.field @"emergencyContactList"
{-# DEPRECATED decsrrsEmergencyContactList "Use generic-lens or generic-optics with 'emergencyContactList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decsrrsResponseStatus :: Lens.Lens' DescribeEmergencyContactSettingsResponse Core.Int
decsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED decsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
