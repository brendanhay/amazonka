{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeEmergencyContactSettings (..)
    , mkDescribeEmergencyContactSettings

    -- * Destructuring the response
    , DescribeEmergencyContactSettingsResponse (..)
    , mkDescribeEmergencyContactSettingsResponse
    -- ** Response lenses
    , decsrrsEmergencyContactList
    , decsrrsResponseStatus
    ) where

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
mkDescribeEmergencyContactSettings
    :: DescribeEmergencyContactSettings
mkDescribeEmergencyContactSettings
  = DescribeEmergencyContactSettings'

instance Core.ToQuery DescribeEmergencyContactSettings where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeEmergencyContactSettings where
        toHeaders DescribeEmergencyContactSettings{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSShield_20160616.DescribeEmergencyContactSettings")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeEmergencyContactSettings where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DescribeEmergencyContactSettings where
        type Rs DescribeEmergencyContactSettings =
             DescribeEmergencyContactSettingsResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeEmergencyContactSettingsResponse' Core.<$>
                   (x Core..:? "EmergencyContactList") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeEmergencyContactSettingsResponse' smart constructor.
data DescribeEmergencyContactSettingsResponse = DescribeEmergencyContactSettingsResponse'
  { emergencyContactList :: Core.Maybe [Types.EmergencyContact]
    -- ^ A list of email addresses and phone numbers that the DDoS Response Team (DRT) can use to contact you if you have proactive engagement enabled, for escalations to the DRT and to initiate proactive customer support.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEmergencyContactSettingsResponse' value with any optional fields omitted.
mkDescribeEmergencyContactSettingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeEmergencyContactSettingsResponse
mkDescribeEmergencyContactSettingsResponse responseStatus
  = DescribeEmergencyContactSettingsResponse'{emergencyContactList =
                                                Core.Nothing,
                                              responseStatus}

-- | A list of email addresses and phone numbers that the DDoS Response Team (DRT) can use to contact you if you have proactive engagement enabled, for escalations to the DRT and to initiate proactive customer support.
--
-- /Note:/ Consider using 'emergencyContactList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decsrrsEmergencyContactList :: Lens.Lens' DescribeEmergencyContactSettingsResponse (Core.Maybe [Types.EmergencyContact])
decsrrsEmergencyContactList = Lens.field @"emergencyContactList"
{-# INLINEABLE decsrrsEmergencyContactList #-}
{-# DEPRECATED emergencyContactList "Use generic-lens or generic-optics with 'emergencyContactList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
decsrrsResponseStatus :: Lens.Lens' DescribeEmergencyContactSettingsResponse Core.Int
decsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE decsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
