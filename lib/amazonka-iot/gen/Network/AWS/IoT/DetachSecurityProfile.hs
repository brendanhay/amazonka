{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DetachSecurityProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a Device Defender security profile from a thing group or from this account.
module Network.AWS.IoT.DetachSecurityProfile
    (
    -- * Creating a request
      DetachSecurityProfile (..)
    , mkDetachSecurityProfile
    -- ** Request lenses
    , dspfSecurityProfileName
    , dspfSecurityProfileTargetArn

    -- * Destructuring the response
    , DetachSecurityProfileResponse (..)
    , mkDetachSecurityProfileResponse
    -- ** Response lenses
    , dsprgrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetachSecurityProfile' smart constructor.
data DetachSecurityProfile = DetachSecurityProfile'
  { securityProfileName :: Types.SecurityProfileName
    -- ^ The security profile that is detached.
  , securityProfileTargetArn :: Types.SecurityProfileTargetArn
    -- ^ The ARN of the thing group from which the security profile is detached.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachSecurityProfile' value with any optional fields omitted.
mkDetachSecurityProfile
    :: Types.SecurityProfileName -- ^ 'securityProfileName'
    -> Types.SecurityProfileTargetArn -- ^ 'securityProfileTargetArn'
    -> DetachSecurityProfile
mkDetachSecurityProfile securityProfileName
  securityProfileTargetArn
  = DetachSecurityProfile'{securityProfileName,
                           securityProfileTargetArn}

-- | The security profile that is detached.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspfSecurityProfileName :: Lens.Lens' DetachSecurityProfile Types.SecurityProfileName
dspfSecurityProfileName = Lens.field @"securityProfileName"
{-# INLINEABLE dspfSecurityProfileName #-}
{-# DEPRECATED securityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead"  #-}

-- | The ARN of the thing group from which the security profile is detached.
--
-- /Note:/ Consider using 'securityProfileTargetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspfSecurityProfileTargetArn :: Lens.Lens' DetachSecurityProfile Types.SecurityProfileTargetArn
dspfSecurityProfileTargetArn = Lens.field @"securityProfileTargetArn"
{-# INLINEABLE dspfSecurityProfileTargetArn #-}
{-# DEPRECATED securityProfileTargetArn "Use generic-lens or generic-optics with 'securityProfileTargetArn' instead"  #-}

instance Core.ToQuery DetachSecurityProfile where
        toQuery DetachSecurityProfile{..}
          = Core.toQueryPair "securityProfileTargetArn"
              securityProfileTargetArn

instance Core.ToHeaders DetachSecurityProfile where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DetachSecurityProfile where
        type Rs DetachSecurityProfile = DetachSecurityProfileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/security-profiles/" Core.<> Core.toText securityProfileName
                             Core.<> "/targets",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DetachSecurityProfileResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDetachSecurityProfileResponse' smart constructor.
newtype DetachSecurityProfileResponse = DetachSecurityProfileResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DetachSecurityProfileResponse' value with any optional fields omitted.
mkDetachSecurityProfileResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DetachSecurityProfileResponse
mkDetachSecurityProfileResponse responseStatus
  = DetachSecurityProfileResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprgrsResponseStatus :: Lens.Lens' DetachSecurityProfileResponse Core.Int
dsprgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsprgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
