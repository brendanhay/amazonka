{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteSecurityProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Device Defender security profile.
module Network.AWS.IoT.DeleteSecurityProfile
    (
    -- * Creating a request
      DeleteSecurityProfile (..)
    , mkDeleteSecurityProfile
    -- ** Request lenses
    , dspSecurityProfileName
    , dspExpectedVersion

    -- * Destructuring the response
    , DeleteSecurityProfileResponse (..)
    , mkDeleteSecurityProfileResponse
    -- ** Response lenses
    , dsprrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteSecurityProfile' smart constructor.
data DeleteSecurityProfile = DeleteSecurityProfile'
  { securityProfileName :: Types.SecurityProfileName
    -- ^ The name of the security profile to be deleted.
  , expectedVersion :: Core.Maybe Core.Integer
    -- ^ The expected version of the security profile. A new version is generated whenever the security profile is updated. If you specify a value that is different from the actual version, a @VersionConflictException@ is thrown.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSecurityProfile' value with any optional fields omitted.
mkDeleteSecurityProfile
    :: Types.SecurityProfileName -- ^ 'securityProfileName'
    -> DeleteSecurityProfile
mkDeleteSecurityProfile securityProfileName
  = DeleteSecurityProfile'{securityProfileName,
                           expectedVersion = Core.Nothing}

-- | The name of the security profile to be deleted.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspSecurityProfileName :: Lens.Lens' DeleteSecurityProfile Types.SecurityProfileName
dspSecurityProfileName = Lens.field @"securityProfileName"
{-# INLINEABLE dspSecurityProfileName #-}
{-# DEPRECATED securityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead"  #-}

-- | The expected version of the security profile. A new version is generated whenever the security profile is updated. If you specify a value that is different from the actual version, a @VersionConflictException@ is thrown.
--
-- /Note:/ Consider using 'expectedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspExpectedVersion :: Lens.Lens' DeleteSecurityProfile (Core.Maybe Core.Integer)
dspExpectedVersion = Lens.field @"expectedVersion"
{-# INLINEABLE dspExpectedVersion #-}
{-# DEPRECATED expectedVersion "Use generic-lens or generic-optics with 'expectedVersion' instead"  #-}

instance Core.ToQuery DeleteSecurityProfile where
        toQuery DeleteSecurityProfile{..}
          = Core.maybe Core.mempty (Core.toQueryPair "expectedVersion")
              expectedVersion

instance Core.ToHeaders DeleteSecurityProfile where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteSecurityProfile where
        type Rs DeleteSecurityProfile = DeleteSecurityProfileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/security-profiles/" Core.<> Core.toText securityProfileName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteSecurityProfileResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteSecurityProfileResponse' smart constructor.
newtype DeleteSecurityProfileResponse = DeleteSecurityProfileResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSecurityProfileResponse' value with any optional fields omitted.
mkDeleteSecurityProfileResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteSecurityProfileResponse
mkDeleteSecurityProfileResponse responseStatus
  = DeleteSecurityProfileResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsResponseStatus :: Lens.Lens' DeleteSecurityProfileResponse Core.Int
dsprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
