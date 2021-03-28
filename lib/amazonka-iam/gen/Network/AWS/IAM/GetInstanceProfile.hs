{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified instance profile, including the instance profile's path, GUID, ARN, and role. For more information about instance profiles, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles> in the /IAM User Guide/ .
module Network.AWS.IAM.GetInstanceProfile
    (
    -- * Creating a request
      GetInstanceProfile (..)
    , mkGetInstanceProfile
    -- ** Request lenses
    , gipInstanceProfileName

    -- * Destructuring the response
    , GetInstanceProfileResponse (..)
    , mkGetInstanceProfileResponse
    -- ** Response lenses
    , giprrsInstanceProfile
    , giprrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetInstanceProfile' smart constructor.
newtype GetInstanceProfile = GetInstanceProfile'
  { instanceProfileName :: Types.InstanceProfileName
    -- ^ The name of the instance profile to get information about.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetInstanceProfile' value with any optional fields omitted.
mkGetInstanceProfile
    :: Types.InstanceProfileName -- ^ 'instanceProfileName'
    -> GetInstanceProfile
mkGetInstanceProfile instanceProfileName
  = GetInstanceProfile'{instanceProfileName}

-- | The name of the instance profile to get information about.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'instanceProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipInstanceProfileName :: Lens.Lens' GetInstanceProfile Types.InstanceProfileName
gipInstanceProfileName = Lens.field @"instanceProfileName"
{-# INLINEABLE gipInstanceProfileName #-}
{-# DEPRECATED instanceProfileName "Use generic-lens or generic-optics with 'instanceProfileName' instead"  #-}

instance Core.ToQuery GetInstanceProfile where
        toQuery GetInstanceProfile{..}
          = Core.toQueryPair "Action" ("GetInstanceProfile" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "InstanceProfileName" instanceProfileName

instance Core.ToHeaders GetInstanceProfile where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetInstanceProfile where
        type Rs GetInstanceProfile = GetInstanceProfileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "GetInstanceProfileResult"
              (\ s h x ->
                 GetInstanceProfileResponse' Core.<$>
                   (x Core..@ "InstanceProfile") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a successful 'GetInstanceProfile' request. 
--
-- /See:/ 'mkGetInstanceProfileResponse' smart constructor.
data GetInstanceProfileResponse = GetInstanceProfileResponse'
  { instanceProfile :: Types.InstanceProfile
    -- ^ A structure containing details about the instance profile.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetInstanceProfileResponse' value with any optional fields omitted.
mkGetInstanceProfileResponse
    :: Types.InstanceProfile -- ^ 'instanceProfile'
    -> Core.Int -- ^ 'responseStatus'
    -> GetInstanceProfileResponse
mkGetInstanceProfileResponse instanceProfile responseStatus
  = GetInstanceProfileResponse'{instanceProfile, responseStatus}

-- | A structure containing details about the instance profile.
--
-- /Note:/ Consider using 'instanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprrsInstanceProfile :: Lens.Lens' GetInstanceProfileResponse Types.InstanceProfile
giprrsInstanceProfile = Lens.field @"instanceProfile"
{-# INLINEABLE giprrsInstanceProfile #-}
{-# DEPRECATED instanceProfile "Use generic-lens or generic-optics with 'instanceProfile' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprrsResponseStatus :: Lens.Lens' GetInstanceProfileResponse Core.Int
giprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE giprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
