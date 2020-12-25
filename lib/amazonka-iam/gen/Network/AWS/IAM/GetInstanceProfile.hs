{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetInstanceProfile (..),
    mkGetInstanceProfile,

    -- ** Request lenses
    gipInstanceProfileName,

    -- * Destructuring the response
    GetInstanceProfileResponse (..),
    mkGetInstanceProfileResponse,

    -- ** Response lenses
    giprrsInstanceProfile,
    giprrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetInstanceProfile' smart constructor.
newtype GetInstanceProfile = GetInstanceProfile'
  { -- | The name of the instance profile to get information about.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    instanceProfileName :: Types.InstanceProfileName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetInstanceProfile' value with any optional fields omitted.
mkGetInstanceProfile ::
  -- | 'instanceProfileName'
  Types.InstanceProfileName ->
  GetInstanceProfile
mkGetInstanceProfile instanceProfileName =
  GetInstanceProfile' {instanceProfileName}

-- | The name of the instance profile to get information about.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'instanceProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipInstanceProfileName :: Lens.Lens' GetInstanceProfile Types.InstanceProfileName
gipInstanceProfileName = Lens.field @"instanceProfileName"
{-# DEPRECATED gipInstanceProfileName "Use generic-lens or generic-optics with 'instanceProfileName' instead." #-}

instance Core.AWSRequest GetInstanceProfile where
  type Rs GetInstanceProfile = GetInstanceProfileResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "GetInstanceProfile")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "InstanceProfileName" instanceProfileName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "GetInstanceProfileResult"
      ( \s h x ->
          GetInstanceProfileResponse'
            Core.<$> (x Core..@ "InstanceProfile")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a successful 'GetInstanceProfile' request.
--
-- /See:/ 'mkGetInstanceProfileResponse' smart constructor.
data GetInstanceProfileResponse = GetInstanceProfileResponse'
  { -- | A structure containing details about the instance profile.
    instanceProfile :: Types.InstanceProfile,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetInstanceProfileResponse' value with any optional fields omitted.
mkGetInstanceProfileResponse ::
  -- | 'instanceProfile'
  Types.InstanceProfile ->
  -- | 'responseStatus'
  Core.Int ->
  GetInstanceProfileResponse
mkGetInstanceProfileResponse instanceProfile responseStatus =
  GetInstanceProfileResponse' {instanceProfile, responseStatus}

-- | A structure containing details about the instance profile.
--
-- /Note:/ Consider using 'instanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprrsInstanceProfile :: Lens.Lens' GetInstanceProfileResponse Types.InstanceProfile
giprrsInstanceProfile = Lens.field @"instanceProfile"
{-# DEPRECATED giprrsInstanceProfile "Use generic-lens or generic-optics with 'instanceProfile' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprrsResponseStatus :: Lens.Lens' GetInstanceProfileResponse Core.Int
giprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED giprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
