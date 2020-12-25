{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.GetProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the details of a room profile by profile ARN.
module Network.AWS.AlexaBusiness.GetProfile
  ( -- * Creating a request
    GetProfile (..),
    mkGetProfile,

    -- ** Request lenses
    gpProfileArn,

    -- * Destructuring the response
    GetProfileResponse (..),
    mkGetProfileResponse,

    -- ** Response lenses
    gprrsProfile,
    gprrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetProfile' smart constructor.
newtype GetProfile = GetProfile'
  { -- | The ARN of the room profile for which to request details. Required.
    profileArn :: Core.Maybe Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetProfile' value with any optional fields omitted.
mkGetProfile ::
  GetProfile
mkGetProfile = GetProfile' {profileArn = Core.Nothing}

-- | The ARN of the room profile for which to request details. Required.
--
-- /Note:/ Consider using 'profileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpProfileArn :: Lens.Lens' GetProfile (Core.Maybe Types.Arn)
gpProfileArn = Lens.field @"profileArn"
{-# DEPRECATED gpProfileArn "Use generic-lens or generic-optics with 'profileArn' instead." #-}

instance Core.FromJSON GetProfile where
  toJSON GetProfile {..} =
    Core.object
      (Core.catMaybes [("ProfileArn" Core..=) Core.<$> profileArn])

instance Core.AWSRequest GetProfile where
  type Rs GetProfile = GetProfileResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.GetProfile")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProfileResponse'
            Core.<$> (x Core..:? "Profile") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetProfileResponse' smart constructor.
data GetProfileResponse = GetProfileResponse'
  { -- | The details of the room profile requested. Required.
    profile :: Core.Maybe Types.Profile,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetProfileResponse' value with any optional fields omitted.
mkGetProfileResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetProfileResponse
mkGetProfileResponse responseStatus =
  GetProfileResponse' {profile = Core.Nothing, responseStatus}

-- | The details of the room profile requested. Required.
--
-- /Note:/ Consider using 'profile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsProfile :: Lens.Lens' GetProfileResponse (Core.Maybe Types.Profile)
gprrsProfile = Lens.field @"profile"
{-# DEPRECATED gprrsProfile "Use generic-lens or generic-optics with 'profile' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprrsResponseStatus :: Lens.Lens' GetProfileResponse Core.Int
gprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
