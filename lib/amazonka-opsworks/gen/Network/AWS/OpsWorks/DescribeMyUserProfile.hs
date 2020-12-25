{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeMyUserProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a user's SSH information.
--
-- __Required Permissions__ : To use this action, an IAM user must have self-management enabled or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeMyUserProfile
  ( -- * Creating a request
    DescribeMyUserProfile (..),
    mkDescribeMyUserProfile,

    -- * Destructuring the response
    DescribeMyUserProfileResponse (..),
    mkDescribeMyUserProfileResponse,

    -- ** Response lenses
    dmuprrsUserProfile,
    dmuprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeMyUserProfile' smart constructor.
data DescribeMyUserProfile = DescribeMyUserProfile'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMyUserProfile' value with any optional fields omitted.
mkDescribeMyUserProfile ::
  DescribeMyUserProfile
mkDescribeMyUserProfile = DescribeMyUserProfile'

instance Core.FromJSON DescribeMyUserProfile where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DescribeMyUserProfile where
  type Rs DescribeMyUserProfile = DescribeMyUserProfileResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "OpsWorks_20130218.DescribeMyUserProfile")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMyUserProfileResponse'
            Core.<$> (x Core..:? "UserProfile") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a @DescribeMyUserProfile@ request.
--
-- /See:/ 'mkDescribeMyUserProfileResponse' smart constructor.
data DescribeMyUserProfileResponse = DescribeMyUserProfileResponse'
  { -- | A @UserProfile@ object that describes the user's SSH information.
    userProfile :: Core.Maybe Types.SelfUserProfile,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMyUserProfileResponse' value with any optional fields omitted.
mkDescribeMyUserProfileResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeMyUserProfileResponse
mkDescribeMyUserProfileResponse responseStatus =
  DescribeMyUserProfileResponse'
    { userProfile = Core.Nothing,
      responseStatus
    }

-- | A @UserProfile@ object that describes the user's SSH information.
--
-- /Note:/ Consider using 'userProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmuprrsUserProfile :: Lens.Lens' DescribeMyUserProfileResponse (Core.Maybe Types.SelfUserProfile)
dmuprrsUserProfile = Lens.field @"userProfile"
{-# DEPRECATED dmuprrsUserProfile "Use generic-lens or generic-optics with 'userProfile' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmuprrsResponseStatus :: Lens.Lens' DescribeMyUserProfileResponse Core.Int
dmuprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmuprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
