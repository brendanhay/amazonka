{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeUserProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe specified users.
--
-- __Required Permissions__ : To use this action, an IAM user must have an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeUserProfiles
  ( -- * Creating a request
    DescribeUserProfiles (..),
    mkDescribeUserProfiles,

    -- ** Request lenses
    dupIamUserArns,

    -- * Destructuring the response
    DescribeUserProfilesResponse (..),
    mkDescribeUserProfilesResponse,

    -- ** Response lenses
    duprrsUserProfiles,
    duprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeUserProfiles' smart constructor.
newtype DescribeUserProfiles = DescribeUserProfiles'
  { -- | An array of IAM or federated user ARNs that identify the users to be described.
    iamUserArns :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUserProfiles' value with any optional fields omitted.
mkDescribeUserProfiles ::
  DescribeUserProfiles
mkDescribeUserProfiles =
  DescribeUserProfiles' {iamUserArns = Core.Nothing}

-- | An array of IAM or federated user ARNs that identify the users to be described.
--
-- /Note:/ Consider using 'iamUserArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupIamUserArns :: Lens.Lens' DescribeUserProfiles (Core.Maybe [Types.String])
dupIamUserArns = Lens.field @"iamUserArns"
{-# DEPRECATED dupIamUserArns "Use generic-lens or generic-optics with 'iamUserArns' instead." #-}

instance Core.FromJSON DescribeUserProfiles where
  toJSON DescribeUserProfiles {..} =
    Core.object
      (Core.catMaybes [("IamUserArns" Core..=) Core.<$> iamUserArns])

instance Core.AWSRequest DescribeUserProfiles where
  type Rs DescribeUserProfiles = DescribeUserProfilesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "OpsWorks_20130218.DescribeUserProfiles")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserProfilesResponse'
            Core.<$> (x Core..:? "UserProfiles") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a @DescribeUserProfiles@ request.
--
-- /See:/ 'mkDescribeUserProfilesResponse' smart constructor.
data DescribeUserProfilesResponse = DescribeUserProfilesResponse'
  { -- | A @Users@ object that describes the specified users.
    userProfiles :: Core.Maybe [Types.UserProfile],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUserProfilesResponse' value with any optional fields omitted.
mkDescribeUserProfilesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeUserProfilesResponse
mkDescribeUserProfilesResponse responseStatus =
  DescribeUserProfilesResponse'
    { userProfiles = Core.Nothing,
      responseStatus
    }

-- | A @Users@ object that describes the specified users.
--
-- /Note:/ Consider using 'userProfiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsUserProfiles :: Lens.Lens' DescribeUserProfilesResponse (Core.Maybe [Types.UserProfile])
duprrsUserProfiles = Lens.field @"userProfiles"
{-# DEPRECATED duprrsUserProfiles "Use generic-lens or generic-optics with 'userProfiles' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsResponseStatus :: Lens.Lens' DescribeUserProfilesResponse Core.Int
duprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED duprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
