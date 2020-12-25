{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.DeleteUserProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user profile in AWS CodeStar, including all personal preference data associated with that profile, such as display name and email address. It does not delete the history of that user, for example the history of commits made by that user.
module Network.AWS.CodeStar.DeleteUserProfile
  ( -- * Creating a request
    DeleteUserProfile (..),
    mkDeleteUserProfile,

    -- ** Request lenses
    dUserArn,

    -- * Destructuring the response
    DeleteUserProfileResponse (..),
    mkDeleteUserProfileResponse,

    -- ** Response lenses
    duprfrsUserArn,
    duprfrsResponseStatus,
  )
where

import qualified Network.AWS.CodeStar.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteUserProfile' smart constructor.
newtype DeleteUserProfile = DeleteUserProfile'
  { -- | The Amazon Resource Name (ARN) of the user to delete from AWS CodeStar.
    userArn :: Types.UserArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserProfile' value with any optional fields omitted.
mkDeleteUserProfile ::
  -- | 'userArn'
  Types.UserArn ->
  DeleteUserProfile
mkDeleteUserProfile userArn = DeleteUserProfile' {userArn}

-- | The Amazon Resource Name (ARN) of the user to delete from AWS CodeStar.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dUserArn :: Lens.Lens' DeleteUserProfile Types.UserArn
dUserArn = Lens.field @"userArn"
{-# DEPRECATED dUserArn "Use generic-lens or generic-optics with 'userArn' instead." #-}

instance Core.FromJSON DeleteUserProfile where
  toJSON DeleteUserProfile {..} =
    Core.object
      (Core.catMaybes [Core.Just ("userArn" Core..= userArn)])

instance Core.AWSRequest DeleteUserProfile where
  type Rs DeleteUserProfile = DeleteUserProfileResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeStar_20170419.DeleteUserProfile")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteUserProfileResponse'
            Core.<$> (x Core..: "userArn") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteUserProfileResponse' smart constructor.
data DeleteUserProfileResponse = DeleteUserProfileResponse'
  { -- | The Amazon Resource Name (ARN) of the user deleted from AWS CodeStar.
    userArn :: Types.UserArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserProfileResponse' value with any optional fields omitted.
mkDeleteUserProfileResponse ::
  -- | 'userArn'
  Types.UserArn ->
  -- | 'responseStatus'
  Core.Int ->
  DeleteUserProfileResponse
mkDeleteUserProfileResponse userArn responseStatus =
  DeleteUserProfileResponse' {userArn, responseStatus}

-- | The Amazon Resource Name (ARN) of the user deleted from AWS CodeStar.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprfrsUserArn :: Lens.Lens' DeleteUserProfileResponse Types.UserArn
duprfrsUserArn = Lens.field @"userArn"
{-# DEPRECATED duprfrsUserArn "Use generic-lens or generic-optics with 'userArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprfrsResponseStatus :: Lens.Lens' DeleteUserProfileResponse Core.Int
duprfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED duprfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
