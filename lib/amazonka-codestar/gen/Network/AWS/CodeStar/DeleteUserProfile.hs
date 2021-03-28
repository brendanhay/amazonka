{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteUserProfile (..)
    , mkDeleteUserProfile
    -- ** Request lenses
    , dUserArn

    -- * Destructuring the response
    , DeleteUserProfileResponse (..)
    , mkDeleteUserProfileResponse
    -- ** Response lenses
    , duprfrsUserArn
    , duprfrsResponseStatus
    ) where

import qualified Network.AWS.CodeStar.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteUserProfile' smart constructor.
newtype DeleteUserProfile = DeleteUserProfile'
  { userArn :: Types.UserArn
    -- ^ The Amazon Resource Name (ARN) of the user to delete from AWS CodeStar.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserProfile' value with any optional fields omitted.
mkDeleteUserProfile
    :: Types.UserArn -- ^ 'userArn'
    -> DeleteUserProfile
mkDeleteUserProfile userArn = DeleteUserProfile'{userArn}

-- | The Amazon Resource Name (ARN) of the user to delete from AWS CodeStar.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dUserArn :: Lens.Lens' DeleteUserProfile Types.UserArn
dUserArn = Lens.field @"userArn"
{-# INLINEABLE dUserArn #-}
{-# DEPRECATED userArn "Use generic-lens or generic-optics with 'userArn' instead"  #-}

instance Core.ToQuery DeleteUserProfile where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteUserProfile where
        toHeaders DeleteUserProfile{..}
          = Core.pure ("X-Amz-Target", "CodeStar_20170419.DeleteUserProfile")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteUserProfile where
        toJSON DeleteUserProfile{..}
          = Core.object
              (Core.catMaybes [Core.Just ("userArn" Core..= userArn)])

instance Core.AWSRequest DeleteUserProfile where
        type Rs DeleteUserProfile = DeleteUserProfileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteUserProfileResponse' Core.<$>
                   (x Core..: "userArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteUserProfileResponse' smart constructor.
data DeleteUserProfileResponse = DeleteUserProfileResponse'
  { userArn :: Types.UserArn
    -- ^ The Amazon Resource Name (ARN) of the user deleted from AWS CodeStar.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserProfileResponse' value with any optional fields omitted.
mkDeleteUserProfileResponse
    :: Types.UserArn -- ^ 'userArn'
    -> Core.Int -- ^ 'responseStatus'
    -> DeleteUserProfileResponse
mkDeleteUserProfileResponse userArn responseStatus
  = DeleteUserProfileResponse'{userArn, responseStatus}

-- | The Amazon Resource Name (ARN) of the user deleted from AWS CodeStar.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprfrsUserArn :: Lens.Lens' DeleteUserProfileResponse Types.UserArn
duprfrsUserArn = Lens.field @"userArn"
{-# INLINEABLE duprfrsUserArn #-}
{-# DEPRECATED userArn "Use generic-lens or generic-optics with 'userArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprfrsResponseStatus :: Lens.Lens' DeleteUserProfileResponse Core.Int
duprfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE duprfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
