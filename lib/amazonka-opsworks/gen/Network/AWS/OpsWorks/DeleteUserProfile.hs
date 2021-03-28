{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DeleteUserProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user profile.
--
-- __Required Permissions__ : To use this action, an IAM user must have an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DeleteUserProfile
    (
    -- * Creating a request
      DeleteUserProfile (..)
    , mkDeleteUserProfile
    -- ** Request lenses
    , dupIamUserArn

    -- * Destructuring the response
    , DeleteUserProfileResponse (..)
    , mkDeleteUserProfileResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteUserProfile' smart constructor.
newtype DeleteUserProfile = DeleteUserProfile'
  { iamUserArn :: Core.Text
    -- ^ The user's IAM ARN. This can also be a federated user's ARN.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserProfile' value with any optional fields omitted.
mkDeleteUserProfile
    :: Core.Text -- ^ 'iamUserArn'
    -> DeleteUserProfile
mkDeleteUserProfile iamUserArn = DeleteUserProfile'{iamUserArn}

-- | The user's IAM ARN. This can also be a federated user's ARN.
--
-- /Note:/ Consider using 'iamUserArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupIamUserArn :: Lens.Lens' DeleteUserProfile Core.Text
dupIamUserArn = Lens.field @"iamUserArn"
{-# INLINEABLE dupIamUserArn #-}
{-# DEPRECATED iamUserArn "Use generic-lens or generic-optics with 'iamUserArn' instead"  #-}

instance Core.ToQuery DeleteUserProfile where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteUserProfile where
        toHeaders DeleteUserProfile{..}
          = Core.pure ("X-Amz-Target", "OpsWorks_20130218.DeleteUserProfile")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteUserProfile where
        toJSON DeleteUserProfile{..}
          = Core.object
              (Core.catMaybes [Core.Just ("IamUserArn" Core..= iamUserArn)])

instance Core.AWSRequest DeleteUserProfile where
        type Rs DeleteUserProfile = DeleteUserProfileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteUserProfileResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteUserProfileResponse' smart constructor.
data DeleteUserProfileResponse = DeleteUserProfileResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserProfileResponse' value with any optional fields omitted.
mkDeleteUserProfileResponse
    :: DeleteUserProfileResponse
mkDeleteUserProfileResponse = DeleteUserProfileResponse'
