{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DeleteUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user account from the specified Amazon Connect instance.
--
-- For information about what happens to a user's data when their account is deleted, see <https://docs.aws.amazon.com/connect/latest/adminguide/delete-users.html Delete Users from Your Amazon Connect Instance> in the /Amazon Connect Administrator Guide/ .
module Network.AWS.Connect.DeleteUser
    (
    -- * Creating a request
      DeleteUser (..)
    , mkDeleteUser
    -- ** Request lenses
    , dufInstanceId
    , dufUserId

    -- * Destructuring the response
    , DeleteUserResponse (..)
    , mkDeleteUserResponse
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , userId :: Types.UserId
    -- ^ The identifier of the user.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUser' value with any optional fields omitted.
mkDeleteUser
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.UserId -- ^ 'userId'
    -> DeleteUser
mkDeleteUser instanceId userId = DeleteUser'{instanceId, userId}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dufInstanceId :: Lens.Lens' DeleteUser Types.InstanceId
dufInstanceId = Lens.field @"instanceId"
{-# INLINEABLE dufInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The identifier of the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dufUserId :: Lens.Lens' DeleteUser Types.UserId
dufUserId = Lens.field @"userId"
{-# INLINEABLE dufUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

instance Core.ToQuery DeleteUser where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteUser where
        toHeaders DeleteUser{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteUser where
        type Rs DeleteUser = DeleteUserResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/users/" Core.<> Core.toText instanceId Core.<> "/" Core.<>
                             Core.toText userId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteUserResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteUserResponse' smart constructor.
data DeleteUserResponse = DeleteUserResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserResponse' value with any optional fields omitted.
mkDeleteUserResponse
    :: DeleteUserResponse
mkDeleteUserResponse = DeleteUserResponse'
