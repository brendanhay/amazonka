{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DeleteUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For Redis engine version 6.x onwards: Deletes a user. The user will be removed from all user groups and in turn removed from all replication groups. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.RBAC.html Using Role Based Access Control (RBAC)> . 
module Network.AWS.ElastiCache.DeleteUser
    (
    -- * Creating a request
      DeleteUser (..)
    , mkDeleteUser
    -- ** Request lenses
    , dUserId

     -- * Destructuring the response
    , Types.User (..)
    , Types.mkUser
    -- ** Response lenses
    , Types.uARN
    , Types.uAccessString
    , Types.uAuthentication
    , Types.uEngine
    , Types.uStatus
    , Types.uUserGroupIds
    , Types.uUserId
    , Types.uUserName
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteUser' smart constructor.
newtype DeleteUser = DeleteUser'
  { userId :: Types.UserId
    -- ^ The ID of the user.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUser' value with any optional fields omitted.
mkDeleteUser
    :: Types.UserId -- ^ 'userId'
    -> DeleteUser
mkDeleteUser userId = DeleteUser'{userId}

-- | The ID of the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dUserId :: Lens.Lens' DeleteUser Types.UserId
dUserId = Lens.field @"userId"
{-# INLINEABLE dUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

instance Core.ToQuery DeleteUser where
        toQuery DeleteUser{..}
          = Core.toQueryPair "Action" ("DeleteUser" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<> Core.toQueryPair "UserId" userId

instance Core.ToHeaders DeleteUser where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteUser where
        type Rs DeleteUser = Types.User
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
          = Response.receiveXMLWrapper "DeleteUserResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}
