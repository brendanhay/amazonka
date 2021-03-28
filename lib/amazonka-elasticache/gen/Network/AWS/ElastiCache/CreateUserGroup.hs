{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.CreateUserGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For Redis engine version 6.x onwards: Creates a Redis user group. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.RBAC.html Using Role Based Access Control (RBAC)> 
module Network.AWS.ElastiCache.CreateUserGroup
    (
    -- * Creating a request
      CreateUserGroup (..)
    , mkCreateUserGroup
    -- ** Request lenses
    , cugUserGroupId
    , cugEngine
    , cugUserIds

     -- * Destructuring the response
    , Types.UserGroup (..)
    , Types.mkUserGroup
    -- ** Response lenses
    , Types.ugARN
    , Types.ugEngine
    , Types.ugPendingChanges
    , Types.ugReplicationGroups
    , Types.ugStatus
    , Types.ugUserGroupId
    , Types.ugUserIds
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateUserGroup' smart constructor.
data CreateUserGroup = CreateUserGroup'
  { userGroupId :: Core.Text
    -- ^ The ID of the user group.
  , engine :: Types.EngineType
    -- ^ Must be Redis. 
  , userIds :: Core.Maybe (Core.NonEmpty Types.UserId)
    -- ^ The list of user IDs that belong to the user group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUserGroup' value with any optional fields omitted.
mkCreateUserGroup
    :: Core.Text -- ^ 'userGroupId'
    -> Types.EngineType -- ^ 'engine'
    -> CreateUserGroup
mkCreateUserGroup userGroupId engine
  = CreateUserGroup'{userGroupId, engine, userIds = Core.Nothing}

-- | The ID of the user group.
--
-- /Note:/ Consider using 'userGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cugUserGroupId :: Lens.Lens' CreateUserGroup Core.Text
cugUserGroupId = Lens.field @"userGroupId"
{-# INLINEABLE cugUserGroupId #-}
{-# DEPRECATED userGroupId "Use generic-lens or generic-optics with 'userGroupId' instead"  #-}

-- | Must be Redis. 
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cugEngine :: Lens.Lens' CreateUserGroup Types.EngineType
cugEngine = Lens.field @"engine"
{-# INLINEABLE cugEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | The list of user IDs that belong to the user group.
--
-- /Note:/ Consider using 'userIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cugUserIds :: Lens.Lens' CreateUserGroup (Core.Maybe (Core.NonEmpty Types.UserId))
cugUserIds = Lens.field @"userIds"
{-# INLINEABLE cugUserIds #-}
{-# DEPRECATED userIds "Use generic-lens or generic-optics with 'userIds' instead"  #-}

instance Core.ToQuery CreateUserGroup where
        toQuery CreateUserGroup{..}
          = Core.toQueryPair "Action" ("CreateUserGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<> Core.toQueryPair "UserGroupId" userGroupId
              Core.<> Core.toQueryPair "Engine" engine
              Core.<>
              Core.toQueryPair "UserIds"
                (Core.maybe Core.mempty (Core.toQueryList "member") userIds)

instance Core.ToHeaders CreateUserGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateUserGroup where
        type Rs CreateUserGroup = Types.UserGroup
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
          = Response.receiveXMLWrapper "CreateUserGroupResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}
