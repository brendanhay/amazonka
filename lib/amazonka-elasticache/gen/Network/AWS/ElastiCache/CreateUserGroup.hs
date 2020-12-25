{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateUserGroup (..),
    mkCreateUserGroup,

    -- ** Request lenses
    cugUserGroupId,
    cugEngine,
    cugUserIds,

    -- * Destructuring the response
    Types.UserGroup (..),
    Types.mkUserGroup,

    -- ** Response lenses
    Types.ugARN,
    Types.ugEngine,
    Types.ugPendingChanges,
    Types.ugReplicationGroups,
    Types.ugStatus,
    Types.ugUserGroupId,
    Types.ugUserIds,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateUserGroup' smart constructor.
data CreateUserGroup = CreateUserGroup'
  { -- | The ID of the user group.
    userGroupId :: Types.String,
    -- | Must be Redis.
    engine :: Types.EngineType,
    -- | The list of user IDs that belong to the user group.
    userIds :: Core.Maybe (Core.NonEmpty Types.UserId)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUserGroup' value with any optional fields omitted.
mkCreateUserGroup ::
  -- | 'userGroupId'
  Types.String ->
  -- | 'engine'
  Types.EngineType ->
  CreateUserGroup
mkCreateUserGroup userGroupId engine =
  CreateUserGroup' {userGroupId, engine, userIds = Core.Nothing}

-- | The ID of the user group.
--
-- /Note:/ Consider using 'userGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cugUserGroupId :: Lens.Lens' CreateUserGroup Types.String
cugUserGroupId = Lens.field @"userGroupId"
{-# DEPRECATED cugUserGroupId "Use generic-lens or generic-optics with 'userGroupId' instead." #-}

-- | Must be Redis.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cugEngine :: Lens.Lens' CreateUserGroup Types.EngineType
cugEngine = Lens.field @"engine"
{-# DEPRECATED cugEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The list of user IDs that belong to the user group.
--
-- /Note:/ Consider using 'userIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cugUserIds :: Lens.Lens' CreateUserGroup (Core.Maybe (Core.NonEmpty Types.UserId))
cugUserIds = Lens.field @"userIds"
{-# DEPRECATED cugUserIds "Use generic-lens or generic-optics with 'userIds' instead." #-}

instance Core.AWSRequest CreateUserGroup where
  type Rs CreateUserGroup = Types.UserGroup
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
            ( Core.pure ("Action", "CreateUserGroup")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> (Core.toQueryValue "UserGroupId" userGroupId)
                Core.<> (Core.toQueryValue "Engine" engine)
                Core.<> ( Core.toQueryValue
                            "UserIds"
                            (Core.toQueryList "member" Core.<$> userIds)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateUserGroupResult"
      (\s h x -> Core.parseXML x)
