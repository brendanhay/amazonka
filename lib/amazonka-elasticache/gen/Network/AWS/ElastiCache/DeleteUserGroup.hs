{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DeleteUserGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For Redis engine version 6.x onwards: Deletes a ser group. The user group must first be disassociated from the replcation group before it can be deleted. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.RBAC.html Using Role Based Access Control (RBAC)> .
module Network.AWS.ElastiCache.DeleteUserGroup
  ( -- * Creating a request
    DeleteUserGroup (..),
    mkDeleteUserGroup,

    -- ** Request lenses
    dugUserGroupId,

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

-- | /See:/ 'mkDeleteUserGroup' smart constructor.
newtype DeleteUserGroup = DeleteUserGroup'
  { -- | The ID of the user group.
    userGroupId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserGroup' value with any optional fields omitted.
mkDeleteUserGroup ::
  -- | 'userGroupId'
  Types.String ->
  DeleteUserGroup
mkDeleteUserGroup userGroupId = DeleteUserGroup' {userGroupId}

-- | The ID of the user group.
--
-- /Note:/ Consider using 'userGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dugUserGroupId :: Lens.Lens' DeleteUserGroup Types.String
dugUserGroupId = Lens.field @"userGroupId"
{-# DEPRECATED dugUserGroupId "Use generic-lens or generic-optics with 'userGroupId' instead." #-}

instance Core.AWSRequest DeleteUserGroup where
  type Rs DeleteUserGroup = Types.UserGroup
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
            ( Core.pure ("Action", "DeleteUserGroup")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> (Core.toQueryValue "UserGroupId" userGroupId)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteUserGroupResult"
      (\s h x -> Core.parseXML x)
