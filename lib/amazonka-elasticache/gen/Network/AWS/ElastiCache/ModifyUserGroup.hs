{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.ModifyUserGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the list of users that belong to the user group.
module Network.AWS.ElastiCache.ModifyUserGroup
  ( -- * Creating a request
    ModifyUserGroup (..),
    mkModifyUserGroup,

    -- ** Request lenses
    mugUserGroupId,
    mugUserIdsToAdd,
    mugUserIdsToRemove,

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

-- | /See:/ 'mkModifyUserGroup' smart constructor.
data ModifyUserGroup = ModifyUserGroup'
  { -- | The ID of the user group.
    userGroupId :: Types.String,
    -- | The list of user IDs to add to the user group.
    userIdsToAdd :: Core.Maybe (Core.NonEmpty Types.UserId),
    -- | The list of user IDs to remove from the user group.
    userIdsToRemove :: Core.Maybe (Core.NonEmpty Types.UserId)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyUserGroup' value with any optional fields omitted.
mkModifyUserGroup ::
  -- | 'userGroupId'
  Types.String ->
  ModifyUserGroup
mkModifyUserGroup userGroupId =
  ModifyUserGroup'
    { userGroupId,
      userIdsToAdd = Core.Nothing,
      userIdsToRemove = Core.Nothing
    }

-- | The ID of the user group.
--
-- /Note:/ Consider using 'userGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mugUserGroupId :: Lens.Lens' ModifyUserGroup Types.String
mugUserGroupId = Lens.field @"userGroupId"
{-# DEPRECATED mugUserGroupId "Use generic-lens or generic-optics with 'userGroupId' instead." #-}

-- | The list of user IDs to add to the user group.
--
-- /Note:/ Consider using 'userIdsToAdd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mugUserIdsToAdd :: Lens.Lens' ModifyUserGroup (Core.Maybe (Core.NonEmpty Types.UserId))
mugUserIdsToAdd = Lens.field @"userIdsToAdd"
{-# DEPRECATED mugUserIdsToAdd "Use generic-lens or generic-optics with 'userIdsToAdd' instead." #-}

-- | The list of user IDs to remove from the user group.
--
-- /Note:/ Consider using 'userIdsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mugUserIdsToRemove :: Lens.Lens' ModifyUserGroup (Core.Maybe (Core.NonEmpty Types.UserId))
mugUserIdsToRemove = Lens.field @"userIdsToRemove"
{-# DEPRECATED mugUserIdsToRemove "Use generic-lens or generic-optics with 'userIdsToRemove' instead." #-}

instance Core.AWSRequest ModifyUserGroup where
  type Rs ModifyUserGroup = Types.UserGroup
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
            ( Core.pure ("Action", "ModifyUserGroup")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> (Core.toQueryValue "UserGroupId" userGroupId)
                Core.<> ( Core.toQueryValue
                            "UserIdsToAdd"
                            (Core.toQueryList "member" Core.<$> userIdsToAdd)
                        )
                Core.<> ( Core.toQueryValue
                            "UserIdsToRemove"
                            (Core.toQueryList "member" Core.<$> userIdsToRemove)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifyUserGroupResult"
      (\s h x -> Core.parseXML x)
