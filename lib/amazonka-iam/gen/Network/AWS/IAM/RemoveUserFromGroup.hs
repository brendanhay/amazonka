{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.RemoveUserFromGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified user from the specified group.
module Network.AWS.IAM.RemoveUserFromGroup
  ( -- * Creating a request
    RemoveUserFromGroup (..),
    mkRemoveUserFromGroup,

    -- ** Request lenses
    rufgGroupName,
    rufgUserName,

    -- * Destructuring the response
    RemoveUserFromGroupResponse (..),
    mkRemoveUserFromGroupResponse,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveUserFromGroup' smart constructor.
data RemoveUserFromGroup = RemoveUserFromGroup'
  { -- | The name of the group to update.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    groupName :: Types.GroupName,
    -- | The name of the user to remove.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Types.UserName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveUserFromGroup' value with any optional fields omitted.
mkRemoveUserFromGroup ::
  -- | 'groupName'
  Types.GroupName ->
  -- | 'userName'
  Types.UserName ->
  RemoveUserFromGroup
mkRemoveUserFromGroup groupName userName =
  RemoveUserFromGroup' {groupName, userName}

-- | The name of the group to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rufgGroupName :: Lens.Lens' RemoveUserFromGroup Types.GroupName
rufgGroupName = Lens.field @"groupName"
{-# DEPRECATED rufgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The name of the user to remove.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rufgUserName :: Lens.Lens' RemoveUserFromGroup Types.UserName
rufgUserName = Lens.field @"userName"
{-# DEPRECATED rufgUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Core.AWSRequest RemoveUserFromGroup where
  type Rs RemoveUserFromGroup = RemoveUserFromGroupResponse
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
            ( Core.pure ("Action", "RemoveUserFromGroup")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "GroupName" groupName)
                Core.<> (Core.toQueryValue "UserName" userName)
            )
      }
  response = Response.receiveNull RemoveUserFromGroupResponse'

-- | /See:/ 'mkRemoveUserFromGroupResponse' smart constructor.
data RemoveUserFromGroupResponse = RemoveUserFromGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveUserFromGroupResponse' value with any optional fields omitted.
mkRemoveUserFromGroupResponse ::
  RemoveUserFromGroupResponse
mkRemoveUserFromGroupResponse = RemoveUserFromGroupResponse'
