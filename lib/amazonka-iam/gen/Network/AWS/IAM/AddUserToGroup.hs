{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.AddUserToGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified user to the specified group.
module Network.AWS.IAM.AddUserToGroup
  ( -- * Creating a request
    AddUserToGroup (..),
    mkAddUserToGroup,

    -- ** Request lenses
    autgGroupName,
    autgUserName,

    -- * Destructuring the response
    AddUserToGroupResponse (..),
    mkAddUserToGroupResponse,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAddUserToGroup' smart constructor.
data AddUserToGroup = AddUserToGroup'
  { -- | The name of the group to update.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    groupName :: Types.GroupName,
    -- | The name of the user to add.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Types.UserName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddUserToGroup' value with any optional fields omitted.
mkAddUserToGroup ::
  -- | 'groupName'
  Types.GroupName ->
  -- | 'userName'
  Types.UserName ->
  AddUserToGroup
mkAddUserToGroup groupName userName =
  AddUserToGroup' {groupName, userName}

-- | The name of the group to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
autgGroupName :: Lens.Lens' AddUserToGroup Types.GroupName
autgGroupName = Lens.field @"groupName"
{-# DEPRECATED autgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The name of the user to add.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
autgUserName :: Lens.Lens' AddUserToGroup Types.UserName
autgUserName = Lens.field @"userName"
{-# DEPRECATED autgUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Core.AWSRequest AddUserToGroup where
  type Rs AddUserToGroup = AddUserToGroupResponse
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
            ( Core.pure ("Action", "AddUserToGroup")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "GroupName" groupName)
                Core.<> (Core.toQueryValue "UserName" userName)
            )
      }
  response = Response.receiveNull AddUserToGroupResponse'

-- | /See:/ 'mkAddUserToGroupResponse' smart constructor.
data AddUserToGroupResponse = AddUserToGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddUserToGroupResponse' value with any optional fields omitted.
mkAddUserToGroupResponse ::
  AddUserToGroupResponse
mkAddUserToGroupResponse = AddUserToGroupResponse'
