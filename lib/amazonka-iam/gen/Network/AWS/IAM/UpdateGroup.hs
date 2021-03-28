{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name and/or the path of the specified IAM group.
--
-- /Important:/ You should understand the implications of changing a group's path or name. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_WorkingWithGroupsAndUsers.html Renaming Users and Groups> in the /IAM User Guide/ .
module Network.AWS.IAM.UpdateGroup
    (
    -- * Creating a request
      UpdateGroup (..)
    , mkUpdateGroup
    -- ** Request lenses
    , ugGroupName
    , ugNewGroupName
    , ugNewPath

    -- * Destructuring the response
    , UpdateGroupResponse (..)
    , mkUpdateGroupResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateGroup' smart constructor.
data UpdateGroup = UpdateGroup'
  { groupName :: Types.GroupNameType
    -- ^ Name of the IAM group to update. If you're changing the name of the group, this is the original name.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  , newGroupName :: Core.Maybe Types.GroupNameType
    -- ^ New name for the IAM group. Only include this if changing the group's name.
--
-- IAM user, group, role, and policy names must be unique within the account. Names are not distinguished by case. For example, you cannot create resources named both "MyResource" and "myresource".
  , newPath :: Core.Maybe Types.NewPath
    -- ^ New path for the IAM group. Only include this if changing the group's path.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGroup' value with any optional fields omitted.
mkUpdateGroup
    :: Types.GroupNameType -- ^ 'groupName'
    -> UpdateGroup
mkUpdateGroup groupName
  = UpdateGroup'{groupName, newGroupName = Core.Nothing,
                 newPath = Core.Nothing}

-- | Name of the IAM group to update. If you're changing the name of the group, this is the original name.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugGroupName :: Lens.Lens' UpdateGroup Types.GroupNameType
ugGroupName = Lens.field @"groupName"
{-# INLINEABLE ugGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | New name for the IAM group. Only include this if changing the group's name.
--
-- IAM user, group, role, and policy names must be unique within the account. Names are not distinguished by case. For example, you cannot create resources named both "MyResource" and "myresource".
--
-- /Note:/ Consider using 'newGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugNewGroupName :: Lens.Lens' UpdateGroup (Core.Maybe Types.GroupNameType)
ugNewGroupName = Lens.field @"newGroupName"
{-# INLINEABLE ugNewGroupName #-}
{-# DEPRECATED newGroupName "Use generic-lens or generic-optics with 'newGroupName' instead"  #-}

-- | New path for the IAM group. Only include this if changing the group's path.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'newPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugNewPath :: Lens.Lens' UpdateGroup (Core.Maybe Types.NewPath)
ugNewPath = Lens.field @"newPath"
{-# INLINEABLE ugNewPath #-}
{-# DEPRECATED newPath "Use generic-lens or generic-optics with 'newPath' instead"  #-}

instance Core.ToQuery UpdateGroup where
        toQuery UpdateGroup{..}
          = Core.toQueryPair "Action" ("UpdateGroup" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "GroupName" groupName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NewGroupName")
                newGroupName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "NewPath") newPath

instance Core.ToHeaders UpdateGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UpdateGroup where
        type Rs UpdateGroup = UpdateGroupResponse
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
        parseResponse = Response.receiveNull UpdateGroupResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateGroupResponse' smart constructor.
data UpdateGroupResponse = UpdateGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGroupResponse' value with any optional fields omitted.
mkUpdateGroupResponse
    :: UpdateGroupResponse
mkUpdateGroupResponse = UpdateGroupResponse'
