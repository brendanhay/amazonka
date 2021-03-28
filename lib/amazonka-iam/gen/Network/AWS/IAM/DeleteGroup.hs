{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified IAM group. The group must not contain any users or have any attached policies.
module Network.AWS.IAM.DeleteGroup
    (
    -- * Creating a request
      DeleteGroup (..)
    , mkDeleteGroup
    -- ** Request lenses
    , dgGroupName

    -- * Destructuring the response
    , DeleteGroupResponse (..)
    , mkDeleteGroupResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteGroup' smart constructor.
newtype DeleteGroup = DeleteGroup'
  { groupName :: Types.GroupNameType
    -- ^ The name of the IAM group to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGroup' value with any optional fields omitted.
mkDeleteGroup
    :: Types.GroupNameType -- ^ 'groupName'
    -> DeleteGroup
mkDeleteGroup groupName = DeleteGroup'{groupName}

-- | The name of the IAM group to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgGroupName :: Lens.Lens' DeleteGroup Types.GroupNameType
dgGroupName = Lens.field @"groupName"
{-# INLINEABLE dgGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

instance Core.ToQuery DeleteGroup where
        toQuery DeleteGroup{..}
          = Core.toQueryPair "Action" ("DeleteGroup" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "GroupName" groupName

instance Core.ToHeaders DeleteGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteGroup where
        type Rs DeleteGroup = DeleteGroupResponse
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
        parseResponse = Response.receiveNull DeleteGroupResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteGroupResponse' smart constructor.
data DeleteGroupResponse = DeleteGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGroupResponse' value with any optional fields omitted.
mkDeleteGroupResponse
    :: DeleteGroupResponse
mkDeleteGroupResponse = DeleteGroupResponse'
