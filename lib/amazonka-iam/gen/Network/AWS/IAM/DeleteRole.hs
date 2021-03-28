{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified role. The role must not have any policies attached. For more information about roles, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Working with Roles> .
--
-- /Important:/ Make sure that you do not have any Amazon EC2 instances running with the role you are about to delete. Deleting a role or instance profile that is associated with a running instance will break any applications running on the instance.
module Network.AWS.IAM.DeleteRole
    (
    -- * Creating a request
      DeleteRole (..)
    , mkDeleteRole
    -- ** Request lenses
    , drRoleName

    -- * Destructuring the response
    , DeleteRoleResponse (..)
    , mkDeleteRoleResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteRole' smart constructor.
newtype DeleteRole = DeleteRole'
  { roleName :: Types.RoleName
    -- ^ The name of the role to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRole' value with any optional fields omitted.
mkDeleteRole
    :: Types.RoleName -- ^ 'roleName'
    -> DeleteRole
mkDeleteRole roleName = DeleteRole'{roleName}

-- | The name of the role to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drRoleName :: Lens.Lens' DeleteRole Types.RoleName
drRoleName = Lens.field @"roleName"
{-# INLINEABLE drRoleName #-}
{-# DEPRECATED roleName "Use generic-lens or generic-optics with 'roleName' instead"  #-}

instance Core.ToQuery DeleteRole where
        toQuery DeleteRole{..}
          = Core.toQueryPair "Action" ("DeleteRole" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "RoleName" roleName

instance Core.ToHeaders DeleteRole where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteRole where
        type Rs DeleteRole = DeleteRoleResponse
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
        parseResponse = Response.receiveNull DeleteRoleResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteRoleResponse' smart constructor.
data DeleteRoleResponse = DeleteRoleResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRoleResponse' value with any optional fields omitted.
mkDeleteRoleResponse
    :: DeleteRoleResponse
mkDeleteRoleResponse = DeleteRoleResponse'
