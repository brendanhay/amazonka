{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UntagRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the role. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
module Network.AWS.IAM.UntagRole
    (
    -- * Creating a request
      UntagRole (..)
    , mkUntagRole
    -- ** Request lenses
    , uRoleName
    , uTagKeys

    -- * Destructuring the response
    , UntagRoleResponse (..)
    , mkUntagRoleResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUntagRole' smart constructor.
data UntagRole = UntagRole'
  { roleName :: Types.RoleName
    -- ^ The name of the IAM role from which you want to remove tags.
--
-- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  , tagKeys :: [Types.TagKeyType]
    -- ^ A list of key names as a simple array of strings. The tags with matching keys are removed from the specified role.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagRole' value with any optional fields omitted.
mkUntagRole
    :: Types.RoleName -- ^ 'roleName'
    -> UntagRole
mkUntagRole roleName = UntagRole'{roleName, tagKeys = Core.mempty}

-- | The name of the IAM role from which you want to remove tags.
--
-- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uRoleName :: Lens.Lens' UntagRole Types.RoleName
uRoleName = Lens.field @"roleName"
{-# INLINEABLE uRoleName #-}
{-# DEPRECATED roleName "Use generic-lens or generic-optics with 'roleName' instead"  #-}

-- | A list of key names as a simple array of strings. The tags with matching keys are removed from the specified role.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uTagKeys :: Lens.Lens' UntagRole [Types.TagKeyType]
uTagKeys = Lens.field @"tagKeys"
{-# INLINEABLE uTagKeys #-}
{-# DEPRECATED tagKeys "Use generic-lens or generic-optics with 'tagKeys' instead"  #-}

instance Core.ToQuery UntagRole where
        toQuery UntagRole{..}
          = Core.toQueryPair "Action" ("UntagRole" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "RoleName" roleName
              Core.<>
              Core.toQueryPair "TagKeys" (Core.toQueryList "member" tagKeys)

instance Core.ToHeaders UntagRole where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UntagRole where
        type Rs UntagRole = UntagRoleResponse
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
        parseResponse = Response.receiveNull UntagRoleResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUntagRoleResponse' smart constructor.
data UntagRoleResponse = UntagRoleResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagRoleResponse' value with any optional fields omitted.
mkUntagRoleResponse
    :: UntagRoleResponse
mkUntagRoleResponse = UntagRoleResponse'
