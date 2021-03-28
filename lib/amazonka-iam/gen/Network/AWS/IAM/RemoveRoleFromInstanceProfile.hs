{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.RemoveRoleFromInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified IAM role from the specified EC2 instance profile.
--
-- /Important:/ Make sure that you do not have any Amazon EC2 instances running with the role you are about to remove from the instance profile. Removing a role from an instance profile that is associated with a running instance might break any applications running on the instance.
-- For more information about IAM roles, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Working with Roles> . For more information about instance profiles, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles> .
module Network.AWS.IAM.RemoveRoleFromInstanceProfile
    (
    -- * Creating a request
      RemoveRoleFromInstanceProfile (..)
    , mkRemoveRoleFromInstanceProfile
    -- ** Request lenses
    , rrfipInstanceProfileName
    , rrfipRoleName

    -- * Destructuring the response
    , RemoveRoleFromInstanceProfileResponse (..)
    , mkRemoveRoleFromInstanceProfileResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveRoleFromInstanceProfile' smart constructor.
data RemoveRoleFromInstanceProfile = RemoveRoleFromInstanceProfile'
  { instanceProfileName :: Types.InstanceProfileName
    -- ^ The name of the instance profile to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  , roleName :: Types.RoleName
    -- ^ The name of the role to remove.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveRoleFromInstanceProfile' value with any optional fields omitted.
mkRemoveRoleFromInstanceProfile
    :: Types.InstanceProfileName -- ^ 'instanceProfileName'
    -> Types.RoleName -- ^ 'roleName'
    -> RemoveRoleFromInstanceProfile
mkRemoveRoleFromInstanceProfile instanceProfileName roleName
  = RemoveRoleFromInstanceProfile'{instanceProfileName, roleName}

-- | The name of the instance profile to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'instanceProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrfipInstanceProfileName :: Lens.Lens' RemoveRoleFromInstanceProfile Types.InstanceProfileName
rrfipInstanceProfileName = Lens.field @"instanceProfileName"
{-# INLINEABLE rrfipInstanceProfileName #-}
{-# DEPRECATED instanceProfileName "Use generic-lens or generic-optics with 'instanceProfileName' instead"  #-}

-- | The name of the role to remove.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrfipRoleName :: Lens.Lens' RemoveRoleFromInstanceProfile Types.RoleName
rrfipRoleName = Lens.field @"roleName"
{-# INLINEABLE rrfipRoleName #-}
{-# DEPRECATED roleName "Use generic-lens or generic-optics with 'roleName' instead"  #-}

instance Core.ToQuery RemoveRoleFromInstanceProfile where
        toQuery RemoveRoleFromInstanceProfile{..}
          = Core.toQueryPair "Action"
              ("RemoveRoleFromInstanceProfile" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "InstanceProfileName" instanceProfileName
              Core.<> Core.toQueryPair "RoleName" roleName

instance Core.ToHeaders RemoveRoleFromInstanceProfile where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RemoveRoleFromInstanceProfile where
        type Rs RemoveRoleFromInstanceProfile =
             RemoveRoleFromInstanceProfileResponse
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
          = Response.receiveNull RemoveRoleFromInstanceProfileResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRemoveRoleFromInstanceProfileResponse' smart constructor.
data RemoveRoleFromInstanceProfileResponse = RemoveRoleFromInstanceProfileResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveRoleFromInstanceProfileResponse' value with any optional fields omitted.
mkRemoveRoleFromInstanceProfileResponse
    :: RemoveRoleFromInstanceProfileResponse
mkRemoveRoleFromInstanceProfileResponse
  = RemoveRoleFromInstanceProfileResponse'
