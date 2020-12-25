{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.AddRoleToInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified IAM role to the specified instance profile. An instance profile can contain only one role. (The number and size of IAM resources in an AWS account are limited. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html IAM and STS Quotas> in the /IAM User Guide/ .) You can remove the existing role and then add a different role to an instance profile. You must then wait for the change to appear across all of AWS because of <https://en.wikipedia.org/wiki/Eventual_consistency eventual consistency> . To force the change, you must <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DisassociateIamInstanceProfile.html disassociate the instance profile> and then <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_AssociateIamInstanceProfile.html associate the instance profile> , or you can stop your instance and then restart it.
--
-- For more information about roles, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Working with Roles> . For more information about instance profiles, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles> .
module Network.AWS.IAM.AddRoleToInstanceProfile
  ( -- * Creating a request
    AddRoleToInstanceProfile (..),
    mkAddRoleToInstanceProfile,

    -- ** Request lenses
    artipInstanceProfileName,
    artipRoleName,

    -- * Destructuring the response
    AddRoleToInstanceProfileResponse (..),
    mkAddRoleToInstanceProfileResponse,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAddRoleToInstanceProfile' smart constructor.
data AddRoleToInstanceProfile = AddRoleToInstanceProfile'
  { -- | The name of the instance profile to update.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    instanceProfileName :: Types.InstanceProfileNameType,
    -- | The name of the role to add.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    roleName :: Types.RoleName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddRoleToInstanceProfile' value with any optional fields omitted.
mkAddRoleToInstanceProfile ::
  -- | 'instanceProfileName'
  Types.InstanceProfileNameType ->
  -- | 'roleName'
  Types.RoleName ->
  AddRoleToInstanceProfile
mkAddRoleToInstanceProfile instanceProfileName roleName =
  AddRoleToInstanceProfile' {instanceProfileName, roleName}

-- | The name of the instance profile to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'instanceProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artipInstanceProfileName :: Lens.Lens' AddRoleToInstanceProfile Types.InstanceProfileNameType
artipInstanceProfileName = Lens.field @"instanceProfileName"
{-# DEPRECATED artipInstanceProfileName "Use generic-lens or generic-optics with 'instanceProfileName' instead." #-}

-- | The name of the role to add.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artipRoleName :: Lens.Lens' AddRoleToInstanceProfile Types.RoleName
artipRoleName = Lens.field @"roleName"
{-# DEPRECATED artipRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

instance Core.AWSRequest AddRoleToInstanceProfile where
  type Rs AddRoleToInstanceProfile = AddRoleToInstanceProfileResponse
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
            ( Core.pure ("Action", "AddRoleToInstanceProfile")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "InstanceProfileName" instanceProfileName)
                Core.<> (Core.toQueryValue "RoleName" roleName)
            )
      }
  response = Response.receiveNull AddRoleToInstanceProfileResponse'

-- | /See:/ 'mkAddRoleToInstanceProfileResponse' smart constructor.
data AddRoleToInstanceProfileResponse = AddRoleToInstanceProfileResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddRoleToInstanceProfileResponse' value with any optional fields omitted.
mkAddRoleToInstanceProfileResponse ::
  AddRoleToInstanceProfileResponse
mkAddRoleToInstanceProfileResponse =
  AddRoleToInstanceProfileResponse'
