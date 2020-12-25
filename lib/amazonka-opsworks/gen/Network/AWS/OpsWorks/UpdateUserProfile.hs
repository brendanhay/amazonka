{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UpdateUserProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified user profile.
--
-- __Required Permissions__ : To use this action, an IAM user must have an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.UpdateUserProfile
  ( -- * Creating a request
    UpdateUserProfile (..),
    mkUpdateUserProfile,

    -- ** Request lenses
    uupIamUserArn,
    uupAllowSelfManagement,
    uupSshPublicKey,
    uupSshUsername,

    -- * Destructuring the response
    UpdateUserProfileResponse (..),
    mkUpdateUserProfileResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateUserProfile' smart constructor.
data UpdateUserProfile = UpdateUserProfile'
  { -- | The user IAM ARN. This can also be a federated user's ARN.
    iamUserArn :: Types.String,
    -- | Whether users can specify their own SSH public key through the My Settings page. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions> .
    allowSelfManagement :: Core.Maybe Core.Bool,
    -- | The user's new SSH public key.
    sshPublicKey :: Core.Maybe Types.String,
    -- | The user's SSH user name. The allowable characters are [a-z], [A-Z], [0-9], '-', and '_'. If the specified name includes other punctuation marks, AWS OpsWorks Stacks removes them. For example, @my.name@ will be changed to @myname@ . If you do not specify an SSH user name, AWS OpsWorks Stacks generates one from the IAM user name.
    sshUsername :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserProfile' value with any optional fields omitted.
mkUpdateUserProfile ::
  -- | 'iamUserArn'
  Types.String ->
  UpdateUserProfile
mkUpdateUserProfile iamUserArn =
  UpdateUserProfile'
    { iamUserArn,
      allowSelfManagement = Core.Nothing,
      sshPublicKey = Core.Nothing,
      sshUsername = Core.Nothing
    }

-- | The user IAM ARN. This can also be a federated user's ARN.
--
-- /Note:/ Consider using 'iamUserArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupIamUserArn :: Lens.Lens' UpdateUserProfile Types.String
uupIamUserArn = Lens.field @"iamUserArn"
{-# DEPRECATED uupIamUserArn "Use generic-lens or generic-optics with 'iamUserArn' instead." #-}

-- | Whether users can specify their own SSH public key through the My Settings page. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions> .
--
-- /Note:/ Consider using 'allowSelfManagement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupAllowSelfManagement :: Lens.Lens' UpdateUserProfile (Core.Maybe Core.Bool)
uupAllowSelfManagement = Lens.field @"allowSelfManagement"
{-# DEPRECATED uupAllowSelfManagement "Use generic-lens or generic-optics with 'allowSelfManagement' instead." #-}

-- | The user's new SSH public key.
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupSshPublicKey :: Lens.Lens' UpdateUserProfile (Core.Maybe Types.String)
uupSshPublicKey = Lens.field @"sshPublicKey"
{-# DEPRECATED uupSshPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead." #-}

-- | The user's SSH user name. The allowable characters are [a-z], [A-Z], [0-9], '-', and '_'. If the specified name includes other punctuation marks, AWS OpsWorks Stacks removes them. For example, @my.name@ will be changed to @myname@ . If you do not specify an SSH user name, AWS OpsWorks Stacks generates one from the IAM user name.
--
-- /Note:/ Consider using 'sshUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupSshUsername :: Lens.Lens' UpdateUserProfile (Core.Maybe Types.String)
uupSshUsername = Lens.field @"sshUsername"
{-# DEPRECATED uupSshUsername "Use generic-lens or generic-optics with 'sshUsername' instead." #-}

instance Core.FromJSON UpdateUserProfile where
  toJSON UpdateUserProfile {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("IamUserArn" Core..= iamUserArn),
            ("AllowSelfManagement" Core..=) Core.<$> allowSelfManagement,
            ("SshPublicKey" Core..=) Core.<$> sshPublicKey,
            ("SshUsername" Core..=) Core.<$> sshUsername
          ]
      )

instance Core.AWSRequest UpdateUserProfile where
  type Rs UpdateUserProfile = UpdateUserProfileResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorks_20130218.UpdateUserProfile")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UpdateUserProfileResponse'

-- | /See:/ 'mkUpdateUserProfileResponse' smart constructor.
data UpdateUserProfileResponse = UpdateUserProfileResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserProfileResponse' value with any optional fields omitted.
mkUpdateUserProfileResponse ::
  UpdateUserProfileResponse
mkUpdateUserProfileResponse = UpdateUserProfileResponse'
