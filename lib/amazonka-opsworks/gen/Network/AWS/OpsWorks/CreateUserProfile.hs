{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.CreateUserProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new user profile.
--
-- __Required Permissions__ : To use this action, an IAM user must have an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.CreateUserProfile
  ( -- * Creating a request
    CreateUserProfile (..),
    mkCreateUserProfile,

    -- ** Request lenses
    cupIamUserArn,
    cupAllowSelfManagement,
    cupSshPublicKey,
    cupSshUsername,

    -- * Destructuring the response
    CreateUserProfileResponse (..),
    mkCreateUserProfileResponse,

    -- ** Response lenses
    cuprrsIamUserArn,
    cuprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateUserProfile' smart constructor.
data CreateUserProfile = CreateUserProfile'
  { -- | The user's IAM ARN; this can also be a federated user's ARN.
    iamUserArn :: Types.String,
    -- | Whether users can specify their own SSH public key through the My Settings page. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Setting an IAM User's Public SSH Key> .
    allowSelfManagement :: Core.Maybe Core.Bool,
    -- | The user's public SSH key.
    sshPublicKey :: Core.Maybe Types.String,
    -- | The user's SSH user name. The allowable characters are [a-z], [A-Z], [0-9], '-', and '_'. If the specified name includes other punctuation marks, AWS OpsWorks Stacks removes them. For example, @my.name@ will be changed to @myname@ . If you do not specify an SSH user name, AWS OpsWorks Stacks generates one from the IAM user name.
    sshUsername :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUserProfile' value with any optional fields omitted.
mkCreateUserProfile ::
  -- | 'iamUserArn'
  Types.String ->
  CreateUserProfile
mkCreateUserProfile iamUserArn =
  CreateUserProfile'
    { iamUserArn,
      allowSelfManagement = Core.Nothing,
      sshPublicKey = Core.Nothing,
      sshUsername = Core.Nothing
    }

-- | The user's IAM ARN; this can also be a federated user's ARN.
--
-- /Note:/ Consider using 'iamUserArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupIamUserArn :: Lens.Lens' CreateUserProfile Types.String
cupIamUserArn = Lens.field @"iamUserArn"
{-# DEPRECATED cupIamUserArn "Use generic-lens or generic-optics with 'iamUserArn' instead." #-}

-- | Whether users can specify their own SSH public key through the My Settings page. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Setting an IAM User's Public SSH Key> .
--
-- /Note:/ Consider using 'allowSelfManagement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupAllowSelfManagement :: Lens.Lens' CreateUserProfile (Core.Maybe Core.Bool)
cupAllowSelfManagement = Lens.field @"allowSelfManagement"
{-# DEPRECATED cupAllowSelfManagement "Use generic-lens or generic-optics with 'allowSelfManagement' instead." #-}

-- | The user's public SSH key.
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupSshPublicKey :: Lens.Lens' CreateUserProfile (Core.Maybe Types.String)
cupSshPublicKey = Lens.field @"sshPublicKey"
{-# DEPRECATED cupSshPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead." #-}

-- | The user's SSH user name. The allowable characters are [a-z], [A-Z], [0-9], '-', and '_'. If the specified name includes other punctuation marks, AWS OpsWorks Stacks removes them. For example, @my.name@ will be changed to @myname@ . If you do not specify an SSH user name, AWS OpsWorks Stacks generates one from the IAM user name.
--
-- /Note:/ Consider using 'sshUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupSshUsername :: Lens.Lens' CreateUserProfile (Core.Maybe Types.String)
cupSshUsername = Lens.field @"sshUsername"
{-# DEPRECATED cupSshUsername "Use generic-lens or generic-optics with 'sshUsername' instead." #-}

instance Core.FromJSON CreateUserProfile where
  toJSON CreateUserProfile {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("IamUserArn" Core..= iamUserArn),
            ("AllowSelfManagement" Core..=) Core.<$> allowSelfManagement,
            ("SshPublicKey" Core..=) Core.<$> sshPublicKey,
            ("SshUsername" Core..=) Core.<$> sshUsername
          ]
      )

instance Core.AWSRequest CreateUserProfile where
  type Rs CreateUserProfile = CreateUserProfileResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorks_20130218.CreateUserProfile")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserProfileResponse'
            Core.<$> (x Core..:? "IamUserArn") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a @CreateUserProfile@ request.
--
-- /See:/ 'mkCreateUserProfileResponse' smart constructor.
data CreateUserProfileResponse = CreateUserProfileResponse'
  { -- | The user's IAM ARN.
    iamUserArn :: Core.Maybe Types.IamUserArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUserProfileResponse' value with any optional fields omitted.
mkCreateUserProfileResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateUserProfileResponse
mkCreateUserProfileResponse responseStatus =
  CreateUserProfileResponse'
    { iamUserArn = Core.Nothing,
      responseStatus
    }

-- | The user's IAM ARN.
--
-- /Note:/ Consider using 'iamUserArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuprrsIamUserArn :: Lens.Lens' CreateUserProfileResponse (Core.Maybe Types.IamUserArn)
cuprrsIamUserArn = Lens.field @"iamUserArn"
{-# DEPRECATED cuprrsIamUserArn "Use generic-lens or generic-optics with 'iamUserArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuprrsResponseStatus :: Lens.Lens' CreateUserProfileResponse Core.Int
cuprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cuprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
