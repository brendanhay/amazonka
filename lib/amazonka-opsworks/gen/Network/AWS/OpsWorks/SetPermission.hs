{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.SetPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies a user's permissions. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workingsecurity.html Security and Permissions> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.SetPermission
  ( -- * Creating a request
    SetPermission (..),
    mkSetPermission,

    -- ** Request lenses
    spIAMUserARN,
    spAllowSudo,
    spStackId,
    spLevel,
    spAllowSSH,

    -- * Destructuring the response
    SetPermissionResponse (..),
    mkSetPermissionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSetPermission' smart constructor.
data SetPermission = SetPermission'
  { -- | The user's IAM ARN. This can also be a federated user's ARN.
    iamUserARN :: Lude.Text,
    -- | The user is allowed to use __sudo__ to elevate privileges.
    allowSudo :: Lude.Maybe Lude.Bool,
    -- | The stack ID.
    stackId :: Lude.Text,
    -- | The user's permission level, which must be set to one of the following strings. You cannot set your own permissions level.
    --
    --
    --     * @deny@
    --
    --
    --     * @show@
    --
    --
    --     * @deploy@
    --
    --
    --     * @manage@
    --
    --
    --     * @iam_only@
    --
    --
    -- For more information about the permissions associated with these levels, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
    level :: Lude.Maybe Lude.Text,
    -- | The user is allowed to use SSH to communicate with the instance.
    allowSSH :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetPermission' with the minimum fields required to make a request.
--
-- * 'iamUserARN' - The user's IAM ARN. This can also be a federated user's ARN.
-- * 'allowSudo' - The user is allowed to use __sudo__ to elevate privileges.
-- * 'stackId' - The stack ID.
-- * 'level' - The user's permission level, which must be set to one of the following strings. You cannot set your own permissions level.
--
--
--     * @deny@
--
--
--     * @show@
--
--
--     * @deploy@
--
--
--     * @manage@
--
--
--     * @iam_only@
--
--
-- For more information about the permissions associated with these levels, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
-- * 'allowSSH' - The user is allowed to use SSH to communicate with the instance.
mkSetPermission ::
  -- | 'iamUserARN'
  Lude.Text ->
  -- | 'stackId'
  Lude.Text ->
  SetPermission
mkSetPermission pIAMUserARN_ pStackId_ =
  SetPermission'
    { iamUserARN = pIAMUserARN_,
      allowSudo = Lude.Nothing,
      stackId = pStackId_,
      level = Lude.Nothing,
      allowSSH = Lude.Nothing
    }

-- | The user's IAM ARN. This can also be a federated user's ARN.
--
-- /Note:/ Consider using 'iamUserARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spIAMUserARN :: Lens.Lens' SetPermission Lude.Text
spIAMUserARN = Lens.lens (iamUserARN :: SetPermission -> Lude.Text) (\s a -> s {iamUserARN = a} :: SetPermission)
{-# DEPRECATED spIAMUserARN "Use generic-lens or generic-optics with 'iamUserARN' instead." #-}

-- | The user is allowed to use __sudo__ to elevate privileges.
--
-- /Note:/ Consider using 'allowSudo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spAllowSudo :: Lens.Lens' SetPermission (Lude.Maybe Lude.Bool)
spAllowSudo = Lens.lens (allowSudo :: SetPermission -> Lude.Maybe Lude.Bool) (\s a -> s {allowSudo = a} :: SetPermission)
{-# DEPRECATED spAllowSudo "Use generic-lens or generic-optics with 'allowSudo' instead." #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spStackId :: Lens.Lens' SetPermission Lude.Text
spStackId = Lens.lens (stackId :: SetPermission -> Lude.Text) (\s a -> s {stackId = a} :: SetPermission)
{-# DEPRECATED spStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The user's permission level, which must be set to one of the following strings. You cannot set your own permissions level.
--
--
--     * @deny@
--
--
--     * @show@
--
--
--     * @deploy@
--
--
--     * @manage@
--
--
--     * @iam_only@
--
--
-- For more information about the permissions associated with these levels, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
-- /Note:/ Consider using 'level' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spLevel :: Lens.Lens' SetPermission (Lude.Maybe Lude.Text)
spLevel = Lens.lens (level :: SetPermission -> Lude.Maybe Lude.Text) (\s a -> s {level = a} :: SetPermission)
{-# DEPRECATED spLevel "Use generic-lens or generic-optics with 'level' instead." #-}

-- | The user is allowed to use SSH to communicate with the instance.
--
-- /Note:/ Consider using 'allowSSH' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spAllowSSH :: Lens.Lens' SetPermission (Lude.Maybe Lude.Bool)
spAllowSSH = Lens.lens (allowSSH :: SetPermission -> Lude.Maybe Lude.Bool) (\s a -> s {allowSSH = a} :: SetPermission)
{-# DEPRECATED spAllowSSH "Use generic-lens or generic-optics with 'allowSSH' instead." #-}

instance Lude.AWSRequest SetPermission where
  type Rs SetPermission = SetPermissionResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull SetPermissionResponse'

instance Lude.ToHeaders SetPermission where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.SetPermission" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SetPermission where
  toJSON SetPermission' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("IamUserArn" Lude..= iamUserARN),
            ("AllowSudo" Lude..=) Lude.<$> allowSudo,
            Lude.Just ("StackId" Lude..= stackId),
            ("Level" Lude..=) Lude.<$> level,
            ("AllowSsh" Lude..=) Lude.<$> allowSSH
          ]
      )

instance Lude.ToPath SetPermission where
  toPath = Lude.const "/"

instance Lude.ToQuery SetPermission where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSetPermissionResponse' smart constructor.
data SetPermissionResponse = SetPermissionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetPermissionResponse' with the minimum fields required to make a request.
mkSetPermissionResponse ::
  SetPermissionResponse
mkSetPermissionResponse = SetPermissionResponse'
