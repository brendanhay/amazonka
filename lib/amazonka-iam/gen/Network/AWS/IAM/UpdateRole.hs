{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description or maximum session duration setting of a role.
module Network.AWS.IAM.UpdateRole
  ( -- * Creating a request
    UpdateRole (..),
    mkUpdateRole,

    -- ** Request lenses
    urMaxSessionDuration,
    urRoleName,
    urDescription,

    -- * Destructuring the response
    UpdateRoleResponse (..),
    mkUpdateRoleResponse,

    -- ** Response lenses
    urrsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateRole' smart constructor.
data UpdateRole = UpdateRole'
  { -- | The maximum session duration (in seconds) that you want to set for the specified role. If you do not specify a value for this setting, the default maximum of one hour is applied. This setting can have a value from 1 hour to 12 hours.
    --
    -- Anyone who assumes the role from the AWS CLI or API can use the @DurationSeconds@ API parameter or the @duration-seconds@ CLI parameter to request a longer session. The @MaxSessionDuration@ setting determines the maximum duration that can be requested using the @DurationSeconds@ parameter. If users don't specify a value for the @DurationSeconds@ parameter, their security credentials are valid for one hour by default. This applies when you use the @AssumeRole*@ API operations or the @assume-role*@ CLI operations but does not apply when you use those operations to create a console URL. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html Using IAM Roles> in the /IAM User Guide/ .
    maxSessionDuration :: Lude.Maybe Lude.Natural,
    -- | The name of the role that you want to modify.
    roleName :: Lude.Text,
    -- | The new description that you want to apply to the specified role.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRole' with the minimum fields required to make a request.
--
-- * 'maxSessionDuration' - The maximum session duration (in seconds) that you want to set for the specified role. If you do not specify a value for this setting, the default maximum of one hour is applied. This setting can have a value from 1 hour to 12 hours.
--
-- Anyone who assumes the role from the AWS CLI or API can use the @DurationSeconds@ API parameter or the @duration-seconds@ CLI parameter to request a longer session. The @MaxSessionDuration@ setting determines the maximum duration that can be requested using the @DurationSeconds@ parameter. If users don't specify a value for the @DurationSeconds@ parameter, their security credentials are valid for one hour by default. This applies when you use the @AssumeRole*@ API operations or the @assume-role*@ CLI operations but does not apply when you use those operations to create a console URL. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html Using IAM Roles> in the /IAM User Guide/ .
-- * 'roleName' - The name of the role that you want to modify.
-- * 'description' - The new description that you want to apply to the specified role.
mkUpdateRole ::
  -- | 'roleName'
  Lude.Text ->
  UpdateRole
mkUpdateRole pRoleName_ =
  UpdateRole'
    { maxSessionDuration = Lude.Nothing,
      roleName = pRoleName_,
      description = Lude.Nothing
    }

-- | The maximum session duration (in seconds) that you want to set for the specified role. If you do not specify a value for this setting, the default maximum of one hour is applied. This setting can have a value from 1 hour to 12 hours.
--
-- Anyone who assumes the role from the AWS CLI or API can use the @DurationSeconds@ API parameter or the @duration-seconds@ CLI parameter to request a longer session. The @MaxSessionDuration@ setting determines the maximum duration that can be requested using the @DurationSeconds@ parameter. If users don't specify a value for the @DurationSeconds@ parameter, their security credentials are valid for one hour by default. This applies when you use the @AssumeRole*@ API operations or the @assume-role*@ CLI operations but does not apply when you use those operations to create a console URL. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html Using IAM Roles> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'maxSessionDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urMaxSessionDuration :: Lens.Lens' UpdateRole (Lude.Maybe Lude.Natural)
urMaxSessionDuration = Lens.lens (maxSessionDuration :: UpdateRole -> Lude.Maybe Lude.Natural) (\s a -> s {maxSessionDuration = a} :: UpdateRole)
{-# DEPRECATED urMaxSessionDuration "Use generic-lens or generic-optics with 'maxSessionDuration' instead." #-}

-- | The name of the role that you want to modify.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urRoleName :: Lens.Lens' UpdateRole Lude.Text
urRoleName = Lens.lens (roleName :: UpdateRole -> Lude.Text) (\s a -> s {roleName = a} :: UpdateRole)
{-# DEPRECATED urRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The new description that you want to apply to the specified role.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urDescription :: Lens.Lens' UpdateRole (Lude.Maybe Lude.Text)
urDescription = Lens.lens (description :: UpdateRole -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateRole)
{-# DEPRECATED urDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdateRole where
  type Rs UpdateRole = UpdateRoleResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "UpdateRoleResult"
      ( \s h x ->
          UpdateRoleResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateRole where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateRole where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateRole where
  toQuery UpdateRole' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UpdateRole" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "MaxSessionDuration" Lude.=: maxSessionDuration,
        "RoleName" Lude.=: roleName,
        "Description" Lude.=: description
      ]

-- | /See:/ 'mkUpdateRoleResponse' smart constructor.
newtype UpdateRoleResponse = UpdateRoleResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRoleResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateRoleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateRoleResponse
mkUpdateRoleResponse pResponseStatus_ =
  UpdateRoleResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsResponseStatus :: Lens.Lens' UpdateRoleResponse Lude.Int
urrsResponseStatus = Lens.lens (responseStatus :: UpdateRoleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateRoleResponse)
{-# DEPRECATED urrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
