{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.UpdateManagedInstanceRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the Amazon Identity and Access Management (IAM) role that is assigned to the on-premises instance or virtual machines (VM). IAM roles are first assigned to these hybrid instances during the activation process. For more information, see 'CreateActivation' .
module Network.AWS.SSM.UpdateManagedInstanceRole
  ( -- * Creating a request
    UpdateManagedInstanceRole (..),
    mkUpdateManagedInstanceRole,

    -- ** Request lenses
    umirInstanceId,
    umirIAMRole,

    -- * Destructuring the response
    UpdateManagedInstanceRoleResponse (..),
    mkUpdateManagedInstanceRoleResponse,

    -- ** Response lenses
    umirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkUpdateManagedInstanceRole' smart constructor.
data UpdateManagedInstanceRole = UpdateManagedInstanceRole'
  { instanceId ::
      Lude.Text,
    iamRole :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateManagedInstanceRole' with the minimum fields required to make a request.
--
-- * 'iamRole' - The IAM role you want to assign or change.
-- * 'instanceId' - The ID of the managed instance where you want to update the role.
mkUpdateManagedInstanceRole ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'iamRole'
  Lude.Text ->
  UpdateManagedInstanceRole
mkUpdateManagedInstanceRole pInstanceId_ pIAMRole_ =
  UpdateManagedInstanceRole'
    { instanceId = pInstanceId_,
      iamRole = pIAMRole_
    }

-- | The ID of the managed instance where you want to update the role.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umirInstanceId :: Lens.Lens' UpdateManagedInstanceRole Lude.Text
umirInstanceId = Lens.lens (instanceId :: UpdateManagedInstanceRole -> Lude.Text) (\s a -> s {instanceId = a} :: UpdateManagedInstanceRole)
{-# DEPRECATED umirInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The IAM role you want to assign or change.
--
-- /Note:/ Consider using 'iamRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umirIAMRole :: Lens.Lens' UpdateManagedInstanceRole Lude.Text
umirIAMRole = Lens.lens (iamRole :: UpdateManagedInstanceRole -> Lude.Text) (\s a -> s {iamRole = a} :: UpdateManagedInstanceRole)
{-# DEPRECATED umirIAMRole "Use generic-lens or generic-optics with 'iamRole' instead." #-}

instance Lude.AWSRequest UpdateManagedInstanceRole where
  type
    Rs UpdateManagedInstanceRole =
      UpdateManagedInstanceRoleResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateManagedInstanceRoleResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateManagedInstanceRole where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.UpdateManagedInstanceRole" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateManagedInstanceRole where
  toJSON UpdateManagedInstanceRole' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("InstanceId" Lude..= instanceId),
            Lude.Just ("IamRole" Lude..= iamRole)
          ]
      )

instance Lude.ToPath UpdateManagedInstanceRole where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateManagedInstanceRole where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateManagedInstanceRoleResponse' smart constructor.
newtype UpdateManagedInstanceRoleResponse = UpdateManagedInstanceRoleResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateManagedInstanceRoleResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateManagedInstanceRoleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateManagedInstanceRoleResponse
mkUpdateManagedInstanceRoleResponse pResponseStatus_ =
  UpdateManagedInstanceRoleResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umirrsResponseStatus :: Lens.Lens' UpdateManagedInstanceRoleResponse Lude.Int
umirrsResponseStatus = Lens.lens (responseStatus :: UpdateManagedInstanceRoleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateManagedInstanceRoleResponse)
{-# DEPRECATED umirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
