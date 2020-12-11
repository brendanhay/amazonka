{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateRoleDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use 'UpdateRole' instead.
--
-- Modifies only the description of a role. This operation performs the same function as the @Description@ parameter in the @UpdateRole@ operation.
module Network.AWS.IAM.UpdateRoleDescription
  ( -- * Creating a request
    UpdateRoleDescription (..),
    mkUpdateRoleDescription,

    -- ** Request lenses
    urdRoleName,
    urdDescription,

    -- * Destructuring the response
    UpdateRoleDescriptionResponse (..),
    mkUpdateRoleDescriptionResponse,

    -- ** Response lenses
    urdrsRole,
    urdrsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateRoleDescription' smart constructor.
data UpdateRoleDescription = UpdateRoleDescription'
  { roleName ::
      Lude.Text,
    description :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRoleDescription' with the minimum fields required to make a request.
--
-- * 'description' - The new description that you want to apply to the specified role.
-- * 'roleName' - The name of the role that you want to modify.
mkUpdateRoleDescription ::
  -- | 'roleName'
  Lude.Text ->
  -- | 'description'
  Lude.Text ->
  UpdateRoleDescription
mkUpdateRoleDescription pRoleName_ pDescription_ =
  UpdateRoleDescription'
    { roleName = pRoleName_,
      description = pDescription_
    }

-- | The name of the role that you want to modify.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdRoleName :: Lens.Lens' UpdateRoleDescription Lude.Text
urdRoleName = Lens.lens (roleName :: UpdateRoleDescription -> Lude.Text) (\s a -> s {roleName = a} :: UpdateRoleDescription)
{-# DEPRECATED urdRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The new description that you want to apply to the specified role.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdDescription :: Lens.Lens' UpdateRoleDescription Lude.Text
urdDescription = Lens.lens (description :: UpdateRoleDescription -> Lude.Text) (\s a -> s {description = a} :: UpdateRoleDescription)
{-# DEPRECATED urdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdateRoleDescription where
  type Rs UpdateRoleDescription = UpdateRoleDescriptionResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "UpdateRoleDescriptionResult"
      ( \s h x ->
          UpdateRoleDescriptionResponse'
            Lude.<$> (x Lude..@? "Role") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateRoleDescription where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateRoleDescription where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateRoleDescription where
  toQuery UpdateRoleDescription' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UpdateRoleDescription" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "RoleName" Lude.=: roleName,
        "Description" Lude.=: description
      ]

-- | /See:/ 'mkUpdateRoleDescriptionResponse' smart constructor.
data UpdateRoleDescriptionResponse = UpdateRoleDescriptionResponse'
  { role' ::
      Lude.Maybe Role,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRoleDescriptionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'role'' - A structure that contains details about the modified role.
mkUpdateRoleDescriptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateRoleDescriptionResponse
mkUpdateRoleDescriptionResponse pResponseStatus_ =
  UpdateRoleDescriptionResponse'
    { role' = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure that contains details about the modified role.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdrsRole :: Lens.Lens' UpdateRoleDescriptionResponse (Lude.Maybe Role)
urdrsRole = Lens.lens (role' :: UpdateRoleDescriptionResponse -> Lude.Maybe Role) (\s a -> s {role' = a} :: UpdateRoleDescriptionResponse)
{-# DEPRECATED urdrsRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdrsResponseStatus :: Lens.Lens' UpdateRoleDescriptionResponse Lude.Int
urdrsResponseStatus = Lens.lens (responseStatus :: UpdateRoleDescriptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateRoleDescriptionResponse)
{-# DEPRECATED urdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
