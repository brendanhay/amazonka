{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a group. Currently only groups with no members can be deleted.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.DeleteGroup
  ( -- * Creating a request
    DeleteGroup (..),
    mkDeleteGroup,

    -- ** Request lenses
    dgUserPoolId,
    dgGroupName,

    -- * Destructuring the response
    DeleteGroupResponse (..),
    mkDeleteGroupResponse,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteGroup' smart constructor.
data DeleteGroup = DeleteGroup'
  { -- | The user pool ID for the user pool.
    userPoolId :: Lude.Text,
    -- | The name of the group.
    groupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGroup' with the minimum fields required to make a request.
--
-- * 'userPoolId' - The user pool ID for the user pool.
-- * 'groupName' - The name of the group.
mkDeleteGroup ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'groupName'
  Lude.Text ->
  DeleteGroup
mkDeleteGroup pUserPoolId_ pGroupName_ =
  DeleteGroup' {userPoolId = pUserPoolId_, groupName = pGroupName_}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgUserPoolId :: Lens.Lens' DeleteGroup Lude.Text
dgUserPoolId = Lens.lens (userPoolId :: DeleteGroup -> Lude.Text) (\s a -> s {userPoolId = a} :: DeleteGroup)
{-# DEPRECATED dgUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The name of the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgGroupName :: Lens.Lens' DeleteGroup Lude.Text
dgGroupName = Lens.lens (groupName :: DeleteGroup -> Lude.Text) (\s a -> s {groupName = a} :: DeleteGroup)
{-# DEPRECATED dgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.AWSRequest DeleteGroup where
  type Rs DeleteGroup = DeleteGroupResponse
  request = Req.postJSON cognitoIdentityProviderService
  response = Res.receiveNull DeleteGroupResponse'

instance Lude.ToHeaders DeleteGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.DeleteGroup" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteGroup where
  toJSON DeleteGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("GroupName" Lude..= groupName)
          ]
      )

instance Lude.ToPath DeleteGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteGroupResponse' smart constructor.
data DeleteGroupResponse = DeleteGroupResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGroupResponse' with the minimum fields required to make a request.
mkDeleteGroupResponse ::
  DeleteGroupResponse
mkDeleteGroupResponse = DeleteGroupResponse'
