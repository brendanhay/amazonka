{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.GetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a group.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.GetGroup
  ( -- * Creating a request
    GetGroup (..),
    mkGetGroup,

    -- ** Request lenses
    ggGroupName,
    ggUserPoolId,

    -- * Destructuring the response
    GetGroupResponse (..),
    mkGetGroupResponse,

    -- ** Response lenses
    ggrsGroup,
    ggrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetGroup' smart constructor.
data GetGroup = GetGroup'
  { groupName :: Lude.Text,
    userPoolId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGroup' with the minimum fields required to make a request.
--
-- * 'groupName' - The name of the group.
-- * 'userPoolId' - The user pool ID for the user pool.
mkGetGroup ::
  -- | 'groupName'
  Lude.Text ->
  -- | 'userPoolId'
  Lude.Text ->
  GetGroup
mkGetGroup pGroupName_ pUserPoolId_ =
  GetGroup' {groupName = pGroupName_, userPoolId = pUserPoolId_}

-- | The name of the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggGroupName :: Lens.Lens' GetGroup Lude.Text
ggGroupName = Lens.lens (groupName :: GetGroup -> Lude.Text) (\s a -> s {groupName = a} :: GetGroup)
{-# DEPRECATED ggGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggUserPoolId :: Lens.Lens' GetGroup Lude.Text
ggUserPoolId = Lens.lens (userPoolId :: GetGroup -> Lude.Text) (\s a -> s {userPoolId = a} :: GetGroup)
{-# DEPRECATED ggUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Lude.AWSRequest GetGroup where
  type Rs GetGroup = GetGroupResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetGroupResponse'
            Lude.<$> (x Lude..?> "Group") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSCognitoIdentityProviderService.GetGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetGroup where
  toJSON GetGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GroupName" Lude..= groupName),
            Lude.Just ("UserPoolId" Lude..= userPoolId)
          ]
      )

instance Lude.ToPath GetGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery GetGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetGroupResponse' smart constructor.
data GetGroupResponse = GetGroupResponse'
  { group ::
      Lude.Maybe GroupType,
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

-- | Creates a value of 'GetGroupResponse' with the minimum fields required to make a request.
--
-- * 'group' - The group object for the group.
-- * 'responseStatus' - The response status code.
mkGetGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetGroupResponse
mkGetGroupResponse pResponseStatus_ =
  GetGroupResponse'
    { group = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The group object for the group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrsGroup :: Lens.Lens' GetGroupResponse (Lude.Maybe GroupType)
ggrsGroup = Lens.lens (group :: GetGroupResponse -> Lude.Maybe GroupType) (\s a -> s {group = a} :: GetGroupResponse)
{-# DEPRECATED ggrsGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggrsResponseStatus :: Lens.Lens' GetGroupResponse Lude.Int
ggrsResponseStatus = Lens.lens (responseStatus :: GetGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetGroupResponse)
{-# DEPRECATED ggrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
