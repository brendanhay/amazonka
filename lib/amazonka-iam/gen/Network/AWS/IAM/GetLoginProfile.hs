{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetLoginProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the user name and password-creation date for the specified IAM user. If the user has not been assigned a password, the operation returns a 404 (@NoSuchEntity@ ) error.
module Network.AWS.IAM.GetLoginProfile
  ( -- * Creating a request
    GetLoginProfile (..),
    mkGetLoginProfile,

    -- ** Request lenses
    glpUserName,

    -- * Destructuring the response
    GetLoginProfileResponse (..),
    mkGetLoginProfileResponse,

    -- ** Response lenses
    glprsLoginProfile,
    glprsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetLoginProfile' smart constructor.
newtype GetLoginProfile = GetLoginProfile'
  { -- | The name of the user whose login profile you want to retrieve.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLoginProfile' with the minimum fields required to make a request.
--
-- * 'userName' - The name of the user whose login profile you want to retrieve.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkGetLoginProfile ::
  -- | 'userName'
  Lude.Text ->
  GetLoginProfile
mkGetLoginProfile pUserName_ =
  GetLoginProfile' {userName = pUserName_}

-- | The name of the user whose login profile you want to retrieve.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glpUserName :: Lens.Lens' GetLoginProfile Lude.Text
glpUserName = Lens.lens (userName :: GetLoginProfile -> Lude.Text) (\s a -> s {userName = a} :: GetLoginProfile)
{-# DEPRECATED glpUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Lude.AWSRequest GetLoginProfile where
  type Rs GetLoginProfile = GetLoginProfileResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "GetLoginProfileResult"
      ( \s h x ->
          GetLoginProfileResponse'
            Lude.<$> (x Lude..@ "LoginProfile") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetLoginProfile where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetLoginProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery GetLoginProfile where
  toQuery GetLoginProfile' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetLoginProfile" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName
      ]

-- | Contains the response to a successful 'GetLoginProfile' request.
--
-- /See:/ 'mkGetLoginProfileResponse' smart constructor.
data GetLoginProfileResponse = GetLoginProfileResponse'
  { -- | A structure containing the user name and password create date for the user.
    loginProfile :: LoginProfile,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLoginProfileResponse' with the minimum fields required to make a request.
--
-- * 'loginProfile' - A structure containing the user name and password create date for the user.
-- * 'responseStatus' - The response status code.
mkGetLoginProfileResponse ::
  -- | 'loginProfile'
  LoginProfile ->
  -- | 'responseStatus'
  Lude.Int ->
  GetLoginProfileResponse
mkGetLoginProfileResponse pLoginProfile_ pResponseStatus_ =
  GetLoginProfileResponse'
    { loginProfile = pLoginProfile_,
      responseStatus = pResponseStatus_
    }

-- | A structure containing the user name and password create date for the user.
--
-- /Note:/ Consider using 'loginProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glprsLoginProfile :: Lens.Lens' GetLoginProfileResponse LoginProfile
glprsLoginProfile = Lens.lens (loginProfile :: GetLoginProfileResponse -> LoginProfile) (\s a -> s {loginProfile = a} :: GetLoginProfileResponse)
{-# DEPRECATED glprsLoginProfile "Use generic-lens or generic-optics with 'loginProfile' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glprsResponseStatus :: Lens.Lens' GetLoginProfileResponse Lude.Int
glprsResponseStatus = Lens.lens (responseStatus :: GetLoginProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetLoginProfileResponse)
{-# DEPRECATED glprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
