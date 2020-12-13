{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified IAM user, including the user's creation date, path, unique ID, and ARN.
--
-- If you do not specify a user name, IAM determines the user name implicitly based on the AWS access key ID used to sign the request to this API.
module Network.AWS.IAM.GetUser
  ( -- * Creating a request
    GetUser (..),
    mkGetUser,

    -- ** Request lenses
    guUserName,

    -- * Destructuring the response
    GetUserResponse (..),
    mkGetUserResponse,

    -- ** Response lenses
    gursUser,
    gursResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetUser' smart constructor.
newtype GetUser = GetUser'
  { -- | The name of the user to get information about.
    --
    -- This parameter is optional. If it is not included, it defaults to the user making the request. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUser' with the minimum fields required to make a request.
--
-- * 'userName' - The name of the user to get information about.
--
-- This parameter is optional. If it is not included, it defaults to the user making the request. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkGetUser ::
  GetUser
mkGetUser = GetUser' {userName = Lude.Nothing}

-- | The name of the user to get information about.
--
-- This parameter is optional. If it is not included, it defaults to the user making the request. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guUserName :: Lens.Lens' GetUser (Lude.Maybe Lude.Text)
guUserName = Lens.lens (userName :: GetUser -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: GetUser)
{-# DEPRECATED guUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Lude.AWSRequest GetUser where
  type Rs GetUser = GetUserResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "GetUserResult"
      ( \s h x ->
          GetUserResponse'
            Lude.<$> (x Lude..@ "User") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetUser where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetUser where
  toPath = Lude.const "/"

instance Lude.ToQuery GetUser where
  toQuery GetUser' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetUser" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName
      ]

-- | Contains the response to a successful 'GetUser' request.
--
-- /See:/ 'mkGetUserResponse' smart constructor.
data GetUserResponse = GetUserResponse'
  { -- | A structure containing details about the IAM user.
    --
    -- /Important:/ Due to a service issue, password last used data does not include password use from May 3, 2018 22:50 PDT to May 23, 2018 14:08 PDT. This affects <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_finding-unused.html last sign-in> dates shown in the IAM console and password last used dates in the <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_getting-report.html IAM credential report> , and returned by this GetUser API. If users signed in during the affected time, the password last used date that is returned is the date the user last signed in before May 3, 2018. For users that signed in after May 23, 2018 14:08 PDT, the returned password last used date is accurate.
    -- You can use password last used information to identify unused credentials for deletion. For example, you might delete users who did not sign in to AWS in the last 90 days. In cases like this, we recommend that you adjust your evaluation window to include dates after May 23, 2018. Alternatively, if your users use access keys to access AWS programmatically you can refer to access key last used information because it is accurate for all dates.
    user :: User,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUserResponse' with the minimum fields required to make a request.
--
-- * 'user' - A structure containing details about the IAM user.
--
-- /Important:/ Due to a service issue, password last used data does not include password use from May 3, 2018 22:50 PDT to May 23, 2018 14:08 PDT. This affects <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_finding-unused.html last sign-in> dates shown in the IAM console and password last used dates in the <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_getting-report.html IAM credential report> , and returned by this GetUser API. If users signed in during the affected time, the password last used date that is returned is the date the user last signed in before May 3, 2018. For users that signed in after May 23, 2018 14:08 PDT, the returned password last used date is accurate.
-- You can use password last used information to identify unused credentials for deletion. For example, you might delete users who did not sign in to AWS in the last 90 days. In cases like this, we recommend that you adjust your evaluation window to include dates after May 23, 2018. Alternatively, if your users use access keys to access AWS programmatically you can refer to access key last used information because it is accurate for all dates.
-- * 'responseStatus' - The response status code.
mkGetUserResponse ::
  -- | 'user'
  User ->
  -- | 'responseStatus'
  Lude.Int ->
  GetUserResponse
mkGetUserResponse pUser_ pResponseStatus_ =
  GetUserResponse'
    { user = pUser_,
      responseStatus = pResponseStatus_
    }

-- | A structure containing details about the IAM user.
--
-- /Important:/ Due to a service issue, password last used data does not include password use from May 3, 2018 22:50 PDT to May 23, 2018 14:08 PDT. This affects <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_finding-unused.html last sign-in> dates shown in the IAM console and password last used dates in the <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_getting-report.html IAM credential report> , and returned by this GetUser API. If users signed in during the affected time, the password last used date that is returned is the date the user last signed in before May 3, 2018. For users that signed in after May 23, 2018 14:08 PDT, the returned password last used date is accurate.
-- You can use password last used information to identify unused credentials for deletion. For example, you might delete users who did not sign in to AWS in the last 90 days. In cases like this, we recommend that you adjust your evaluation window to include dates after May 23, 2018. Alternatively, if your users use access keys to access AWS programmatically you can refer to access key last used information because it is accurate for all dates.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gursUser :: Lens.Lens' GetUserResponse User
gursUser = Lens.lens (user :: GetUserResponse -> User) (\s a -> s {user = a} :: GetUserResponse)
{-# DEPRECATED gursUser "Use generic-lens or generic-optics with 'user' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gursResponseStatus :: Lens.Lens' GetUserResponse Lude.Int
gursResponseStatus = Lens.lens (responseStatus :: GetUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetUserResponse)
{-# DEPRECATED gursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
