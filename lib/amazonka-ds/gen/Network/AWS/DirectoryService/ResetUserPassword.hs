{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.ResetUserPassword
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the password for any user in your AWS Managed Microsoft AD or Simple AD directory.
--
-- You can reset the password for any user in your directory with the following exceptions:
--
--     * For Simple AD, you cannot reset the password for any user that is a member of either the __Domain Admins__ or __Enterprise Admins__ group except for the administrator user.
--
--
--     * For AWS Managed Microsoft AD, you can only reset the password for a user that is in an OU based off of the NetBIOS name that you typed when you created your directory. For example, you cannot reset the password for a user in the __AWS Reserved__ OU. For more information about the OU structure for an AWS Managed Microsoft AD directory, see <https://docs.aws.amazon.com/directoryservice/latest/admin-guide/ms_ad_getting_started_what_gets_created.html What Gets Created> in the /AWS Directory Service Administration Guide/ .
module Network.AWS.DirectoryService.ResetUserPassword
  ( -- * Creating a request
    ResetUserPassword (..),
    mkResetUserPassword,

    -- ** Request lenses
    rupDirectoryId,
    rupNewPassword,
    rupUserName,

    -- * Destructuring the response
    ResetUserPasswordResponse (..),
    mkResetUserPasswordResponse,

    -- ** Response lenses
    ruprsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkResetUserPassword' smart constructor.
data ResetUserPassword = ResetUserPassword'
  { -- | Identifier of the AWS Managed Microsoft AD or Simple AD directory in which the user resides.
    directoryId :: Lude.Text,
    -- | The new password that will be reset.
    newPassword :: Lude.Sensitive Lude.Text,
    -- | The user name of the user whose password will be reset.
    userName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResetUserPassword' with the minimum fields required to make a request.
--
-- * 'directoryId' - Identifier of the AWS Managed Microsoft AD or Simple AD directory in which the user resides.
-- * 'newPassword' - The new password that will be reset.
-- * 'userName' - The user name of the user whose password will be reset.
mkResetUserPassword ::
  -- | 'directoryId'
  Lude.Text ->
  -- | 'newPassword'
  Lude.Sensitive Lude.Text ->
  -- | 'userName'
  Lude.Text ->
  ResetUserPassword
mkResetUserPassword pDirectoryId_ pNewPassword_ pUserName_ =
  ResetUserPassword'
    { directoryId = pDirectoryId_,
      newPassword = pNewPassword_,
      userName = pUserName_
    }

-- | Identifier of the AWS Managed Microsoft AD or Simple AD directory in which the user resides.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rupDirectoryId :: Lens.Lens' ResetUserPassword Lude.Text
rupDirectoryId = Lens.lens (directoryId :: ResetUserPassword -> Lude.Text) (\s a -> s {directoryId = a} :: ResetUserPassword)
{-# DEPRECATED rupDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The new password that will be reset.
--
-- /Note:/ Consider using 'newPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rupNewPassword :: Lens.Lens' ResetUserPassword (Lude.Sensitive Lude.Text)
rupNewPassword = Lens.lens (newPassword :: ResetUserPassword -> Lude.Sensitive Lude.Text) (\s a -> s {newPassword = a} :: ResetUserPassword)
{-# DEPRECATED rupNewPassword "Use generic-lens or generic-optics with 'newPassword' instead." #-}

-- | The user name of the user whose password will be reset.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rupUserName :: Lens.Lens' ResetUserPassword Lude.Text
rupUserName = Lens.lens (userName :: ResetUserPassword -> Lude.Text) (\s a -> s {userName = a} :: ResetUserPassword)
{-# DEPRECATED rupUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Lude.AWSRequest ResetUserPassword where
  type Rs ResetUserPassword = ResetUserPasswordResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ResetUserPasswordResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ResetUserPassword where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.ResetUserPassword" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ResetUserPassword where
  toJSON ResetUserPassword' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            Lude.Just ("NewPassword" Lude..= newPassword),
            Lude.Just ("UserName" Lude..= userName)
          ]
      )

instance Lude.ToPath ResetUserPassword where
  toPath = Lude.const "/"

instance Lude.ToQuery ResetUserPassword where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkResetUserPasswordResponse' smart constructor.
newtype ResetUserPasswordResponse = ResetUserPasswordResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResetUserPasswordResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkResetUserPasswordResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ResetUserPasswordResponse
mkResetUserPasswordResponse pResponseStatus_ =
  ResetUserPasswordResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ruprsResponseStatus :: Lens.Lens' ResetUserPasswordResponse Lude.Int
ruprsResponseStatus = Lens.lens (responseStatus :: ResetUserPasswordResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ResetUserPasswordResponse)
{-# DEPRECATED ruprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
