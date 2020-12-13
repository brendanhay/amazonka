{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ResetServiceSpecificCredential
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the password for a service-specific credential. The new password is AWS generated and cryptographically strong. It cannot be configured by the user. Resetting the password immediately invalidates the previous password associated with this user.
module Network.AWS.IAM.ResetServiceSpecificCredential
  ( -- * Creating a request
    ResetServiceSpecificCredential (..),
    mkResetServiceSpecificCredential,

    -- ** Request lenses
    rsscUserName,
    rsscServiceSpecificCredentialId,

    -- * Destructuring the response
    ResetServiceSpecificCredentialResponse (..),
    mkResetServiceSpecificCredentialResponse,

    -- ** Response lenses
    rsscrsServiceSpecificCredential,
    rsscrsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkResetServiceSpecificCredential' smart constructor.
data ResetServiceSpecificCredential = ResetServiceSpecificCredential'
  { -- | The name of the IAM user associated with the service-specific credential. If this value is not specified, then the operation assumes the user whose credentials are used to call the operation.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Lude.Maybe Lude.Text,
    -- | The unique identifier of the service-specific credential.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
    serviceSpecificCredentialId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResetServiceSpecificCredential' with the minimum fields required to make a request.
--
-- * 'userName' - The name of the IAM user associated with the service-specific credential. If this value is not specified, then the operation assumes the user whose credentials are used to call the operation.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'serviceSpecificCredentialId' - The unique identifier of the service-specific credential.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
mkResetServiceSpecificCredential ::
  -- | 'serviceSpecificCredentialId'
  Lude.Text ->
  ResetServiceSpecificCredential
mkResetServiceSpecificCredential pServiceSpecificCredentialId_ =
  ResetServiceSpecificCredential'
    { userName = Lude.Nothing,
      serviceSpecificCredentialId = pServiceSpecificCredentialId_
    }

-- | The name of the IAM user associated with the service-specific credential. If this value is not specified, then the operation assumes the user whose credentials are used to call the operation.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsscUserName :: Lens.Lens' ResetServiceSpecificCredential (Lude.Maybe Lude.Text)
rsscUserName = Lens.lens (userName :: ResetServiceSpecificCredential -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: ResetServiceSpecificCredential)
{-# DEPRECATED rsscUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The unique identifier of the service-specific credential.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
--
-- /Note:/ Consider using 'serviceSpecificCredentialId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsscServiceSpecificCredentialId :: Lens.Lens' ResetServiceSpecificCredential Lude.Text
rsscServiceSpecificCredentialId = Lens.lens (serviceSpecificCredentialId :: ResetServiceSpecificCredential -> Lude.Text) (\s a -> s {serviceSpecificCredentialId = a} :: ResetServiceSpecificCredential)
{-# DEPRECATED rsscServiceSpecificCredentialId "Use generic-lens or generic-optics with 'serviceSpecificCredentialId' instead." #-}

instance Lude.AWSRequest ResetServiceSpecificCredential where
  type
    Rs ResetServiceSpecificCredential =
      ResetServiceSpecificCredentialResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ResetServiceSpecificCredentialResult"
      ( \s h x ->
          ResetServiceSpecificCredentialResponse'
            Lude.<$> (x Lude..@? "ServiceSpecificCredential")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ResetServiceSpecificCredential where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ResetServiceSpecificCredential where
  toPath = Lude.const "/"

instance Lude.ToQuery ResetServiceSpecificCredential where
  toQuery ResetServiceSpecificCredential' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ResetServiceSpecificCredential" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName,
        "ServiceSpecificCredentialId" Lude.=: serviceSpecificCredentialId
      ]

-- | /See:/ 'mkResetServiceSpecificCredentialResponse' smart constructor.
data ResetServiceSpecificCredentialResponse = ResetServiceSpecificCredentialResponse'
  { -- | A structure with details about the updated service-specific credential, including the new password.
    --
    -- /Important:/ This is the __only__ time that you can access the password. You cannot recover the password later, but you can reset it again.
    serviceSpecificCredential :: Lude.Maybe ServiceSpecificCredential,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResetServiceSpecificCredentialResponse' with the minimum fields required to make a request.
--
-- * 'serviceSpecificCredential' - A structure with details about the updated service-specific credential, including the new password.
--
-- /Important:/ This is the __only__ time that you can access the password. You cannot recover the password later, but you can reset it again.
-- * 'responseStatus' - The response status code.
mkResetServiceSpecificCredentialResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ResetServiceSpecificCredentialResponse
mkResetServiceSpecificCredentialResponse pResponseStatus_ =
  ResetServiceSpecificCredentialResponse'
    { serviceSpecificCredential =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure with details about the updated service-specific credential, including the new password.
--
-- /Important:/ This is the __only__ time that you can access the password. You cannot recover the password later, but you can reset it again.
--
-- /Note:/ Consider using 'serviceSpecificCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsscrsServiceSpecificCredential :: Lens.Lens' ResetServiceSpecificCredentialResponse (Lude.Maybe ServiceSpecificCredential)
rsscrsServiceSpecificCredential = Lens.lens (serviceSpecificCredential :: ResetServiceSpecificCredentialResponse -> Lude.Maybe ServiceSpecificCredential) (\s a -> s {serviceSpecificCredential = a} :: ResetServiceSpecificCredentialResponse)
{-# DEPRECATED rsscrsServiceSpecificCredential "Use generic-lens or generic-optics with 'serviceSpecificCredential' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsscrsResponseStatus :: Lens.Lens' ResetServiceSpecificCredentialResponse Lude.Int
rsscrsResponseStatus = Lens.lens (responseStatus :: ResetServiceSpecificCredentialResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ResetServiceSpecificCredentialResponse)
{-# DEPRECATED rsscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
