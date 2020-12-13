{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteServiceSpecificCredential
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified service-specific credential.
module Network.AWS.IAM.DeleteServiceSpecificCredential
  ( -- * Creating a request
    DeleteServiceSpecificCredential (..),
    mkDeleteServiceSpecificCredential,

    -- ** Request lenses
    dsscUserName,
    dsscServiceSpecificCredentialId,

    -- * Destructuring the response
    DeleteServiceSpecificCredentialResponse (..),
    mkDeleteServiceSpecificCredentialResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteServiceSpecificCredential' smart constructor.
data DeleteServiceSpecificCredential = DeleteServiceSpecificCredential'
  { -- | The name of the IAM user associated with the service-specific credential. If this value is not specified, then the operation assumes the user whose credentials are used to call the operation.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Lude.Maybe Lude.Text,
    -- | The unique identifier of the service-specific credential. You can get this value by calling 'ListServiceSpecificCredentials' .
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
    serviceSpecificCredentialId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteServiceSpecificCredential' with the minimum fields required to make a request.
--
-- * 'userName' - The name of the IAM user associated with the service-specific credential. If this value is not specified, then the operation assumes the user whose credentials are used to call the operation.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'serviceSpecificCredentialId' - The unique identifier of the service-specific credential. You can get this value by calling 'ListServiceSpecificCredentials' .
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
mkDeleteServiceSpecificCredential ::
  -- | 'serviceSpecificCredentialId'
  Lude.Text ->
  DeleteServiceSpecificCredential
mkDeleteServiceSpecificCredential pServiceSpecificCredentialId_ =
  DeleteServiceSpecificCredential'
    { userName = Lude.Nothing,
      serviceSpecificCredentialId = pServiceSpecificCredentialId_
    }

-- | The name of the IAM user associated with the service-specific credential. If this value is not specified, then the operation assumes the user whose credentials are used to call the operation.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsscUserName :: Lens.Lens' DeleteServiceSpecificCredential (Lude.Maybe Lude.Text)
dsscUserName = Lens.lens (userName :: DeleteServiceSpecificCredential -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: DeleteServiceSpecificCredential)
{-# DEPRECATED dsscUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The unique identifier of the service-specific credential. You can get this value by calling 'ListServiceSpecificCredentials' .
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
--
-- /Note:/ Consider using 'serviceSpecificCredentialId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsscServiceSpecificCredentialId :: Lens.Lens' DeleteServiceSpecificCredential Lude.Text
dsscServiceSpecificCredentialId = Lens.lens (serviceSpecificCredentialId :: DeleteServiceSpecificCredential -> Lude.Text) (\s a -> s {serviceSpecificCredentialId = a} :: DeleteServiceSpecificCredential)
{-# DEPRECATED dsscServiceSpecificCredentialId "Use generic-lens or generic-optics with 'serviceSpecificCredentialId' instead." #-}

instance Lude.AWSRequest DeleteServiceSpecificCredential where
  type
    Rs DeleteServiceSpecificCredential =
      DeleteServiceSpecificCredentialResponse
  request = Req.postQuery iamService
  response = Res.receiveNull DeleteServiceSpecificCredentialResponse'

instance Lude.ToHeaders DeleteServiceSpecificCredential where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteServiceSpecificCredential where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteServiceSpecificCredential where
  toQuery DeleteServiceSpecificCredential' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteServiceSpecificCredential" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName,
        "ServiceSpecificCredentialId" Lude.=: serviceSpecificCredentialId
      ]

-- | /See:/ 'mkDeleteServiceSpecificCredentialResponse' smart constructor.
data DeleteServiceSpecificCredentialResponse = DeleteServiceSpecificCredentialResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteServiceSpecificCredentialResponse' with the minimum fields required to make a request.
mkDeleteServiceSpecificCredentialResponse ::
  DeleteServiceSpecificCredentialResponse
mkDeleteServiceSpecificCredentialResponse =
  DeleteServiceSpecificCredentialResponse'
