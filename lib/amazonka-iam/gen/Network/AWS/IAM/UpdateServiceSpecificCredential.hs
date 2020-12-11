{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateServiceSpecificCredential
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the status of a service-specific credential to @Active@ or @Inactive@ . Service-specific credentials that are inactive cannot be used for authentication to the service. This operation can be used to disable a user's service-specific credential as part of a credential rotation work flow.
module Network.AWS.IAM.UpdateServiceSpecificCredential
  ( -- * Creating a request
    UpdateServiceSpecificCredential (..),
    mkUpdateServiceSpecificCredential,

    -- ** Request lenses
    usscUserName,
    usscServiceSpecificCredentialId,
    usscStatus,

    -- * Destructuring the response
    UpdateServiceSpecificCredentialResponse (..),
    mkUpdateServiceSpecificCredentialResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateServiceSpecificCredential' smart constructor.
data UpdateServiceSpecificCredential = UpdateServiceSpecificCredential'
  { userName ::
      Lude.Maybe Lude.Text,
    serviceSpecificCredentialId ::
      Lude.Text,
    status :: StatusType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateServiceSpecificCredential' with the minimum fields required to make a request.
--
-- * 'serviceSpecificCredentialId' - The unique identifier of the service-specific credential.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
-- * 'status' - The status to be assigned to the service-specific credential.
-- * 'userName' - The name of the IAM user associated with the service-specific credential. If you do not specify this value, then the operation assumes the user whose credentials are used to call the operation.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkUpdateServiceSpecificCredential ::
  -- | 'serviceSpecificCredentialId'
  Lude.Text ->
  -- | 'status'
  StatusType ->
  UpdateServiceSpecificCredential
mkUpdateServiceSpecificCredential
  pServiceSpecificCredentialId_
  pStatus_ =
    UpdateServiceSpecificCredential'
      { userName = Lude.Nothing,
        serviceSpecificCredentialId = pServiceSpecificCredentialId_,
        status = pStatus_
      }

-- | The name of the IAM user associated with the service-specific credential. If you do not specify this value, then the operation assumes the user whose credentials are used to call the operation.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usscUserName :: Lens.Lens' UpdateServiceSpecificCredential (Lude.Maybe Lude.Text)
usscUserName = Lens.lens (userName :: UpdateServiceSpecificCredential -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: UpdateServiceSpecificCredential)
{-# DEPRECATED usscUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The unique identifier of the service-specific credential.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
--
-- /Note:/ Consider using 'serviceSpecificCredentialId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usscServiceSpecificCredentialId :: Lens.Lens' UpdateServiceSpecificCredential Lude.Text
usscServiceSpecificCredentialId = Lens.lens (serviceSpecificCredentialId :: UpdateServiceSpecificCredential -> Lude.Text) (\s a -> s {serviceSpecificCredentialId = a} :: UpdateServiceSpecificCredential)
{-# DEPRECATED usscServiceSpecificCredentialId "Use generic-lens or generic-optics with 'serviceSpecificCredentialId' instead." #-}

-- | The status to be assigned to the service-specific credential.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usscStatus :: Lens.Lens' UpdateServiceSpecificCredential StatusType
usscStatus = Lens.lens (status :: UpdateServiceSpecificCredential -> StatusType) (\s a -> s {status = a} :: UpdateServiceSpecificCredential)
{-# DEPRECATED usscStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.AWSRequest UpdateServiceSpecificCredential where
  type
    Rs UpdateServiceSpecificCredential =
      UpdateServiceSpecificCredentialResponse
  request = Req.postQuery iamService
  response = Res.receiveNull UpdateServiceSpecificCredentialResponse'

instance Lude.ToHeaders UpdateServiceSpecificCredential where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateServiceSpecificCredential where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateServiceSpecificCredential where
  toQuery UpdateServiceSpecificCredential' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("UpdateServiceSpecificCredential" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName,
        "ServiceSpecificCredentialId" Lude.=: serviceSpecificCredentialId,
        "Status" Lude.=: status
      ]

-- | /See:/ 'mkUpdateServiceSpecificCredentialResponse' smart constructor.
data UpdateServiceSpecificCredentialResponse = UpdateServiceSpecificCredentialResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateServiceSpecificCredentialResponse' with the minimum fields required to make a request.
mkUpdateServiceSpecificCredentialResponse ::
  UpdateServiceSpecificCredentialResponse
mkUpdateServiceSpecificCredentialResponse =
  UpdateServiceSpecificCredentialResponse'
