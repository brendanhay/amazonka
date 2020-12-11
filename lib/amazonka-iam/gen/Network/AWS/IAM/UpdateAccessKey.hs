{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateAccessKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the status of the specified access key from Active to Inactive, or vice versa. This operation can be used to disable a user's key as part of a key rotation workflow.
--
-- If the @UserName@ is not specified, the user name is determined implicitly based on the AWS access key ID used to sign the request. This operation works for access keys under the AWS account. Consequently, you can use this operation to manage AWS account root user credentials even if the AWS account has no associated users.
-- For information about rotating keys, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/ManagingCredentials.html Managing Keys and Certificates> in the /IAM User Guide/ .
module Network.AWS.IAM.UpdateAccessKey
  ( -- * Creating a request
    UpdateAccessKey (..),
    mkUpdateAccessKey,

    -- ** Request lenses
    uakUserName,
    uakAccessKeyId,
    uakStatus,

    -- * Destructuring the response
    UpdateAccessKeyResponse (..),
    mkUpdateAccessKeyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateAccessKey' smart constructor.
data UpdateAccessKey = UpdateAccessKey'
  { userName ::
      Lude.Maybe Lude.Text,
    accessKeyId :: AccessKey,
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

-- | Creates a value of 'UpdateAccessKey' with the minimum fields required to make a request.
--
-- * 'accessKeyId' - The access key ID of the secret access key you want to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
-- * 'status' - The status you want to assign to the secret access key. @Active@ means that the key can be used for API calls to AWS, while @Inactive@ means that the key cannot be used.
-- * 'userName' - The name of the user whose key you want to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkUpdateAccessKey ::
  -- | 'accessKeyId'
  AccessKey ->
  -- | 'status'
  StatusType ->
  UpdateAccessKey
mkUpdateAccessKey pAccessKeyId_ pStatus_ =
  UpdateAccessKey'
    { userName = Lude.Nothing,
      accessKeyId = pAccessKeyId_,
      status = pStatus_
    }

-- | The name of the user whose key you want to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uakUserName :: Lens.Lens' UpdateAccessKey (Lude.Maybe Lude.Text)
uakUserName = Lens.lens (userName :: UpdateAccessKey -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: UpdateAccessKey)
{-# DEPRECATED uakUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The access key ID of the secret access key you want to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
--
-- /Note:/ Consider using 'accessKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uakAccessKeyId :: Lens.Lens' UpdateAccessKey AccessKey
uakAccessKeyId = Lens.lens (accessKeyId :: UpdateAccessKey -> AccessKey) (\s a -> s {accessKeyId = a} :: UpdateAccessKey)
{-# DEPRECATED uakAccessKeyId "Use generic-lens or generic-optics with 'accessKeyId' instead." #-}

-- | The status you want to assign to the secret access key. @Active@ means that the key can be used for API calls to AWS, while @Inactive@ means that the key cannot be used.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uakStatus :: Lens.Lens' UpdateAccessKey StatusType
uakStatus = Lens.lens (status :: UpdateAccessKey -> StatusType) (\s a -> s {status = a} :: UpdateAccessKey)
{-# DEPRECATED uakStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.AWSRequest UpdateAccessKey where
  type Rs UpdateAccessKey = UpdateAccessKeyResponse
  request = Req.postQuery iamService
  response = Res.receiveNull UpdateAccessKeyResponse'

instance Lude.ToHeaders UpdateAccessKey where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateAccessKey where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateAccessKey where
  toQuery UpdateAccessKey' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UpdateAccessKey" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName,
        "AccessKeyId" Lude.=: accessKeyId,
        "Status" Lude.=: status
      ]

-- | /See:/ 'mkUpdateAccessKeyResponse' smart constructor.
data UpdateAccessKeyResponse = UpdateAccessKeyResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAccessKeyResponse' with the minimum fields required to make a request.
mkUpdateAccessKeyResponse ::
  UpdateAccessKeyResponse
mkUpdateAccessKeyResponse = UpdateAccessKeyResponse'
