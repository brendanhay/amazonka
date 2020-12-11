{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreateAccessKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new AWS secret access key and corresponding AWS access key ID for the specified user. The default status for new keys is @Active@ .
--
-- If you do not specify a user name, IAM determines the user name implicitly based on the AWS access key ID signing the request. This operation works for access keys under the AWS account. Consequently, you can use this operation to manage AWS account root user credentials. This is true even if the AWS account has no associated users.
-- The number and size of IAM resources in an AWS account are limited. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html IAM and STS Quotas> in the /IAM User Guide/ .
-- /Important:/ To ensure the security of your AWS account, the secret access key is accessible only during key and user creation. You must save the key (for example, in a text file) if you want to be able to access it again. If a secret key is lost, you can delete the access keys for the associated user and then create new keys.
module Network.AWS.IAM.CreateAccessKey
  ( -- * Creating a request
    CreateAccessKey (..),
    mkCreateAccessKey,

    -- ** Request lenses
    cakUserName,

    -- * Destructuring the response
    CreateAccessKeyResponse (..),
    mkCreateAccessKeyResponse,

    -- ** Response lenses
    cakrsResponseStatus,
    cakrsAccessKey,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateAccessKey' smart constructor.
newtype CreateAccessKey = CreateAccessKey'
  { userName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAccessKey' with the minimum fields required to make a request.
--
-- * 'userName' - The name of the IAM user that the new key will belong to.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkCreateAccessKey ::
  CreateAccessKey
mkCreateAccessKey = CreateAccessKey' {userName = Lude.Nothing}

-- | The name of the IAM user that the new key will belong to.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakUserName :: Lens.Lens' CreateAccessKey (Lude.Maybe Lude.Text)
cakUserName = Lens.lens (userName :: CreateAccessKey -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: CreateAccessKey)
{-# DEPRECATED cakUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Lude.AWSRequest CreateAccessKey where
  type Rs CreateAccessKey = CreateAccessKeyResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "CreateAccessKeyResult"
      ( \s h x ->
          CreateAccessKeyResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..@ "AccessKey")
      )

instance Lude.ToHeaders CreateAccessKey where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateAccessKey where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateAccessKey where
  toQuery CreateAccessKey' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateAccessKey" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName
      ]

-- | Contains the response to a successful 'CreateAccessKey' request.
--
-- /See:/ 'mkCreateAccessKeyResponse' smart constructor.
data CreateAccessKeyResponse = CreateAccessKeyResponse'
  { responseStatus ::
      Lude.Int,
    accessKey :: AccessKeyInfo
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAccessKeyResponse' with the minimum fields required to make a request.
--
-- * 'accessKey' - A structure with details about the access key.
-- * 'responseStatus' - The response status code.
mkCreateAccessKeyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'accessKey'
  AccessKeyInfo ->
  CreateAccessKeyResponse
mkCreateAccessKeyResponse pResponseStatus_ pAccessKey_ =
  CreateAccessKeyResponse'
    { responseStatus = pResponseStatus_,
      accessKey = pAccessKey_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakrsResponseStatus :: Lens.Lens' CreateAccessKeyResponse Lude.Int
cakrsResponseStatus = Lens.lens (responseStatus :: CreateAccessKeyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateAccessKeyResponse)
{-# DEPRECATED cakrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A structure with details about the access key.
--
-- /Note:/ Consider using 'accessKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakrsAccessKey :: Lens.Lens' CreateAccessKeyResponse AccessKeyInfo
cakrsAccessKey = Lens.lens (accessKey :: CreateAccessKeyResponse -> AccessKeyInfo) (\s a -> s {accessKey = a} :: CreateAccessKeyResponse)
{-# DEPRECATED cakrsAccessKey "Use generic-lens or generic-optics with 'accessKey' instead." #-}
