{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteAccessKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the access key pair associated with the specified IAM user.
--
-- If you do not specify a user name, IAM determines the user name implicitly based on the AWS access key ID signing the request. This operation works for access keys under the AWS account. Consequently, you can use this operation to manage AWS account root user credentials even if the AWS account has no associated users.
module Network.AWS.IAM.DeleteAccessKey
  ( -- * Creating a request
    DeleteAccessKey (..),
    mkDeleteAccessKey,

    -- ** Request lenses
    dakUserName,
    dakAccessKeyId,

    -- * Destructuring the response
    DeleteAccessKeyResponse (..),
    mkDeleteAccessKeyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAccessKey' smart constructor.
data DeleteAccessKey = DeleteAccessKey'
  { -- | The name of the user whose access key pair you want to delete.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Lude.Maybe Lude.Text,
    -- | The access key ID for the access key ID and secret access key you want to delete.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
    accessKeyId :: AccessKey
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAccessKey' with the minimum fields required to make a request.
--
-- * 'userName' - The name of the user whose access key pair you want to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'accessKeyId' - The access key ID for the access key ID and secret access key you want to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
mkDeleteAccessKey ::
  -- | 'accessKeyId'
  AccessKey ->
  DeleteAccessKey
mkDeleteAccessKey pAccessKeyId_ =
  DeleteAccessKey'
    { userName = Lude.Nothing,
      accessKeyId = pAccessKeyId_
    }

-- | The name of the user whose access key pair you want to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dakUserName :: Lens.Lens' DeleteAccessKey (Lude.Maybe Lude.Text)
dakUserName = Lens.lens (userName :: DeleteAccessKey -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: DeleteAccessKey)
{-# DEPRECATED dakUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The access key ID for the access key ID and secret access key you want to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
--
-- /Note:/ Consider using 'accessKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dakAccessKeyId :: Lens.Lens' DeleteAccessKey AccessKey
dakAccessKeyId = Lens.lens (accessKeyId :: DeleteAccessKey -> AccessKey) (\s a -> s {accessKeyId = a} :: DeleteAccessKey)
{-# DEPRECATED dakAccessKeyId "Use generic-lens or generic-optics with 'accessKeyId' instead." #-}

instance Lude.AWSRequest DeleteAccessKey where
  type Rs DeleteAccessKey = DeleteAccessKeyResponse
  request = Req.postQuery iamService
  response = Res.receiveNull DeleteAccessKeyResponse'

instance Lude.ToHeaders DeleteAccessKey where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteAccessKey where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAccessKey where
  toQuery DeleteAccessKey' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteAccessKey" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName,
        "AccessKeyId" Lude.=: accessKeyId
      ]

-- | /See:/ 'mkDeleteAccessKeyResponse' smart constructor.
data DeleteAccessKeyResponse = DeleteAccessKeyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAccessKeyResponse' with the minimum fields required to make a request.
mkDeleteAccessKeyResponse ::
  DeleteAccessKeyResponse
mkDeleteAccessKeyResponse = DeleteAccessKeyResponse'
