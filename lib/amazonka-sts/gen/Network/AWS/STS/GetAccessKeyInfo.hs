{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.GetAccessKeyInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the account identifier for the specified access key ID.
--
-- Access keys consist of two parts: an access key ID (for example, @AKIAIOSFODNN7EXAMPLE@ ) and a secret access key (for example, @wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY@ ). For more information about access keys, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html Managing Access Keys for IAM Users> in the /IAM User Guide/ .
-- When you pass an access key ID to this operation, it returns the ID of the AWS account to which the keys belong. Access key IDs beginning with @AKIA@ are long-term credentials for an IAM user or the AWS account root user. Access key IDs beginning with @ASIA@ are temporary credentials that are created using STS operations. If the account in the response belongs to you, you can sign in as the root user and review your root user access keys. Then, you can pull a <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_getting-report.html credentials report> to learn which IAM user owns the keys. To learn who requested the temporary credentials for an @ASIA@ access key, view the STS events in your <https://docs.aws.amazon.com/IAM/latest/UserGuide/cloudtrail-integration.html CloudTrail logs> in the /IAM User Guide/ .
-- This operation does not indicate the state of the access key. The key might be active, inactive, or deleted. Active keys might not have permissions to perform an operation. Providing a deleted access key might return an error that the key doesn't exist.
module Network.AWS.STS.GetAccessKeyInfo
  ( -- * Creating a request
    GetAccessKeyInfo (..),
    mkGetAccessKeyInfo,

    -- ** Request lenses
    gakiAccessKeyId,

    -- * Destructuring the response
    GetAccessKeyInfoResponse (..),
    mkGetAccessKeyInfoResponse,

    -- ** Response lenses
    gakirsAccount,
    gakirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.STS.Types

-- | /See:/ 'mkGetAccessKeyInfo' smart constructor.
newtype GetAccessKeyInfo = GetAccessKeyInfo'
  { -- | The identifier of an access key.
    --
    -- This parameter allows (through its regex pattern) a string of characters that can consist of any upper- or lowercase letter or digit.
    accessKeyId :: AccessKey
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAccessKeyInfo' with the minimum fields required to make a request.
--
-- * 'accessKeyId' - The identifier of an access key.
--
-- This parameter allows (through its regex pattern) a string of characters that can consist of any upper- or lowercase letter or digit.
mkGetAccessKeyInfo ::
  -- | 'accessKeyId'
  AccessKey ->
  GetAccessKeyInfo
mkGetAccessKeyInfo pAccessKeyId_ =
  GetAccessKeyInfo' {accessKeyId = pAccessKeyId_}

-- | The identifier of an access key.
--
-- This parameter allows (through its regex pattern) a string of characters that can consist of any upper- or lowercase letter or digit.
--
-- /Note:/ Consider using 'accessKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakiAccessKeyId :: Lens.Lens' GetAccessKeyInfo AccessKey
gakiAccessKeyId = Lens.lens (accessKeyId :: GetAccessKeyInfo -> AccessKey) (\s a -> s {accessKeyId = a} :: GetAccessKeyInfo)
{-# DEPRECATED gakiAccessKeyId "Use generic-lens or generic-optics with 'accessKeyId' instead." #-}

instance Lude.AWSRequest GetAccessKeyInfo where
  type Rs GetAccessKeyInfo = GetAccessKeyInfoResponse
  request = Req.postQuery stsService
  response =
    Res.receiveXMLWrapper
      "GetAccessKeyInfoResult"
      ( \s h x ->
          GetAccessKeyInfoResponse'
            Lude.<$> (x Lude..@? "Account") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAccessKeyInfo where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetAccessKeyInfo where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAccessKeyInfo where
  toQuery GetAccessKeyInfo' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetAccessKeyInfo" :: Lude.ByteString),
        "Version" Lude.=: ("2011-06-15" :: Lude.ByteString),
        "AccessKeyId" Lude.=: accessKeyId
      ]

-- | /See:/ 'mkGetAccessKeyInfoResponse' smart constructor.
data GetAccessKeyInfoResponse = GetAccessKeyInfoResponse'
  { -- | The number used to identify the AWS account.
    account :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAccessKeyInfoResponse' with the minimum fields required to make a request.
--
-- * 'account' - The number used to identify the AWS account.
-- * 'responseStatus' - The response status code.
mkGetAccessKeyInfoResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAccessKeyInfoResponse
mkGetAccessKeyInfoResponse pResponseStatus_ =
  GetAccessKeyInfoResponse'
    { account = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The number used to identify the AWS account.
--
-- /Note:/ Consider using 'account' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakirsAccount :: Lens.Lens' GetAccessKeyInfoResponse (Lude.Maybe Lude.Text)
gakirsAccount = Lens.lens (account :: GetAccessKeyInfoResponse -> Lude.Maybe Lude.Text) (\s a -> s {account = a} :: GetAccessKeyInfoResponse)
{-# DEPRECATED gakirsAccount "Use generic-lens or generic-optics with 'account' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakirsResponseStatus :: Lens.Lens' GetAccessKeyInfoResponse Lude.Int
gakirsResponseStatus = Lens.lens (responseStatus :: GetAccessKeyInfoResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAccessKeyInfoResponse)
{-# DEPRECATED gakirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
