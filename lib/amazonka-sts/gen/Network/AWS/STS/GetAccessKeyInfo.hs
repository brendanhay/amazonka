{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
--
-- Access keys consist of two parts: an access key ID (for example, @AKIAIOSFODNN7EXAMPLE@ ) and a secret access key (for example, @wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY@ ). For more information about access keys, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html Managing Access Keys for IAM Users> in the /IAM User Guide/ .
--
-- When you pass an access key ID to this operation, it returns the ID of the AWS account to which the keys belong. Access key IDs beginning with @AKIA@ are long-term credentials for an IAM user or the AWS account root user. Access key IDs beginning with @ASIA@ are temporary credentials that are created using STS operations. If the account in the response belongs to you, you can sign in as the root user and review your root user access keys. Then, you can pull a <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_getting-report.html credentials report> to learn which IAM user owns the keys. To learn who requested the temporary credentials for an @ASIA@ access key, view the STS events in your <https://docs.aws.amazon.com/IAM/latest/UserGuide/cloudtrail-integration.html CloudTrail logs> in the /IAM User Guide/ .
--
-- This operation does not indicate the state of the access key. The key might be active, inactive, or deleted. Active keys might not have permissions to perform an operation. Providing a deleted access key might return an error that the key doesn't exist.
module Network.AWS.STS.GetAccessKeyInfo
  ( -- * Creating a Request
    getAccessKeyInfo,
    GetAccessKeyInfo,

    -- * Request Lenses
    gakiAccessKeyId,

    -- * Destructuring the Response
    getAccessKeyInfoResponse,
    GetAccessKeyInfoResponse,

    -- * Response Lenses
    gakirsAccount,
    gakirsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.STS.Types

-- | /See:/ 'getAccessKeyInfo' smart constructor.
newtype GetAccessKeyInfo = GetAccessKeyInfo'
  { _gakiAccessKeyId ::
      AccessKey
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetAccessKeyInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gakiAccessKeyId' - The identifier of an access key. This parameter allows (through its regex pattern) a string of characters that can consist of any upper- or lowercase letter or digit.
getAccessKeyInfo ::
  -- | 'gakiAccessKeyId'
  AccessKey ->
  GetAccessKeyInfo
getAccessKeyInfo pAccessKeyId_ =
  GetAccessKeyInfo' {_gakiAccessKeyId = pAccessKeyId_}

-- | The identifier of an access key. This parameter allows (through its regex pattern) a string of characters that can consist of any upper- or lowercase letter or digit.
gakiAccessKeyId :: Lens' GetAccessKeyInfo AccessKey
gakiAccessKeyId = lens _gakiAccessKeyId (\s a -> s {_gakiAccessKeyId = a})

instance AWSRequest GetAccessKeyInfo where
  type Rs GetAccessKeyInfo = GetAccessKeyInfoResponse
  request = postQuery sts
  response =
    receiveXMLWrapper
      "GetAccessKeyInfoResult"
      ( \s h x ->
          GetAccessKeyInfoResponse'
            <$> (x .@? "Account") <*> (pure (fromEnum s))
      )

instance Hashable GetAccessKeyInfo

instance NFData GetAccessKeyInfo

instance ToHeaders GetAccessKeyInfo where
  toHeaders = const mempty

instance ToPath GetAccessKeyInfo where
  toPath = const "/"

instance ToQuery GetAccessKeyInfo where
  toQuery GetAccessKeyInfo' {..} =
    mconcat
      [ "Action" =: ("GetAccessKeyInfo" :: ByteString),
        "Version" =: ("2011-06-15" :: ByteString),
        "AccessKeyId" =: _gakiAccessKeyId
      ]

-- | /See:/ 'getAccessKeyInfoResponse' smart constructor.
data GetAccessKeyInfoResponse = GetAccessKeyInfoResponse'
  { _gakirsAccount ::
      !(Maybe Text),
    _gakirsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetAccessKeyInfoResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gakirsAccount' - The number used to identify the AWS account.
--
-- * 'gakirsResponseStatus' - -- | The response status code.
getAccessKeyInfoResponse ::
  -- | 'gakirsResponseStatus'
  Int ->
  GetAccessKeyInfoResponse
getAccessKeyInfoResponse pResponseStatus_ =
  GetAccessKeyInfoResponse'
    { _gakirsAccount = Nothing,
      _gakirsResponseStatus = pResponseStatus_
    }

-- | The number used to identify the AWS account.
gakirsAccount :: Lens' GetAccessKeyInfoResponse (Maybe Text)
gakirsAccount = lens _gakirsAccount (\s a -> s {_gakirsAccount = a})

-- | -- | The response status code.
gakirsResponseStatus :: Lens' GetAccessKeyInfoResponse Int
gakirsResponseStatus = lens _gakirsResponseStatus (\s a -> s {_gakirsResponseStatus = a})

instance NFData GetAccessKeyInfoResponse
