{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.AuthorizationData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.AuthorizationData where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing authorization data for an Amazon ECR registry.
--
--
--
-- /See:/ 'authorizationData' smart constructor.
data AuthorizationData = AuthorizationData'
  { _adExpiresAt ::
      !(Maybe POSIX),
    _adProxyEndpoint :: !(Maybe Text),
    _adAuthorizationToken :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AuthorizationData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adExpiresAt' - The Unix time in seconds and milliseconds when the authorization token expires. Authorization tokens are valid for 12 hours.
--
-- * 'adProxyEndpoint' - The registry URL to use for this authorization token in a @docker login@ command. The Amazon ECR registry URL format is @https://aws_account_id.dkr.ecr.region.amazonaws.com@ . For example, @https://012345678910.dkr.ecr.us-east-1.amazonaws.com@ ..
--
-- * 'adAuthorizationToken' - A base64-encoded string that contains authorization data for the specified Amazon ECR registry. When the string is decoded, it is presented in the format @user:password@ for private registry authentication using @docker login@ .
authorizationData ::
  AuthorizationData
authorizationData =
  AuthorizationData'
    { _adExpiresAt = Nothing,
      _adProxyEndpoint = Nothing,
      _adAuthorizationToken = Nothing
    }

-- | The Unix time in seconds and milliseconds when the authorization token expires. Authorization tokens are valid for 12 hours.
adExpiresAt :: Lens' AuthorizationData (Maybe UTCTime)
adExpiresAt = lens _adExpiresAt (\s a -> s {_adExpiresAt = a}) . mapping _Time

-- | The registry URL to use for this authorization token in a @docker login@ command. The Amazon ECR registry URL format is @https://aws_account_id.dkr.ecr.region.amazonaws.com@ . For example, @https://012345678910.dkr.ecr.us-east-1.amazonaws.com@ ..
adProxyEndpoint :: Lens' AuthorizationData (Maybe Text)
adProxyEndpoint = lens _adProxyEndpoint (\s a -> s {_adProxyEndpoint = a})

-- | A base64-encoded string that contains authorization data for the specified Amazon ECR registry. When the string is decoded, it is presented in the format @user:password@ for private registry authentication using @docker login@ .
adAuthorizationToken :: Lens' AuthorizationData (Maybe Text)
adAuthorizationToken = lens _adAuthorizationToken (\s a -> s {_adAuthorizationToken = a})

instance FromJSON AuthorizationData where
  parseJSON =
    withObject
      "AuthorizationData"
      ( \x ->
          AuthorizationData'
            <$> (x .:? "expiresAt")
            <*> (x .:? "proxyEndpoint")
            <*> (x .:? "authorizationToken")
      )

instance Hashable AuthorizationData

instance NFData AuthorizationData
