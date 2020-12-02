{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.CognitoIdentityProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.CognitoIdentityProvider where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A provider representing an Amazon Cognito user pool and its client ID.
--
--
--
-- /See:/ 'cognitoIdentityProvider' smart constructor.
data CognitoIdentityProvider = CognitoIdentityProvider'
  { _cipClientId ::
      !(Maybe Text),
    _cipServerSideTokenCheck :: !(Maybe Bool),
    _cipProviderName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CognitoIdentityProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cipClientId' - The client ID for the Amazon Cognito user pool.
--
-- * 'cipServerSideTokenCheck' - TRUE if server-side token validation is enabled for the identity provider’s token. Once you set @ServerSideTokenCheck@ to TRUE for an identity pool, that identity pool will check with the integrated user pools to make sure that the user has not been globally signed out or deleted before the identity pool provides an OIDC token or AWS credentials for the user. If the user is signed out or deleted, the identity pool will return a 400 Not Authorized error.
--
-- * 'cipProviderName' - The provider name for an Amazon Cognito user pool. For example, @cognito-idp.us-east-1.amazonaws.com/us-east-1_123456789@ .
cognitoIdentityProvider ::
  CognitoIdentityProvider
cognitoIdentityProvider =
  CognitoIdentityProvider'
    { _cipClientId = Nothing,
      _cipServerSideTokenCheck = Nothing,
      _cipProviderName = Nothing
    }

-- | The client ID for the Amazon Cognito user pool.
cipClientId :: Lens' CognitoIdentityProvider (Maybe Text)
cipClientId = lens _cipClientId (\s a -> s {_cipClientId = a})

-- | TRUE if server-side token validation is enabled for the identity provider’s token. Once you set @ServerSideTokenCheck@ to TRUE for an identity pool, that identity pool will check with the integrated user pools to make sure that the user has not been globally signed out or deleted before the identity pool provides an OIDC token or AWS credentials for the user. If the user is signed out or deleted, the identity pool will return a 400 Not Authorized error.
cipServerSideTokenCheck :: Lens' CognitoIdentityProvider (Maybe Bool)
cipServerSideTokenCheck = lens _cipServerSideTokenCheck (\s a -> s {_cipServerSideTokenCheck = a})

-- | The provider name for an Amazon Cognito user pool. For example, @cognito-idp.us-east-1.amazonaws.com/us-east-1_123456789@ .
cipProviderName :: Lens' CognitoIdentityProvider (Maybe Text)
cipProviderName = lens _cipProviderName (\s a -> s {_cipProviderName = a})

instance FromJSON CognitoIdentityProvider where
  parseJSON =
    withObject
      "CognitoIdentityProvider"
      ( \x ->
          CognitoIdentityProvider'
            <$> (x .:? "ClientId")
            <*> (x .:? "ServerSideTokenCheck")
            <*> (x .:? "ProviderName")
      )

instance Hashable CognitoIdentityProvider

instance NFData CognitoIdentityProvider

instance ToJSON CognitoIdentityProvider where
  toJSON CognitoIdentityProvider' {..} =
    object
      ( catMaybes
          [ ("ClientId" .=) <$> _cipClientId,
            ("ServerSideTokenCheck" .=) <$> _cipServerSideTokenCheck,
            ("ProviderName" .=) <$> _cipProviderName
          ]
      )
