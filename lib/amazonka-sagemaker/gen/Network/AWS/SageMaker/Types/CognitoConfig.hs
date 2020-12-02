{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CognitoConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CognitoConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Use this parameter to configure your Amazon Cognito workforce. A single Cognito workforce is created using and corresponds to a single <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool> .
--
--
--
-- /See:/ 'cognitoConfig' smart constructor.
data CognitoConfig = CognitoConfig'
  { _ccUserPool :: !Text,
    _ccClientId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CognitoConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccUserPool' - A <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html user pool> is a user directory in Amazon Cognito. With a user pool, your users can sign in to your web or mobile app through Amazon Cognito. Your users can also sign in through social identity providers like Google, Facebook, Amazon, or Apple, and through SAML identity providers.
--
-- * 'ccClientId' - The client ID for your Amazon Cognito user pool.
cognitoConfig ::
  -- | 'ccUserPool'
  Text ->
  -- | 'ccClientId'
  Text ->
  CognitoConfig
cognitoConfig pUserPool_ pClientId_ =
  CognitoConfig'
    { _ccUserPool = pUserPool_,
      _ccClientId = pClientId_
    }

-- | A <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html user pool> is a user directory in Amazon Cognito. With a user pool, your users can sign in to your web or mobile app through Amazon Cognito. Your users can also sign in through social identity providers like Google, Facebook, Amazon, or Apple, and through SAML identity providers.
ccUserPool :: Lens' CognitoConfig Text
ccUserPool = lens _ccUserPool (\s a -> s {_ccUserPool = a})

-- | The client ID for your Amazon Cognito user pool.
ccClientId :: Lens' CognitoConfig Text
ccClientId = lens _ccClientId (\s a -> s {_ccClientId = a})

instance FromJSON CognitoConfig where
  parseJSON =
    withObject
      "CognitoConfig"
      (\x -> CognitoConfig' <$> (x .: "UserPool") <*> (x .: "ClientId"))

instance Hashable CognitoConfig

instance NFData CognitoConfig

instance ToJSON CognitoConfig where
  toJSON CognitoConfig' {..} =
    object
      ( catMaybes
          [ Just ("UserPool" .= _ccUserPool),
            Just ("ClientId" .= _ccClientId)
          ]
      )
