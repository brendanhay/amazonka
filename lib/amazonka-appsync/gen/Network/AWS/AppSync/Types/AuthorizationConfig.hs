{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.AuthorizationConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.AuthorizationConfig where

import Network.AWS.AppSync.Types.AWSIAMConfig
import Network.AWS.AppSync.Types.AuthorizationType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The authorization config in case the HTTP endpoint requires authorization.
--
--
--
-- /See:/ 'authorizationConfig' smart constructor.
data AuthorizationConfig = AuthorizationConfig'
  { _acAwsIAMConfig ::
      !(Maybe AWSIAMConfig),
    _acAuthorizationType :: !AuthorizationType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AuthorizationConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acAwsIAMConfig' - The AWS IAM settings.
--
-- * 'acAuthorizationType' - The authorization type required by the HTTP endpoint.     * __AWS_IAM__ : The authorization type is Sigv4.
authorizationConfig ::
  -- | 'acAuthorizationType'
  AuthorizationType ->
  AuthorizationConfig
authorizationConfig pAuthorizationType_ =
  AuthorizationConfig'
    { _acAwsIAMConfig = Nothing,
      _acAuthorizationType = pAuthorizationType_
    }

-- | The AWS IAM settings.
acAwsIAMConfig :: Lens' AuthorizationConfig (Maybe AWSIAMConfig)
acAwsIAMConfig = lens _acAwsIAMConfig (\s a -> s {_acAwsIAMConfig = a})

-- | The authorization type required by the HTTP endpoint.     * __AWS_IAM__ : The authorization type is Sigv4.
acAuthorizationType :: Lens' AuthorizationConfig AuthorizationType
acAuthorizationType = lens _acAuthorizationType (\s a -> s {_acAuthorizationType = a})

instance FromJSON AuthorizationConfig where
  parseJSON =
    withObject
      "AuthorizationConfig"
      ( \x ->
          AuthorizationConfig'
            <$> (x .:? "awsIamConfig") <*> (x .: "authorizationType")
      )

instance Hashable AuthorizationConfig

instance NFData AuthorizationConfig

instance ToJSON AuthorizationConfig where
  toJSON AuthorizationConfig' {..} =
    object
      ( catMaybes
          [ ("awsIamConfig" .=) <$> _acAwsIAMConfig,
            Just ("authorizationType" .= _acAuthorizationType)
          ]
      )
