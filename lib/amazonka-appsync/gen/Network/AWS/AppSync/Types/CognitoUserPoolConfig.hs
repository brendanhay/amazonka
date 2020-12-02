{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.CognitoUserPoolConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.CognitoUserPoolConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an Amazon Cognito user pool configuration.
--
--
--
-- /See:/ 'cognitoUserPoolConfig' smart constructor.
data CognitoUserPoolConfig = CognitoUserPoolConfig'
  { _cupcAppIdClientRegex ::
      !(Maybe Text),
    _cupcUserPoolId :: !Text,
    _cupcAwsRegion :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CognitoUserPoolConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cupcAppIdClientRegex' - A regular expression for validating the incoming Amazon Cognito user pool app client ID.
--
-- * 'cupcUserPoolId' - The user pool ID.
--
-- * 'cupcAwsRegion' - The AWS Region in which the user pool was created.
cognitoUserPoolConfig ::
  -- | 'cupcUserPoolId'
  Text ->
  -- | 'cupcAwsRegion'
  Text ->
  CognitoUserPoolConfig
cognitoUserPoolConfig pUserPoolId_ pAwsRegion_ =
  CognitoUserPoolConfig'
    { _cupcAppIdClientRegex = Nothing,
      _cupcUserPoolId = pUserPoolId_,
      _cupcAwsRegion = pAwsRegion_
    }

-- | A regular expression for validating the incoming Amazon Cognito user pool app client ID.
cupcAppIdClientRegex :: Lens' CognitoUserPoolConfig (Maybe Text)
cupcAppIdClientRegex = lens _cupcAppIdClientRegex (\s a -> s {_cupcAppIdClientRegex = a})

-- | The user pool ID.
cupcUserPoolId :: Lens' CognitoUserPoolConfig Text
cupcUserPoolId = lens _cupcUserPoolId (\s a -> s {_cupcUserPoolId = a})

-- | The AWS Region in which the user pool was created.
cupcAwsRegion :: Lens' CognitoUserPoolConfig Text
cupcAwsRegion = lens _cupcAwsRegion (\s a -> s {_cupcAwsRegion = a})

instance FromJSON CognitoUserPoolConfig where
  parseJSON =
    withObject
      "CognitoUserPoolConfig"
      ( \x ->
          CognitoUserPoolConfig'
            <$> (x .:? "appIdClientRegex")
            <*> (x .: "userPoolId")
            <*> (x .: "awsRegion")
      )

instance Hashable CognitoUserPoolConfig

instance NFData CognitoUserPoolConfig

instance ToJSON CognitoUserPoolConfig where
  toJSON CognitoUserPoolConfig' {..} =
    object
      ( catMaybes
          [ ("appIdClientRegex" .=) <$> _cupcAppIdClientRegex,
            Just ("userPoolId" .= _cupcUserPoolId),
            Just ("awsRegion" .= _cupcAwsRegion)
          ]
      )
