{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.UserPoolConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.UserPoolConfig where

import Network.AWS.AppSync.Types.DefaultAction
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an Amazon Cognito user pool configuration.
--
--
--
-- /See:/ 'userPoolConfig' smart constructor.
data UserPoolConfig = UserPoolConfig'
  { _upcAppIdClientRegex ::
      !(Maybe Text),
    _upcUserPoolId :: !Text,
    _upcAwsRegion :: !Text,
    _upcDefaultAction :: !DefaultAction
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserPoolConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upcAppIdClientRegex' - A regular expression for validating the incoming Amazon Cognito user pool app client ID.
--
-- * 'upcUserPoolId' - The user pool ID.
--
-- * 'upcAwsRegion' - The AWS Region in which the user pool was created.
--
-- * 'upcDefaultAction' - The action that you want your GraphQL API to take when a request that uses Amazon Cognito user pool authentication doesn't match the Amazon Cognito user pool configuration.
userPoolConfig ::
  -- | 'upcUserPoolId'
  Text ->
  -- | 'upcAwsRegion'
  Text ->
  -- | 'upcDefaultAction'
  DefaultAction ->
  UserPoolConfig
userPoolConfig pUserPoolId_ pAwsRegion_ pDefaultAction_ =
  UserPoolConfig'
    { _upcAppIdClientRegex = Nothing,
      _upcUserPoolId = pUserPoolId_,
      _upcAwsRegion = pAwsRegion_,
      _upcDefaultAction = pDefaultAction_
    }

-- | A regular expression for validating the incoming Amazon Cognito user pool app client ID.
upcAppIdClientRegex :: Lens' UserPoolConfig (Maybe Text)
upcAppIdClientRegex = lens _upcAppIdClientRegex (\s a -> s {_upcAppIdClientRegex = a})

-- | The user pool ID.
upcUserPoolId :: Lens' UserPoolConfig Text
upcUserPoolId = lens _upcUserPoolId (\s a -> s {_upcUserPoolId = a})

-- | The AWS Region in which the user pool was created.
upcAwsRegion :: Lens' UserPoolConfig Text
upcAwsRegion = lens _upcAwsRegion (\s a -> s {_upcAwsRegion = a})

-- | The action that you want your GraphQL API to take when a request that uses Amazon Cognito user pool authentication doesn't match the Amazon Cognito user pool configuration.
upcDefaultAction :: Lens' UserPoolConfig DefaultAction
upcDefaultAction = lens _upcDefaultAction (\s a -> s {_upcDefaultAction = a})

instance FromJSON UserPoolConfig where
  parseJSON =
    withObject
      "UserPoolConfig"
      ( \x ->
          UserPoolConfig'
            <$> (x .:? "appIdClientRegex")
            <*> (x .: "userPoolId")
            <*> (x .: "awsRegion")
            <*> (x .: "defaultAction")
      )

instance Hashable UserPoolConfig

instance NFData UserPoolConfig

instance ToJSON UserPoolConfig where
  toJSON UserPoolConfig' {..} =
    object
      ( catMaybes
          [ ("appIdClientRegex" .=) <$> _upcAppIdClientRegex,
            Just ("userPoolId" .= _upcUserPoolId),
            Just ("awsRegion" .= _upcAwsRegion),
            Just ("defaultAction" .= _upcDefaultAction)
          ]
      )
