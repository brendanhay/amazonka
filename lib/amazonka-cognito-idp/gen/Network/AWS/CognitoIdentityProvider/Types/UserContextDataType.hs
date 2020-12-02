{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserContextDataType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserContextDataType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
--
--
-- /See:/ 'userContextDataType' smart constructor.
newtype UserContextDataType = UserContextDataType'
  { _ucdtEncodedData ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserContextDataType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucdtEncodedData' - Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
userContextDataType ::
  UserContextDataType
userContextDataType =
  UserContextDataType' {_ucdtEncodedData = Nothing}

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
ucdtEncodedData :: Lens' UserContextDataType (Maybe Text)
ucdtEncodedData = lens _ucdtEncodedData (\s a -> s {_ucdtEncodedData = a})

instance Hashable UserContextDataType

instance NFData UserContextDataType

instance ToJSON UserContextDataType where
  toJSON UserContextDataType' {..} =
    object (catMaybes [("EncodedData" .=) <$> _ucdtEncodedData])
