{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ContextDataType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.ContextDataType where

import Network.AWS.CognitoIdentityProvider.Types.HTTPHeader
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contextual user data type used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
--
--
-- /See:/ 'contextDataType' smart constructor.
data ContextDataType = ContextDataType'
  { _cdtEncodedData ::
      !(Maybe Text),
    _cdtIPAddress :: !Text,
    _cdtServerName :: !Text,
    _cdtServerPath :: !Text,
    _cdtHTTPHeaders :: ![HTTPHeader]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContextDataType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdtEncodedData' - Encoded data containing device fingerprinting details, collected using the Amazon Cognito context data collection library.
--
-- * 'cdtIPAddress' - Source IP address of your user.
--
-- * 'cdtServerName' - Your server endpoint where this API is invoked.
--
-- * 'cdtServerPath' - Your server path where this API is invoked.
--
-- * 'cdtHTTPHeaders' - HttpHeaders received on your server in same order.
contextDataType ::
  -- | 'cdtIPAddress'
  Text ->
  -- | 'cdtServerName'
  Text ->
  -- | 'cdtServerPath'
  Text ->
  ContextDataType
contextDataType pIPAddress_ pServerName_ pServerPath_ =
  ContextDataType'
    { _cdtEncodedData = Nothing,
      _cdtIPAddress = pIPAddress_,
      _cdtServerName = pServerName_,
      _cdtServerPath = pServerPath_,
      _cdtHTTPHeaders = mempty
    }

-- | Encoded data containing device fingerprinting details, collected using the Amazon Cognito context data collection library.
cdtEncodedData :: Lens' ContextDataType (Maybe Text)
cdtEncodedData = lens _cdtEncodedData (\s a -> s {_cdtEncodedData = a})

-- | Source IP address of your user.
cdtIPAddress :: Lens' ContextDataType Text
cdtIPAddress = lens _cdtIPAddress (\s a -> s {_cdtIPAddress = a})

-- | Your server endpoint where this API is invoked.
cdtServerName :: Lens' ContextDataType Text
cdtServerName = lens _cdtServerName (\s a -> s {_cdtServerName = a})

-- | Your server path where this API is invoked.
cdtServerPath :: Lens' ContextDataType Text
cdtServerPath = lens _cdtServerPath (\s a -> s {_cdtServerPath = a})

-- | HttpHeaders received on your server in same order.
cdtHTTPHeaders :: Lens' ContextDataType [HTTPHeader]
cdtHTTPHeaders = lens _cdtHTTPHeaders (\s a -> s {_cdtHTTPHeaders = a}) . _Coerce

instance Hashable ContextDataType

instance NFData ContextDataType

instance ToJSON ContextDataType where
  toJSON ContextDataType' {..} =
    object
      ( catMaybes
          [ ("EncodedData" .=) <$> _cdtEncodedData,
            Just ("IpAddress" .= _cdtIPAddress),
            Just ("ServerName" .= _cdtServerName),
            Just ("ServerPath" .= _cdtServerPath),
            Just ("HttpHeaders" .= _cdtHTTPHeaders)
          ]
      )
