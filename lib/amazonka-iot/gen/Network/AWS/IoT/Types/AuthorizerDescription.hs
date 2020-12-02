{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuthorizerDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuthorizerDescription where

import Network.AWS.IoT.Types.AuthorizerStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The authorizer description.
--
--
--
-- /See:/ 'authorizerDescription' smart constructor.
data AuthorizerDescription = AuthorizerDescription'
  { _adStatus ::
      !(Maybe AuthorizerStatus),
    _adLastModifiedDate :: !(Maybe POSIX),
    _adSigningDisabled :: !(Maybe Bool),
    _adAuthorizerName :: !(Maybe Text),
    _adAuthorizerFunctionARN :: !(Maybe Text),
    _adAuthorizerARN :: !(Maybe Text),
    _adCreationDate :: !(Maybe POSIX),
    _adTokenSigningPublicKeys ::
      !(Maybe (Map Text (Text))),
    _adTokenKeyName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AuthorizerDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adStatus' - The status of the authorizer.
--
-- * 'adLastModifiedDate' - The UNIX timestamp of when the authorizer was last updated.
--
-- * 'adSigningDisabled' - Specifies whether AWS IoT validates the token signature in an authorization request.
--
-- * 'adAuthorizerName' - The authorizer name.
--
-- * 'adAuthorizerFunctionARN' - The authorizer's Lambda function ARN.
--
-- * 'adAuthorizerARN' - The authorizer ARN.
--
-- * 'adCreationDate' - The UNIX timestamp of when the authorizer was created.
--
-- * 'adTokenSigningPublicKeys' - The public keys used to validate the token signature returned by your custom authentication service.
--
-- * 'adTokenKeyName' - The key used to extract the token from the HTTP headers.
authorizerDescription ::
  AuthorizerDescription
authorizerDescription =
  AuthorizerDescription'
    { _adStatus = Nothing,
      _adLastModifiedDate = Nothing,
      _adSigningDisabled = Nothing,
      _adAuthorizerName = Nothing,
      _adAuthorizerFunctionARN = Nothing,
      _adAuthorizerARN = Nothing,
      _adCreationDate = Nothing,
      _adTokenSigningPublicKeys = Nothing,
      _adTokenKeyName = Nothing
    }

-- | The status of the authorizer.
adStatus :: Lens' AuthorizerDescription (Maybe AuthorizerStatus)
adStatus = lens _adStatus (\s a -> s {_adStatus = a})

-- | The UNIX timestamp of when the authorizer was last updated.
adLastModifiedDate :: Lens' AuthorizerDescription (Maybe UTCTime)
adLastModifiedDate = lens _adLastModifiedDate (\s a -> s {_adLastModifiedDate = a}) . mapping _Time

-- | Specifies whether AWS IoT validates the token signature in an authorization request.
adSigningDisabled :: Lens' AuthorizerDescription (Maybe Bool)
adSigningDisabled = lens _adSigningDisabled (\s a -> s {_adSigningDisabled = a})

-- | The authorizer name.
adAuthorizerName :: Lens' AuthorizerDescription (Maybe Text)
adAuthorizerName = lens _adAuthorizerName (\s a -> s {_adAuthorizerName = a})

-- | The authorizer's Lambda function ARN.
adAuthorizerFunctionARN :: Lens' AuthorizerDescription (Maybe Text)
adAuthorizerFunctionARN = lens _adAuthorizerFunctionARN (\s a -> s {_adAuthorizerFunctionARN = a})

-- | The authorizer ARN.
adAuthorizerARN :: Lens' AuthorizerDescription (Maybe Text)
adAuthorizerARN = lens _adAuthorizerARN (\s a -> s {_adAuthorizerARN = a})

-- | The UNIX timestamp of when the authorizer was created.
adCreationDate :: Lens' AuthorizerDescription (Maybe UTCTime)
adCreationDate = lens _adCreationDate (\s a -> s {_adCreationDate = a}) . mapping _Time

-- | The public keys used to validate the token signature returned by your custom authentication service.
adTokenSigningPublicKeys :: Lens' AuthorizerDescription (HashMap Text (Text))
adTokenSigningPublicKeys = lens _adTokenSigningPublicKeys (\s a -> s {_adTokenSigningPublicKeys = a}) . _Default . _Map

-- | The key used to extract the token from the HTTP headers.
adTokenKeyName :: Lens' AuthorizerDescription (Maybe Text)
adTokenKeyName = lens _adTokenKeyName (\s a -> s {_adTokenKeyName = a})

instance FromJSON AuthorizerDescription where
  parseJSON =
    withObject
      "AuthorizerDescription"
      ( \x ->
          AuthorizerDescription'
            <$> (x .:? "status")
            <*> (x .:? "lastModifiedDate")
            <*> (x .:? "signingDisabled")
            <*> (x .:? "authorizerName")
            <*> (x .:? "authorizerFunctionArn")
            <*> (x .:? "authorizerArn")
            <*> (x .:? "creationDate")
            <*> (x .:? "tokenSigningPublicKeys" .!= mempty)
            <*> (x .:? "tokenKeyName")
      )

instance Hashable AuthorizerDescription

instance NFData AuthorizerDescription
