{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.Credentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.Credentials where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Credentials for the provided identity ID.
--
--
--
-- /See:/ 'credentials' smart constructor.
data Credentials = Credentials'
  { _cSessionToken :: !(Maybe Text),
    _cExpiration :: !(Maybe POSIX),
    _cSecretKey :: !(Maybe Text),
    _cAccessKeyId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Credentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cSessionToken' - The Session Token portion of the credentials
--
-- * 'cExpiration' - The date at which these credentials will expire.
--
-- * 'cSecretKey' - The Secret Access Key portion of the credentials
--
-- * 'cAccessKeyId' - The Access Key portion of the credentials.
credentials ::
  Credentials
credentials =
  Credentials'
    { _cSessionToken = Nothing,
      _cExpiration = Nothing,
      _cSecretKey = Nothing,
      _cAccessKeyId = Nothing
    }

-- | The Session Token portion of the credentials
cSessionToken :: Lens' Credentials (Maybe Text)
cSessionToken = lens _cSessionToken (\s a -> s {_cSessionToken = a})

-- | The date at which these credentials will expire.
cExpiration :: Lens' Credentials (Maybe UTCTime)
cExpiration = lens _cExpiration (\s a -> s {_cExpiration = a}) . mapping _Time

-- | The Secret Access Key portion of the credentials
cSecretKey :: Lens' Credentials (Maybe Text)
cSecretKey = lens _cSecretKey (\s a -> s {_cSecretKey = a})

-- | The Access Key portion of the credentials.
cAccessKeyId :: Lens' Credentials (Maybe Text)
cAccessKeyId = lens _cAccessKeyId (\s a -> s {_cAccessKeyId = a})

instance FromJSON Credentials where
  parseJSON =
    withObject
      "Credentials"
      ( \x ->
          Credentials'
            <$> (x .:? "SessionToken")
            <*> (x .:? "Expiration")
            <*> (x .:? "SecretKey")
            <*> (x .:? "AccessKeyId")
      )

instance Hashable Credentials

instance NFData Credentials
