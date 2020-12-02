{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.AWSCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.AWSCredentials where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Temporary access credentials used for uploading game build files to Amazon GameLift. They are valid for a limited time. If they expire before you upload your game build, get a new set by calling 'RequestUploadCredentials' .
--
--
--
-- /See:/ 'awsCredentials' smart constructor.
data AWSCredentials = AWSCredentials'
  { _acSecretAccessKey ::
      !(Maybe Text),
    _acSessionToken :: !(Maybe Text),
    _acAccessKeyId :: !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'AWSCredentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acSecretAccessKey' - Temporary secret key allowing access to the Amazon GameLift S3 account.
--
-- * 'acSessionToken' - Token used to associate a specific build ID with the files uploaded using these credentials.
--
-- * 'acAccessKeyId' - Temporary key allowing access to the Amazon GameLift S3 account.
awsCredentials ::
  AWSCredentials
awsCredentials =
  AWSCredentials'
    { _acSecretAccessKey = Nothing,
      _acSessionToken = Nothing,
      _acAccessKeyId = Nothing
    }

-- | Temporary secret key allowing access to the Amazon GameLift S3 account.
acSecretAccessKey :: Lens' AWSCredentials (Maybe Text)
acSecretAccessKey = lens _acSecretAccessKey (\s a -> s {_acSecretAccessKey = a})

-- | Token used to associate a specific build ID with the files uploaded using these credentials.
acSessionToken :: Lens' AWSCredentials (Maybe Text)
acSessionToken = lens _acSessionToken (\s a -> s {_acSessionToken = a})

-- | Temporary key allowing access to the Amazon GameLift S3 account.
acAccessKeyId :: Lens' AWSCredentials (Maybe Text)
acAccessKeyId = lens _acAccessKeyId (\s a -> s {_acAccessKeyId = a})

instance FromJSON AWSCredentials where
  parseJSON =
    withObject
      "AWSCredentials"
      ( \x ->
          AWSCredentials'
            <$> (x .:? "SecretAccessKey")
            <*> (x .:? "SessionToken")
            <*> (x .:? "AccessKeyId")
      )

instance Hashable AWSCredentials

instance NFData AWSCredentials
