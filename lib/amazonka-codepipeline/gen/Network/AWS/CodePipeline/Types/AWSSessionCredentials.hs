{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.AWSSessionCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.AWSSessionCredentials where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents an AWS session credentials object. These credentials are temporary credentials that are issued by AWS Secure Token Service (STS). They can be used to access input and output artifacts in the S3 bucket used to store artifact for the pipeline in AWS CodePipeline.
--
--
--
-- /See:/ 'awsSessionCredentials' smart constructor.
data AWSSessionCredentials = AWSSessionCredentials'
  { _ascAccessKeyId ::
      !Text,
    _ascSecretAccessKey :: !Text,
    _ascSessionToken :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'AWSSessionCredentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ascAccessKeyId' - The access key for the session.
--
-- * 'ascSecretAccessKey' - The secret access key for the session.
--
-- * 'ascSessionToken' - The token for the session.
awsSessionCredentials ::
  -- | 'ascAccessKeyId'
  Text ->
  -- | 'ascSecretAccessKey'
  Text ->
  -- | 'ascSessionToken'
  Text ->
  AWSSessionCredentials
awsSessionCredentials
  pAccessKeyId_
  pSecretAccessKey_
  pSessionToken_ =
    AWSSessionCredentials'
      { _ascAccessKeyId = pAccessKeyId_,
        _ascSecretAccessKey = pSecretAccessKey_,
        _ascSessionToken = pSessionToken_
      }

-- | The access key for the session.
ascAccessKeyId :: Lens' AWSSessionCredentials Text
ascAccessKeyId = lens _ascAccessKeyId (\s a -> s {_ascAccessKeyId = a})

-- | The secret access key for the session.
ascSecretAccessKey :: Lens' AWSSessionCredentials Text
ascSecretAccessKey = lens _ascSecretAccessKey (\s a -> s {_ascSecretAccessKey = a})

-- | The token for the session.
ascSessionToken :: Lens' AWSSessionCredentials Text
ascSessionToken = lens _ascSessionToken (\s a -> s {_ascSessionToken = a})

instance FromJSON AWSSessionCredentials where
  parseJSON =
    withObject
      "AWSSessionCredentials"
      ( \x ->
          AWSSessionCredentials'
            <$> (x .: "accessKeyId")
            <*> (x .: "secretAccessKey")
            <*> (x .: "sessionToken")
      )

instance Hashable AWSSessionCredentials

instance NFData AWSSessionCredentials
