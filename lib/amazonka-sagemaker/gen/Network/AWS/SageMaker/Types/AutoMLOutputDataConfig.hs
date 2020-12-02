{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLOutputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLOutputDataConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The output data configuration.
--
--
--
-- /See:/ 'autoMLOutputDataConfig' smart constructor.
data AutoMLOutputDataConfig = AutoMLOutputDataConfig'
  { _amlodcKMSKeyId ::
      !(Maybe Text),
    _amlodcS3OutputPath :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoMLOutputDataConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amlodcKMSKeyId' - The AWS KMS encryption key ID.
--
-- * 'amlodcS3OutputPath' - The Amazon S3 output path. Must be 128 characters or less.
autoMLOutputDataConfig ::
  -- | 'amlodcS3OutputPath'
  Text ->
  AutoMLOutputDataConfig
autoMLOutputDataConfig pS3OutputPath_ =
  AutoMLOutputDataConfig'
    { _amlodcKMSKeyId = Nothing,
      _amlodcS3OutputPath = pS3OutputPath_
    }

-- | The AWS KMS encryption key ID.
amlodcKMSKeyId :: Lens' AutoMLOutputDataConfig (Maybe Text)
amlodcKMSKeyId = lens _amlodcKMSKeyId (\s a -> s {_amlodcKMSKeyId = a})

-- | The Amazon S3 output path. Must be 128 characters or less.
amlodcS3OutputPath :: Lens' AutoMLOutputDataConfig Text
amlodcS3OutputPath = lens _amlodcS3OutputPath (\s a -> s {_amlodcS3OutputPath = a})

instance FromJSON AutoMLOutputDataConfig where
  parseJSON =
    withObject
      "AutoMLOutputDataConfig"
      ( \x ->
          AutoMLOutputDataConfig'
            <$> (x .:? "KmsKeyId") <*> (x .: "S3OutputPath")
      )

instance Hashable AutoMLOutputDataConfig

instance NFData AutoMLOutputDataConfig

instance ToJSON AutoMLOutputDataConfig where
  toJSON AutoMLOutputDataConfig' {..} =
    object
      ( catMaybes
          [ ("KmsKeyId" .=) <$> _amlodcKMSKeyId,
            Just ("S3OutputPath" .= _amlodcS3OutputPath)
          ]
      )
