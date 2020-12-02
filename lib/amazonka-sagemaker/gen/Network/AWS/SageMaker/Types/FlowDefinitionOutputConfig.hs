{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.FlowDefinitionOutputConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.FlowDefinitionOutputConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about where human output will be stored.
--
--
--
-- /See:/ 'flowDefinitionOutputConfig' smart constructor.
data FlowDefinitionOutputConfig = FlowDefinitionOutputConfig'
  { _fdocKMSKeyId ::
      !(Maybe Text),
    _fdocS3OutputPath :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FlowDefinitionOutputConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fdocKMSKeyId' - The Amazon Key Management Service (KMS) key ID for server-side encryption.
--
-- * 'fdocS3OutputPath' - The Amazon S3 path where the object containing human output will be made available.
flowDefinitionOutputConfig ::
  -- | 'fdocS3OutputPath'
  Text ->
  FlowDefinitionOutputConfig
flowDefinitionOutputConfig pS3OutputPath_ =
  FlowDefinitionOutputConfig'
    { _fdocKMSKeyId = Nothing,
      _fdocS3OutputPath = pS3OutputPath_
    }

-- | The Amazon Key Management Service (KMS) key ID for server-side encryption.
fdocKMSKeyId :: Lens' FlowDefinitionOutputConfig (Maybe Text)
fdocKMSKeyId = lens _fdocKMSKeyId (\s a -> s {_fdocKMSKeyId = a})

-- | The Amazon S3 path where the object containing human output will be made available.
fdocS3OutputPath :: Lens' FlowDefinitionOutputConfig Text
fdocS3OutputPath = lens _fdocS3OutputPath (\s a -> s {_fdocS3OutputPath = a})

instance FromJSON FlowDefinitionOutputConfig where
  parseJSON =
    withObject
      "FlowDefinitionOutputConfig"
      ( \x ->
          FlowDefinitionOutputConfig'
            <$> (x .:? "KmsKeyId") <*> (x .: "S3OutputPath")
      )

instance Hashable FlowDefinitionOutputConfig

instance NFData FlowDefinitionOutputConfig

instance ToJSON FlowDefinitionOutputConfig where
  toJSON FlowDefinitionOutputConfig' {..} =
    object
      ( catMaybes
          [ ("KmsKeyId" .=) <$> _fdocKMSKeyId,
            Just ("S3OutputPath" .= _fdocS3OutputPath)
          ]
      )
