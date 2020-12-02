{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.InstanceStorageConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.InstanceStorageConfig where

import Network.AWS.Connect.Types.KinesisFirehoseConfig
import Network.AWS.Connect.Types.KinesisStreamConfig
import Network.AWS.Connect.Types.KinesisVideoStreamConfig
import Network.AWS.Connect.Types.S3Config
import Network.AWS.Connect.Types.StorageType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The storage configuration for the instance.
--
--
--
-- /See:/ 'instanceStorageConfig' smart constructor.
data InstanceStorageConfig = InstanceStorageConfig'
  { _iscAssociationId ::
      !(Maybe Text),
    _iscKinesisStreamConfig ::
      !(Maybe KinesisStreamConfig),
    _iscKinesisVideoStreamConfig ::
      !(Maybe KinesisVideoStreamConfig),
    _iscS3Config :: !(Maybe S3Config),
    _iscKinesisFirehoseConfig ::
      !(Maybe KinesisFirehoseConfig),
    _iscStorageType :: !StorageType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceStorageConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iscAssociationId' - The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
--
-- * 'iscKinesisStreamConfig' - The configuration of the Kinesis data stream.
--
-- * 'iscKinesisVideoStreamConfig' - The configuration of the Kinesis video stream.
--
-- * 'iscS3Config' - The S3 configuration.
--
-- * 'iscKinesisFirehoseConfig' - The configuration of the Kinesis Firehose delivery stream.
--
-- * 'iscStorageType' - A valid storage type.
instanceStorageConfig ::
  -- | 'iscStorageType'
  StorageType ->
  InstanceStorageConfig
instanceStorageConfig pStorageType_ =
  InstanceStorageConfig'
    { _iscAssociationId = Nothing,
      _iscKinesisStreamConfig = Nothing,
      _iscKinesisVideoStreamConfig = Nothing,
      _iscS3Config = Nothing,
      _iscKinesisFirehoseConfig = Nothing,
      _iscStorageType = pStorageType_
    }

-- | The existing association identifier that uniquely identifies the resource type and storage config for the given instance ID.
iscAssociationId :: Lens' InstanceStorageConfig (Maybe Text)
iscAssociationId = lens _iscAssociationId (\s a -> s {_iscAssociationId = a})

-- | The configuration of the Kinesis data stream.
iscKinesisStreamConfig :: Lens' InstanceStorageConfig (Maybe KinesisStreamConfig)
iscKinesisStreamConfig = lens _iscKinesisStreamConfig (\s a -> s {_iscKinesisStreamConfig = a})

-- | The configuration of the Kinesis video stream.
iscKinesisVideoStreamConfig :: Lens' InstanceStorageConfig (Maybe KinesisVideoStreamConfig)
iscKinesisVideoStreamConfig = lens _iscKinesisVideoStreamConfig (\s a -> s {_iscKinesisVideoStreamConfig = a})

-- | The S3 configuration.
iscS3Config :: Lens' InstanceStorageConfig (Maybe S3Config)
iscS3Config = lens _iscS3Config (\s a -> s {_iscS3Config = a})

-- | The configuration of the Kinesis Firehose delivery stream.
iscKinesisFirehoseConfig :: Lens' InstanceStorageConfig (Maybe KinesisFirehoseConfig)
iscKinesisFirehoseConfig = lens _iscKinesisFirehoseConfig (\s a -> s {_iscKinesisFirehoseConfig = a})

-- | A valid storage type.
iscStorageType :: Lens' InstanceStorageConfig StorageType
iscStorageType = lens _iscStorageType (\s a -> s {_iscStorageType = a})

instance FromJSON InstanceStorageConfig where
  parseJSON =
    withObject
      "InstanceStorageConfig"
      ( \x ->
          InstanceStorageConfig'
            <$> (x .:? "AssociationId")
            <*> (x .:? "KinesisStreamConfig")
            <*> (x .:? "KinesisVideoStreamConfig")
            <*> (x .:? "S3Config")
            <*> (x .:? "KinesisFirehoseConfig")
            <*> (x .: "StorageType")
      )

instance Hashable InstanceStorageConfig

instance NFData InstanceStorageConfig

instance ToJSON InstanceStorageConfig where
  toJSON InstanceStorageConfig' {..} =
    object
      ( catMaybes
          [ ("AssociationId" .=) <$> _iscAssociationId,
            ("KinesisStreamConfig" .=) <$> _iscKinesisStreamConfig,
            ("KinesisVideoStreamConfig" .=) <$> _iscKinesisVideoStreamConfig,
            ("S3Config" .=) <$> _iscS3Config,
            ("KinesisFirehoseConfig" .=) <$> _iscKinesisFirehoseConfig,
            Just ("StorageType" .= _iscStorageType)
          ]
      )
