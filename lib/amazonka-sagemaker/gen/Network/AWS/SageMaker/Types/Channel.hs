{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Channel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Channel where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.CompressionType
import Network.AWS.SageMaker.Types.DataSource
import Network.AWS.SageMaker.Types.RecordWrapper
import Network.AWS.SageMaker.Types.ShuffleConfig
import Network.AWS.SageMaker.Types.TrainingInputMode

-- | A channel is a named input source that training algorithms can consume.
--
--
--
-- /See:/ 'channel' smart constructor.
data Channel = Channel'
  { _cShuffleConfig :: !(Maybe ShuffleConfig),
    _cRecordWrapperType :: !(Maybe RecordWrapper),
    _cInputMode :: !(Maybe TrainingInputMode),
    _cCompressionType :: !(Maybe CompressionType),
    _cContentType :: !(Maybe Text),
    _cChannelName :: !Text,
    _cDataSource :: !DataSource
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Channel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cShuffleConfig' - A configuration for a shuffle option for input data in a channel. If you use @S3Prefix@ for @S3DataType@ , this shuffles the results of the S3 key prefix matches. If you use @ManifestFile@ , the order of the S3 object references in the @ManifestFile@ is shuffled. If you use @AugmentedManifestFile@ , the order of the JSON lines in the @AugmentedManifestFile@ is shuffled. The shuffling order is determined using the @Seed@ value. For Pipe input mode, shuffling is done at the start of every epoch. With large datasets this ensures that the order of the training data is different for each epoch, it helps reduce bias and possible overfitting. In a multi-node training job when ShuffleConfig is combined with @S3DataDistributionType@ of @ShardedByS3Key@ , the data is shuffled across nodes so that the content sent to a particular node on the first epoch might be sent to a different node on the second epoch.
--
-- * 'cRecordWrapperType' -  Specify RecordIO as the value when input data is in raw format but the training algorithm requires the RecordIO format. In this case, Amazon SageMaker wraps each individual S3 object in a RecordIO record. If the input data is already in RecordIO format, you don't need to set this attribute. For more information, see <https://mxnet.apache.org/api/architecture/note_data_loading#data-format Create a Dataset Using RecordIO> .  In File mode, leave this field unset or set it to None.
--
-- * 'cInputMode' - (Optional) The input mode to use for the data channel in a training job. If you don't set a value for @InputMode@ , Amazon SageMaker uses the value set for @TrainingInputMode@ . Use this parameter to override the @TrainingInputMode@ setting in a 'AlgorithmSpecification' request when you have a channel that needs a different input mode from the training job's general setting. To download the data from Amazon Simple Storage Service (Amazon S3) to the provisioned ML storage volume, and mount the directory to a Docker volume, use @File@ input mode. To stream data directly from Amazon S3 to the container, choose @Pipe@ input mode. To use a model for incremental training, choose @File@ input model.
--
-- * 'cCompressionType' - If training data is compressed, the compression type. The default value is @None@ . @CompressionType@ is used only in Pipe input mode. In File mode, leave this field unset or set it to None.
--
-- * 'cContentType' - The MIME type of the data.
--
-- * 'cChannelName' - The name of the channel.
--
-- * 'cDataSource' - The location of the channel data.
channel ::
  -- | 'cChannelName'
  Text ->
  -- | 'cDataSource'
  DataSource ->
  Channel
channel pChannelName_ pDataSource_ =
  Channel'
    { _cShuffleConfig = Nothing,
      _cRecordWrapperType = Nothing,
      _cInputMode = Nothing,
      _cCompressionType = Nothing,
      _cContentType = Nothing,
      _cChannelName = pChannelName_,
      _cDataSource = pDataSource_
    }

-- | A configuration for a shuffle option for input data in a channel. If you use @S3Prefix@ for @S3DataType@ , this shuffles the results of the S3 key prefix matches. If you use @ManifestFile@ , the order of the S3 object references in the @ManifestFile@ is shuffled. If you use @AugmentedManifestFile@ , the order of the JSON lines in the @AugmentedManifestFile@ is shuffled. The shuffling order is determined using the @Seed@ value. For Pipe input mode, shuffling is done at the start of every epoch. With large datasets this ensures that the order of the training data is different for each epoch, it helps reduce bias and possible overfitting. In a multi-node training job when ShuffleConfig is combined with @S3DataDistributionType@ of @ShardedByS3Key@ , the data is shuffled across nodes so that the content sent to a particular node on the first epoch might be sent to a different node on the second epoch.
cShuffleConfig :: Lens' Channel (Maybe ShuffleConfig)
cShuffleConfig = lens _cShuffleConfig (\s a -> s {_cShuffleConfig = a})

-- |  Specify RecordIO as the value when input data is in raw format but the training algorithm requires the RecordIO format. In this case, Amazon SageMaker wraps each individual S3 object in a RecordIO record. If the input data is already in RecordIO format, you don't need to set this attribute. For more information, see <https://mxnet.apache.org/api/architecture/note_data_loading#data-format Create a Dataset Using RecordIO> .  In File mode, leave this field unset or set it to None.
cRecordWrapperType :: Lens' Channel (Maybe RecordWrapper)
cRecordWrapperType = lens _cRecordWrapperType (\s a -> s {_cRecordWrapperType = a})

-- | (Optional) The input mode to use for the data channel in a training job. If you don't set a value for @InputMode@ , Amazon SageMaker uses the value set for @TrainingInputMode@ . Use this parameter to override the @TrainingInputMode@ setting in a 'AlgorithmSpecification' request when you have a channel that needs a different input mode from the training job's general setting. To download the data from Amazon Simple Storage Service (Amazon S3) to the provisioned ML storage volume, and mount the directory to a Docker volume, use @File@ input mode. To stream data directly from Amazon S3 to the container, choose @Pipe@ input mode. To use a model for incremental training, choose @File@ input model.
cInputMode :: Lens' Channel (Maybe TrainingInputMode)
cInputMode = lens _cInputMode (\s a -> s {_cInputMode = a})

-- | If training data is compressed, the compression type. The default value is @None@ . @CompressionType@ is used only in Pipe input mode. In File mode, leave this field unset or set it to None.
cCompressionType :: Lens' Channel (Maybe CompressionType)
cCompressionType = lens _cCompressionType (\s a -> s {_cCompressionType = a})

-- | The MIME type of the data.
cContentType :: Lens' Channel (Maybe Text)
cContentType = lens _cContentType (\s a -> s {_cContentType = a})

-- | The name of the channel.
cChannelName :: Lens' Channel Text
cChannelName = lens _cChannelName (\s a -> s {_cChannelName = a})

-- | The location of the channel data.
cDataSource :: Lens' Channel DataSource
cDataSource = lens _cDataSource (\s a -> s {_cDataSource = a})

instance FromJSON Channel where
  parseJSON =
    withObject
      "Channel"
      ( \x ->
          Channel'
            <$> (x .:? "ShuffleConfig")
            <*> (x .:? "RecordWrapperType")
            <*> (x .:? "InputMode")
            <*> (x .:? "CompressionType")
            <*> (x .:? "ContentType")
            <*> (x .: "ChannelName")
            <*> (x .: "DataSource")
      )

instance Hashable Channel

instance NFData Channel

instance ToJSON Channel where
  toJSON Channel' {..} =
    object
      ( catMaybes
          [ ("ShuffleConfig" .=) <$> _cShuffleConfig,
            ("RecordWrapperType" .=) <$> _cRecordWrapperType,
            ("InputMode" .=) <$> _cInputMode,
            ("CompressionType" .=) <$> _cCompressionType,
            ("ContentType" .=) <$> _cContentType,
            Just ("ChannelName" .= _cChannelName),
            Just ("DataSource" .= _cDataSource)
          ]
      )
