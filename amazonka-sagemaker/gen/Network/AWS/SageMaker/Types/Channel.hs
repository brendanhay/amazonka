{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Channel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Channel where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.CompressionType
import Network.AWS.SageMaker.Types.DataSource
import Network.AWS.SageMaker.Types.RecordWrapper
import Network.AWS.SageMaker.Types.ShuffleConfig
import Network.AWS.SageMaker.Types.TrainingInputMode

-- | A channel is a named input source that training algorithms can consume.
--
-- /See:/ 'newChannel' smart constructor.
data Channel = Channel'
  { -- | The MIME type of the data.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | Specify RecordIO as the value when input data is in raw format but the
    -- training algorithm requires the RecordIO format. In this case, Amazon
    -- SageMaker wraps each individual S3 object in a RecordIO record. If the
    -- input data is already in RecordIO format, you don\'t need to set this
    -- attribute. For more information, see
    -- <https://mxnet.apache.org/api/architecture/note_data_loading#data-format Create a Dataset Using RecordIO>.
    --
    -- In File mode, leave this field unset or set it to None.
    recordWrapperType :: Prelude.Maybe RecordWrapper,
    -- | A configuration for a shuffle option for input data in a channel. If you
    -- use @S3Prefix@ for @S3DataType@, this shuffles the results of the S3 key
    -- prefix matches. If you use @ManifestFile@, the order of the S3 object
    -- references in the @ManifestFile@ is shuffled. If you use
    -- @AugmentedManifestFile@, the order of the JSON lines in the
    -- @AugmentedManifestFile@ is shuffled. The shuffling order is determined
    -- using the @Seed@ value.
    --
    -- For Pipe input mode, shuffling is done at the start of every epoch. With
    -- large datasets this ensures that the order of the training data is
    -- different for each epoch, it helps reduce bias and possible overfitting.
    -- In a multi-node training job when ShuffleConfig is combined with
    -- @S3DataDistributionType@ of @ShardedByS3Key@, the data is shuffled
    -- across nodes so that the content sent to a particular node on the first
    -- epoch might be sent to a different node on the second epoch.
    shuffleConfig :: Prelude.Maybe ShuffleConfig,
    -- | If training data is compressed, the compression type. The default value
    -- is @None@. @CompressionType@ is used only in Pipe input mode. In File
    -- mode, leave this field unset or set it to None.
    compressionType :: Prelude.Maybe CompressionType,
    -- | (Optional) The input mode to use for the data channel in a training job.
    -- If you don\'t set a value for @InputMode@, Amazon SageMaker uses the
    -- value set for @TrainingInputMode@. Use this parameter to override the
    -- @TrainingInputMode@ setting in a AlgorithmSpecification request when you
    -- have a channel that needs a different input mode from the training
    -- job\'s general setting. To download the data from Amazon Simple Storage
    -- Service (Amazon S3) to the provisioned ML storage volume, and mount the
    -- directory to a Docker volume, use @File@ input mode. To stream data
    -- directly from Amazon S3 to the container, choose @Pipe@ input mode.
    --
    -- To use a model for incremental training, choose @File@ input model.
    inputMode :: Prelude.Maybe TrainingInputMode,
    -- | The name of the channel.
    channelName :: Prelude.Text,
    -- | The location of the channel data.
    dataSource :: DataSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Channel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'channel_contentType' - The MIME type of the data.
--
-- 'recordWrapperType', 'channel_recordWrapperType' - Specify RecordIO as the value when input data is in raw format but the
-- training algorithm requires the RecordIO format. In this case, Amazon
-- SageMaker wraps each individual S3 object in a RecordIO record. If the
-- input data is already in RecordIO format, you don\'t need to set this
-- attribute. For more information, see
-- <https://mxnet.apache.org/api/architecture/note_data_loading#data-format Create a Dataset Using RecordIO>.
--
-- In File mode, leave this field unset or set it to None.
--
-- 'shuffleConfig', 'channel_shuffleConfig' - A configuration for a shuffle option for input data in a channel. If you
-- use @S3Prefix@ for @S3DataType@, this shuffles the results of the S3 key
-- prefix matches. If you use @ManifestFile@, the order of the S3 object
-- references in the @ManifestFile@ is shuffled. If you use
-- @AugmentedManifestFile@, the order of the JSON lines in the
-- @AugmentedManifestFile@ is shuffled. The shuffling order is determined
-- using the @Seed@ value.
--
-- For Pipe input mode, shuffling is done at the start of every epoch. With
-- large datasets this ensures that the order of the training data is
-- different for each epoch, it helps reduce bias and possible overfitting.
-- In a multi-node training job when ShuffleConfig is combined with
-- @S3DataDistributionType@ of @ShardedByS3Key@, the data is shuffled
-- across nodes so that the content sent to a particular node on the first
-- epoch might be sent to a different node on the second epoch.
--
-- 'compressionType', 'channel_compressionType' - If training data is compressed, the compression type. The default value
-- is @None@. @CompressionType@ is used only in Pipe input mode. In File
-- mode, leave this field unset or set it to None.
--
-- 'inputMode', 'channel_inputMode' - (Optional) The input mode to use for the data channel in a training job.
-- If you don\'t set a value for @InputMode@, Amazon SageMaker uses the
-- value set for @TrainingInputMode@. Use this parameter to override the
-- @TrainingInputMode@ setting in a AlgorithmSpecification request when you
-- have a channel that needs a different input mode from the training
-- job\'s general setting. To download the data from Amazon Simple Storage
-- Service (Amazon S3) to the provisioned ML storage volume, and mount the
-- directory to a Docker volume, use @File@ input mode. To stream data
-- directly from Amazon S3 to the container, choose @Pipe@ input mode.
--
-- To use a model for incremental training, choose @File@ input model.
--
-- 'channelName', 'channel_channelName' - The name of the channel.
--
-- 'dataSource', 'channel_dataSource' - The location of the channel data.
newChannel ::
  -- | 'channelName'
  Prelude.Text ->
  -- | 'dataSource'
  DataSource ->
  Channel
newChannel pChannelName_ pDataSource_ =
  Channel'
    { contentType = Prelude.Nothing,
      recordWrapperType = Prelude.Nothing,
      shuffleConfig = Prelude.Nothing,
      compressionType = Prelude.Nothing,
      inputMode = Prelude.Nothing,
      channelName = pChannelName_,
      dataSource = pDataSource_
    }

-- | The MIME type of the data.
channel_contentType :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_contentType = Lens.lens (\Channel' {contentType} -> contentType) (\s@Channel' {} a -> s {contentType = a} :: Channel)

-- | Specify RecordIO as the value when input data is in raw format but the
-- training algorithm requires the RecordIO format. In this case, Amazon
-- SageMaker wraps each individual S3 object in a RecordIO record. If the
-- input data is already in RecordIO format, you don\'t need to set this
-- attribute. For more information, see
-- <https://mxnet.apache.org/api/architecture/note_data_loading#data-format Create a Dataset Using RecordIO>.
--
-- In File mode, leave this field unset or set it to None.
channel_recordWrapperType :: Lens.Lens' Channel (Prelude.Maybe RecordWrapper)
channel_recordWrapperType = Lens.lens (\Channel' {recordWrapperType} -> recordWrapperType) (\s@Channel' {} a -> s {recordWrapperType = a} :: Channel)

-- | A configuration for a shuffle option for input data in a channel. If you
-- use @S3Prefix@ for @S3DataType@, this shuffles the results of the S3 key
-- prefix matches. If you use @ManifestFile@, the order of the S3 object
-- references in the @ManifestFile@ is shuffled. If you use
-- @AugmentedManifestFile@, the order of the JSON lines in the
-- @AugmentedManifestFile@ is shuffled. The shuffling order is determined
-- using the @Seed@ value.
--
-- For Pipe input mode, shuffling is done at the start of every epoch. With
-- large datasets this ensures that the order of the training data is
-- different for each epoch, it helps reduce bias and possible overfitting.
-- In a multi-node training job when ShuffleConfig is combined with
-- @S3DataDistributionType@ of @ShardedByS3Key@, the data is shuffled
-- across nodes so that the content sent to a particular node on the first
-- epoch might be sent to a different node on the second epoch.
channel_shuffleConfig :: Lens.Lens' Channel (Prelude.Maybe ShuffleConfig)
channel_shuffleConfig = Lens.lens (\Channel' {shuffleConfig} -> shuffleConfig) (\s@Channel' {} a -> s {shuffleConfig = a} :: Channel)

-- | If training data is compressed, the compression type. The default value
-- is @None@. @CompressionType@ is used only in Pipe input mode. In File
-- mode, leave this field unset or set it to None.
channel_compressionType :: Lens.Lens' Channel (Prelude.Maybe CompressionType)
channel_compressionType = Lens.lens (\Channel' {compressionType} -> compressionType) (\s@Channel' {} a -> s {compressionType = a} :: Channel)

-- | (Optional) The input mode to use for the data channel in a training job.
-- If you don\'t set a value for @InputMode@, Amazon SageMaker uses the
-- value set for @TrainingInputMode@. Use this parameter to override the
-- @TrainingInputMode@ setting in a AlgorithmSpecification request when you
-- have a channel that needs a different input mode from the training
-- job\'s general setting. To download the data from Amazon Simple Storage
-- Service (Amazon S3) to the provisioned ML storage volume, and mount the
-- directory to a Docker volume, use @File@ input mode. To stream data
-- directly from Amazon S3 to the container, choose @Pipe@ input mode.
--
-- To use a model for incremental training, choose @File@ input model.
channel_inputMode :: Lens.Lens' Channel (Prelude.Maybe TrainingInputMode)
channel_inputMode = Lens.lens (\Channel' {inputMode} -> inputMode) (\s@Channel' {} a -> s {inputMode = a} :: Channel)

-- | The name of the channel.
channel_channelName :: Lens.Lens' Channel Prelude.Text
channel_channelName = Lens.lens (\Channel' {channelName} -> channelName) (\s@Channel' {} a -> s {channelName = a} :: Channel)

-- | The location of the channel data.
channel_dataSource :: Lens.Lens' Channel DataSource
channel_dataSource = Lens.lens (\Channel' {dataSource} -> dataSource) (\s@Channel' {} a -> s {dataSource = a} :: Channel)

instance Prelude.FromJSON Channel where
  parseJSON =
    Prelude.withObject
      "Channel"
      ( \x ->
          Channel'
            Prelude.<$> (x Prelude..:? "ContentType")
            Prelude.<*> (x Prelude..:? "RecordWrapperType")
            Prelude.<*> (x Prelude..:? "ShuffleConfig")
            Prelude.<*> (x Prelude..:? "CompressionType")
            Prelude.<*> (x Prelude..:? "InputMode")
            Prelude.<*> (x Prelude..: "ChannelName")
            Prelude.<*> (x Prelude..: "DataSource")
      )

instance Prelude.Hashable Channel

instance Prelude.NFData Channel

instance Prelude.ToJSON Channel where
  toJSON Channel' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ContentType" Prelude..=) Prelude.<$> contentType,
            ("RecordWrapperType" Prelude..=)
              Prelude.<$> recordWrapperType,
            ("ShuffleConfig" Prelude..=)
              Prelude.<$> shuffleConfig,
            ("CompressionType" Prelude..=)
              Prelude.<$> compressionType,
            ("InputMode" Prelude..=) Prelude.<$> inputMode,
            Prelude.Just ("ChannelName" Prelude..= channelName),
            Prelude.Just ("DataSource" Prelude..= dataSource)
          ]
      )
