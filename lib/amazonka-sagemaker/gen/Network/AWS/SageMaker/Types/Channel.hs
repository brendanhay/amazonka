{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Channel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Channel
  ( Channel (..),

    -- * Smart constructor
    mkChannel,

    -- * Lenses
    cShuffleConfig,
    cRecordWrapperType,
    cInputMode,
    cCompressionType,
    cContentType,
    cChannelName,
    cDataSource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.CompressionType
import Network.AWS.SageMaker.Types.DataSource
import Network.AWS.SageMaker.Types.RecordWrapper
import Network.AWS.SageMaker.Types.ShuffleConfig
import Network.AWS.SageMaker.Types.TrainingInputMode

-- | A channel is a named input source that training algorithms can consume.
--
-- /See:/ 'mkChannel' smart constructor.
data Channel = Channel'
  { shuffleConfig :: Lude.Maybe ShuffleConfig,
    recordWrapperType :: Lude.Maybe RecordWrapper,
    inputMode :: Lude.Maybe TrainingInputMode,
    compressionType :: Lude.Maybe CompressionType,
    contentType :: Lude.Maybe Lude.Text,
    channelName :: Lude.Text,
    dataSource :: DataSource
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Channel' with the minimum fields required to make a request.
--
-- * 'channelName' - The name of the channel.
-- * 'compressionType' - If training data is compressed, the compression type. The default value is @None@ . @CompressionType@ is used only in Pipe input mode. In File mode, leave this field unset or set it to None.
-- * 'contentType' - The MIME type of the data.
-- * 'dataSource' - The location of the channel data.
-- * 'inputMode' - (Optional) The input mode to use for the data channel in a training job. If you don't set a value for @InputMode@ , Amazon SageMaker uses the value set for @TrainingInputMode@ . Use this parameter to override the @TrainingInputMode@ setting in a 'AlgorithmSpecification' request when you have a channel that needs a different input mode from the training job's general setting. To download the data from Amazon Simple Storage Service (Amazon S3) to the provisioned ML storage volume, and mount the directory to a Docker volume, use @File@ input mode. To stream data directly from Amazon S3 to the container, choose @Pipe@ input mode.
--
-- To use a model for incremental training, choose @File@ input model.
-- * 'recordWrapperType' -
--
-- Specify RecordIO as the value when input data is in raw format but the training algorithm requires the RecordIO format. In this case, Amazon SageMaker wraps each individual S3 object in a RecordIO record. If the input data is already in RecordIO format, you don't need to set this attribute. For more information, see <https://mxnet.apache.org/api/architecture/note_data_loading#data-format Create a Dataset Using RecordIO> .
-- In File mode, leave this field unset or set it to None.
-- * 'shuffleConfig' - A configuration for a shuffle option for input data in a channel. If you use @S3Prefix@ for @S3DataType@ , this shuffles the results of the S3 key prefix matches. If you use @ManifestFile@ , the order of the S3 object references in the @ManifestFile@ is shuffled. If you use @AugmentedManifestFile@ , the order of the JSON lines in the @AugmentedManifestFile@ is shuffled. The shuffling order is determined using the @Seed@ value.
--
-- For Pipe input mode, shuffling is done at the start of every epoch. With large datasets this ensures that the order of the training data is different for each epoch, it helps reduce bias and possible overfitting. In a multi-node training job when ShuffleConfig is combined with @S3DataDistributionType@ of @ShardedByS3Key@ , the data is shuffled across nodes so that the content sent to a particular node on the first epoch might be sent to a different node on the second epoch.
mkChannel ::
  -- | 'channelName'
  Lude.Text ->
  -- | 'dataSource'
  DataSource ->
  Channel
mkChannel pChannelName_ pDataSource_ =
  Channel'
    { shuffleConfig = Lude.Nothing,
      recordWrapperType = Lude.Nothing,
      inputMode = Lude.Nothing,
      compressionType = Lude.Nothing,
      contentType = Lude.Nothing,
      channelName = pChannelName_,
      dataSource = pDataSource_
    }

-- | A configuration for a shuffle option for input data in a channel. If you use @S3Prefix@ for @S3DataType@ , this shuffles the results of the S3 key prefix matches. If you use @ManifestFile@ , the order of the S3 object references in the @ManifestFile@ is shuffled. If you use @AugmentedManifestFile@ , the order of the JSON lines in the @AugmentedManifestFile@ is shuffled. The shuffling order is determined using the @Seed@ value.
--
-- For Pipe input mode, shuffling is done at the start of every epoch. With large datasets this ensures that the order of the training data is different for each epoch, it helps reduce bias and possible overfitting. In a multi-node training job when ShuffleConfig is combined with @S3DataDistributionType@ of @ShardedByS3Key@ , the data is shuffled across nodes so that the content sent to a particular node on the first epoch might be sent to a different node on the second epoch.
--
-- /Note:/ Consider using 'shuffleConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cShuffleConfig :: Lens.Lens' Channel (Lude.Maybe ShuffleConfig)
cShuffleConfig = Lens.lens (shuffleConfig :: Channel -> Lude.Maybe ShuffleConfig) (\s a -> s {shuffleConfig = a} :: Channel)
{-# DEPRECATED cShuffleConfig "Use generic-lens or generic-optics with 'shuffleConfig' instead." #-}

-- |
--
-- Specify RecordIO as the value when input data is in raw format but the training algorithm requires the RecordIO format. In this case, Amazon SageMaker wraps each individual S3 object in a RecordIO record. If the input data is already in RecordIO format, you don't need to set this attribute. For more information, see <https://mxnet.apache.org/api/architecture/note_data_loading#data-format Create a Dataset Using RecordIO> .
-- In File mode, leave this field unset or set it to None.
--
-- /Note:/ Consider using 'recordWrapperType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRecordWrapperType :: Lens.Lens' Channel (Lude.Maybe RecordWrapper)
cRecordWrapperType = Lens.lens (recordWrapperType :: Channel -> Lude.Maybe RecordWrapper) (\s a -> s {recordWrapperType = a} :: Channel)
{-# DEPRECATED cRecordWrapperType "Use generic-lens or generic-optics with 'recordWrapperType' instead." #-}

-- | (Optional) The input mode to use for the data channel in a training job. If you don't set a value for @InputMode@ , Amazon SageMaker uses the value set for @TrainingInputMode@ . Use this parameter to override the @TrainingInputMode@ setting in a 'AlgorithmSpecification' request when you have a channel that needs a different input mode from the training job's general setting. To download the data from Amazon Simple Storage Service (Amazon S3) to the provisioned ML storage volume, and mount the directory to a Docker volume, use @File@ input mode. To stream data directly from Amazon S3 to the container, choose @Pipe@ input mode.
--
-- To use a model for incremental training, choose @File@ input model.
--
-- /Note:/ Consider using 'inputMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cInputMode :: Lens.Lens' Channel (Lude.Maybe TrainingInputMode)
cInputMode = Lens.lens (inputMode :: Channel -> Lude.Maybe TrainingInputMode) (\s a -> s {inputMode = a} :: Channel)
{-# DEPRECATED cInputMode "Use generic-lens or generic-optics with 'inputMode' instead." #-}

-- | If training data is compressed, the compression type. The default value is @None@ . @CompressionType@ is used only in Pipe input mode. In File mode, leave this field unset or set it to None.
--
-- /Note:/ Consider using 'compressionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCompressionType :: Lens.Lens' Channel (Lude.Maybe CompressionType)
cCompressionType = Lens.lens (compressionType :: Channel -> Lude.Maybe CompressionType) (\s a -> s {compressionType = a} :: Channel)
{-# DEPRECATED cCompressionType "Use generic-lens or generic-optics with 'compressionType' instead." #-}

-- | The MIME type of the data.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cContentType :: Lens.Lens' Channel (Lude.Maybe Lude.Text)
cContentType = Lens.lens (contentType :: Channel -> Lude.Maybe Lude.Text) (\s a -> s {contentType = a} :: Channel)
{-# DEPRECATED cContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | The name of the channel.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cChannelName :: Lens.Lens' Channel Lude.Text
cChannelName = Lens.lens (channelName :: Channel -> Lude.Text) (\s a -> s {channelName = a} :: Channel)
{-# DEPRECATED cChannelName "Use generic-lens or generic-optics with 'channelName' instead." #-}

-- | The location of the channel data.
--
-- /Note:/ Consider using 'dataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDataSource :: Lens.Lens' Channel DataSource
cDataSource = Lens.lens (dataSource :: Channel -> DataSource) (\s a -> s {dataSource = a} :: Channel)
{-# DEPRECATED cDataSource "Use generic-lens or generic-optics with 'dataSource' instead." #-}

instance Lude.FromJSON Channel where
  parseJSON =
    Lude.withObject
      "Channel"
      ( \x ->
          Channel'
            Lude.<$> (x Lude..:? "ShuffleConfig")
            Lude.<*> (x Lude..:? "RecordWrapperType")
            Lude.<*> (x Lude..:? "InputMode")
            Lude.<*> (x Lude..:? "CompressionType")
            Lude.<*> (x Lude..:? "ContentType")
            Lude.<*> (x Lude..: "ChannelName")
            Lude.<*> (x Lude..: "DataSource")
      )

instance Lude.ToJSON Channel where
  toJSON Channel' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ShuffleConfig" Lude..=) Lude.<$> shuffleConfig,
            ("RecordWrapperType" Lude..=) Lude.<$> recordWrapperType,
            ("InputMode" Lude..=) Lude.<$> inputMode,
            ("CompressionType" Lude..=) Lude.<$> compressionType,
            ("ContentType" Lude..=) Lude.<$> contentType,
            Lude.Just ("ChannelName" Lude..= channelName),
            Lude.Just ("DataSource" Lude..= dataSource)
          ]
      )
