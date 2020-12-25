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
    cChannelName,
    cDataSource,
    cCompressionType,
    cContentType,
    cInputMode,
    cRecordWrapperType,
    cShuffleConfig,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ChannelName as Types
import qualified Network.AWS.SageMaker.Types.CompressionType as Types
import qualified Network.AWS.SageMaker.Types.ContentType as Types
import qualified Network.AWS.SageMaker.Types.DataSource as Types
import qualified Network.AWS.SageMaker.Types.RecordWrapper as Types
import qualified Network.AWS.SageMaker.Types.ShuffleConfig as Types
import qualified Network.AWS.SageMaker.Types.TrainingInputMode as Types

-- | A channel is a named input source that training algorithms can consume.
--
-- /See:/ 'mkChannel' smart constructor.
data Channel = Channel'
  { -- | The name of the channel.
    channelName :: Types.ChannelName,
    -- | The location of the channel data.
    dataSource :: Types.DataSource,
    -- | If training data is compressed, the compression type. The default value is @None@ . @CompressionType@ is used only in Pipe input mode. In File mode, leave this field unset or set it to None.
    compressionType :: Core.Maybe Types.CompressionType,
    -- | The MIME type of the data.
    contentType :: Core.Maybe Types.ContentType,
    -- | (Optional) The input mode to use for the data channel in a training job. If you don't set a value for @InputMode@ , Amazon SageMaker uses the value set for @TrainingInputMode@ . Use this parameter to override the @TrainingInputMode@ setting in a 'AlgorithmSpecification' request when you have a channel that needs a different input mode from the training job's general setting. To download the data from Amazon Simple Storage Service (Amazon S3) to the provisioned ML storage volume, and mount the directory to a Docker volume, use @File@ input mode. To stream data directly from Amazon S3 to the container, choose @Pipe@ input mode.
    --
    -- To use a model for incremental training, choose @File@ input model.
    inputMode :: Core.Maybe Types.TrainingInputMode,
    -- |
    --
    -- Specify RecordIO as the value when input data is in raw format but the training algorithm requires the RecordIO format. In this case, Amazon SageMaker wraps each individual S3 object in a RecordIO record. If the input data is already in RecordIO format, you don't need to set this attribute. For more information, see <https://mxnet.apache.org/api/architecture/note_data_loading#data-format Create a Dataset Using RecordIO> .
    -- In File mode, leave this field unset or set it to None.
    recordWrapperType :: Core.Maybe Types.RecordWrapper,
    -- | A configuration for a shuffle option for input data in a channel. If you use @S3Prefix@ for @S3DataType@ , this shuffles the results of the S3 key prefix matches. If you use @ManifestFile@ , the order of the S3 object references in the @ManifestFile@ is shuffled. If you use @AugmentedManifestFile@ , the order of the JSON lines in the @AugmentedManifestFile@ is shuffled. The shuffling order is determined using the @Seed@ value.
    --
    -- For Pipe input mode, shuffling is done at the start of every epoch. With large datasets this ensures that the order of the training data is different for each epoch, it helps reduce bias and possible overfitting. In a multi-node training job when ShuffleConfig is combined with @S3DataDistributionType@ of @ShardedByS3Key@ , the data is shuffled across nodes so that the content sent to a particular node on the first epoch might be sent to a different node on the second epoch.
    shuffleConfig :: Core.Maybe Types.ShuffleConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Channel' value with any optional fields omitted.
mkChannel ::
  -- | 'channelName'
  Types.ChannelName ->
  -- | 'dataSource'
  Types.DataSource ->
  Channel
mkChannel channelName dataSource =
  Channel'
    { channelName,
      dataSource,
      compressionType = Core.Nothing,
      contentType = Core.Nothing,
      inputMode = Core.Nothing,
      recordWrapperType = Core.Nothing,
      shuffleConfig = Core.Nothing
    }

-- | The name of the channel.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cChannelName :: Lens.Lens' Channel Types.ChannelName
cChannelName = Lens.field @"channelName"
{-# DEPRECATED cChannelName "Use generic-lens or generic-optics with 'channelName' instead." #-}

-- | The location of the channel data.
--
-- /Note:/ Consider using 'dataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDataSource :: Lens.Lens' Channel Types.DataSource
cDataSource = Lens.field @"dataSource"
{-# DEPRECATED cDataSource "Use generic-lens or generic-optics with 'dataSource' instead." #-}

-- | If training data is compressed, the compression type. The default value is @None@ . @CompressionType@ is used only in Pipe input mode. In File mode, leave this field unset or set it to None.
--
-- /Note:/ Consider using 'compressionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCompressionType :: Lens.Lens' Channel (Core.Maybe Types.CompressionType)
cCompressionType = Lens.field @"compressionType"
{-# DEPRECATED cCompressionType "Use generic-lens or generic-optics with 'compressionType' instead." #-}

-- | The MIME type of the data.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cContentType :: Lens.Lens' Channel (Core.Maybe Types.ContentType)
cContentType = Lens.field @"contentType"
{-# DEPRECATED cContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | (Optional) The input mode to use for the data channel in a training job. If you don't set a value for @InputMode@ , Amazon SageMaker uses the value set for @TrainingInputMode@ . Use this parameter to override the @TrainingInputMode@ setting in a 'AlgorithmSpecification' request when you have a channel that needs a different input mode from the training job's general setting. To download the data from Amazon Simple Storage Service (Amazon S3) to the provisioned ML storage volume, and mount the directory to a Docker volume, use @File@ input mode. To stream data directly from Amazon S3 to the container, choose @Pipe@ input mode.
--
-- To use a model for incremental training, choose @File@ input model.
--
-- /Note:/ Consider using 'inputMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cInputMode :: Lens.Lens' Channel (Core.Maybe Types.TrainingInputMode)
cInputMode = Lens.field @"inputMode"
{-# DEPRECATED cInputMode "Use generic-lens or generic-optics with 'inputMode' instead." #-}

-- |
--
-- Specify RecordIO as the value when input data is in raw format but the training algorithm requires the RecordIO format. In this case, Amazon SageMaker wraps each individual S3 object in a RecordIO record. If the input data is already in RecordIO format, you don't need to set this attribute. For more information, see <https://mxnet.apache.org/api/architecture/note_data_loading#data-format Create a Dataset Using RecordIO> .
-- In File mode, leave this field unset or set it to None.
--
-- /Note:/ Consider using 'recordWrapperType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRecordWrapperType :: Lens.Lens' Channel (Core.Maybe Types.RecordWrapper)
cRecordWrapperType = Lens.field @"recordWrapperType"
{-# DEPRECATED cRecordWrapperType "Use generic-lens or generic-optics with 'recordWrapperType' instead." #-}

-- | A configuration for a shuffle option for input data in a channel. If you use @S3Prefix@ for @S3DataType@ , this shuffles the results of the S3 key prefix matches. If you use @ManifestFile@ , the order of the S3 object references in the @ManifestFile@ is shuffled. If you use @AugmentedManifestFile@ , the order of the JSON lines in the @AugmentedManifestFile@ is shuffled. The shuffling order is determined using the @Seed@ value.
--
-- For Pipe input mode, shuffling is done at the start of every epoch. With large datasets this ensures that the order of the training data is different for each epoch, it helps reduce bias and possible overfitting. In a multi-node training job when ShuffleConfig is combined with @S3DataDistributionType@ of @ShardedByS3Key@ , the data is shuffled across nodes so that the content sent to a particular node on the first epoch might be sent to a different node on the second epoch.
--
-- /Note:/ Consider using 'shuffleConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cShuffleConfig :: Lens.Lens' Channel (Core.Maybe Types.ShuffleConfig)
cShuffleConfig = Lens.field @"shuffleConfig"
{-# DEPRECATED cShuffleConfig "Use generic-lens or generic-optics with 'shuffleConfig' instead." #-}

instance Core.FromJSON Channel where
  toJSON Channel {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ChannelName" Core..= channelName),
            Core.Just ("DataSource" Core..= dataSource),
            ("CompressionType" Core..=) Core.<$> compressionType,
            ("ContentType" Core..=) Core.<$> contentType,
            ("InputMode" Core..=) Core.<$> inputMode,
            ("RecordWrapperType" Core..=) Core.<$> recordWrapperType,
            ("ShuffleConfig" Core..=) Core.<$> shuffleConfig
          ]
      )

instance Core.FromJSON Channel where
  parseJSON =
    Core.withObject "Channel" Core.$
      \x ->
        Channel'
          Core.<$> (x Core..: "ChannelName")
          Core.<*> (x Core..: "DataSource")
          Core.<*> (x Core..:? "CompressionType")
          Core.<*> (x Core..:? "ContentType")
          Core.<*> (x Core..:? "InputMode")
          Core.<*> (x Core..:? "RecordWrapperType")
          Core.<*> (x Core..:? "ShuffleConfig")
