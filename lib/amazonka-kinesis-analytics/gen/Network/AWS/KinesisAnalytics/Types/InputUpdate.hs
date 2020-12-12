{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputUpdate
  ( InputUpdate (..),

    -- * Smart constructor
    mkInputUpdate,

    -- * Lenses
    iuInputProcessingConfigurationUpdate,
    iuKinesisStreamsInputUpdate,
    iuInputParallelismUpdate,
    iuNamePrefixUpdate,
    iuInputSchemaUpdate,
    iuKinesisFirehoseInputUpdate,
    iuInputId,
  )
where

import Network.AWS.KinesisAnalytics.Types.InputParallelismUpdate
import Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationUpdate
import Network.AWS.KinesisAnalytics.Types.InputSchemaUpdate
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputUpdate
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes updates to a specific input configuration (identified by the @InputId@ of an application).
--
-- /See:/ 'mkInputUpdate' smart constructor.
data InputUpdate = InputUpdate'
  { inputProcessingConfigurationUpdate ::
      Lude.Maybe InputProcessingConfigurationUpdate,
    kinesisStreamsInputUpdate :: Lude.Maybe KinesisStreamsInputUpdate,
    inputParallelismUpdate :: Lude.Maybe InputParallelismUpdate,
    namePrefixUpdate :: Lude.Maybe Lude.Text,
    inputSchemaUpdate :: Lude.Maybe InputSchemaUpdate,
    kinesisFirehoseInputUpdate ::
      Lude.Maybe KinesisFirehoseInputUpdate,
    inputId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputUpdate' with the minimum fields required to make a request.
--
-- * 'inputId' - Input ID of the application input to be updated.
-- * 'inputParallelismUpdate' - Describes the parallelism updates (the number in-application streams Amazon Kinesis Analytics creates for the specific streaming source).
-- * 'inputProcessingConfigurationUpdate' - Describes updates for an input processing configuration.
-- * 'inputSchemaUpdate' - Describes the data format on the streaming source, and how record elements on the streaming source map to columns of the in-application stream that is created.
-- * 'kinesisFirehoseInputUpdate' - If an Amazon Kinesis Firehose delivery stream is the streaming source to be updated, provides an updated stream ARN and IAM role ARN.
-- * 'kinesisStreamsInputUpdate' - If an Amazon Kinesis stream is the streaming source to be updated, provides an updated stream Amazon Resource Name (ARN) and IAM role ARN.
-- * 'namePrefixUpdate' - Name prefix for in-application streams that Amazon Kinesis Analytics creates for the specific streaming source.
mkInputUpdate ::
  -- | 'inputId'
  Lude.Text ->
  InputUpdate
mkInputUpdate pInputId_ =
  InputUpdate'
    { inputProcessingConfigurationUpdate = Lude.Nothing,
      kinesisStreamsInputUpdate = Lude.Nothing,
      inputParallelismUpdate = Lude.Nothing,
      namePrefixUpdate = Lude.Nothing,
      inputSchemaUpdate = Lude.Nothing,
      kinesisFirehoseInputUpdate = Lude.Nothing,
      inputId = pInputId_
    }

-- | Describes updates for an input processing configuration.
--
-- /Note:/ Consider using 'inputProcessingConfigurationUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuInputProcessingConfigurationUpdate :: Lens.Lens' InputUpdate (Lude.Maybe InputProcessingConfigurationUpdate)
iuInputProcessingConfigurationUpdate = Lens.lens (inputProcessingConfigurationUpdate :: InputUpdate -> Lude.Maybe InputProcessingConfigurationUpdate) (\s a -> s {inputProcessingConfigurationUpdate = a} :: InputUpdate)
{-# DEPRECATED iuInputProcessingConfigurationUpdate "Use generic-lens or generic-optics with 'inputProcessingConfigurationUpdate' instead." #-}

-- | If an Amazon Kinesis stream is the streaming source to be updated, provides an updated stream Amazon Resource Name (ARN) and IAM role ARN.
--
-- /Note:/ Consider using 'kinesisStreamsInputUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuKinesisStreamsInputUpdate :: Lens.Lens' InputUpdate (Lude.Maybe KinesisStreamsInputUpdate)
iuKinesisStreamsInputUpdate = Lens.lens (kinesisStreamsInputUpdate :: InputUpdate -> Lude.Maybe KinesisStreamsInputUpdate) (\s a -> s {kinesisStreamsInputUpdate = a} :: InputUpdate)
{-# DEPRECATED iuKinesisStreamsInputUpdate "Use generic-lens or generic-optics with 'kinesisStreamsInputUpdate' instead." #-}

-- | Describes the parallelism updates (the number in-application streams Amazon Kinesis Analytics creates for the specific streaming source).
--
-- /Note:/ Consider using 'inputParallelismUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuInputParallelismUpdate :: Lens.Lens' InputUpdate (Lude.Maybe InputParallelismUpdate)
iuInputParallelismUpdate = Lens.lens (inputParallelismUpdate :: InputUpdate -> Lude.Maybe InputParallelismUpdate) (\s a -> s {inputParallelismUpdate = a} :: InputUpdate)
{-# DEPRECATED iuInputParallelismUpdate "Use generic-lens or generic-optics with 'inputParallelismUpdate' instead." #-}

-- | Name prefix for in-application streams that Amazon Kinesis Analytics creates for the specific streaming source.
--
-- /Note:/ Consider using 'namePrefixUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuNamePrefixUpdate :: Lens.Lens' InputUpdate (Lude.Maybe Lude.Text)
iuNamePrefixUpdate = Lens.lens (namePrefixUpdate :: InputUpdate -> Lude.Maybe Lude.Text) (\s a -> s {namePrefixUpdate = a} :: InputUpdate)
{-# DEPRECATED iuNamePrefixUpdate "Use generic-lens or generic-optics with 'namePrefixUpdate' instead." #-}

-- | Describes the data format on the streaming source, and how record elements on the streaming source map to columns of the in-application stream that is created.
--
-- /Note:/ Consider using 'inputSchemaUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuInputSchemaUpdate :: Lens.Lens' InputUpdate (Lude.Maybe InputSchemaUpdate)
iuInputSchemaUpdate = Lens.lens (inputSchemaUpdate :: InputUpdate -> Lude.Maybe InputSchemaUpdate) (\s a -> s {inputSchemaUpdate = a} :: InputUpdate)
{-# DEPRECATED iuInputSchemaUpdate "Use generic-lens or generic-optics with 'inputSchemaUpdate' instead." #-}

-- | If an Amazon Kinesis Firehose delivery stream is the streaming source to be updated, provides an updated stream ARN and IAM role ARN.
--
-- /Note:/ Consider using 'kinesisFirehoseInputUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuKinesisFirehoseInputUpdate :: Lens.Lens' InputUpdate (Lude.Maybe KinesisFirehoseInputUpdate)
iuKinesisFirehoseInputUpdate = Lens.lens (kinesisFirehoseInputUpdate :: InputUpdate -> Lude.Maybe KinesisFirehoseInputUpdate) (\s a -> s {kinesisFirehoseInputUpdate = a} :: InputUpdate)
{-# DEPRECATED iuKinesisFirehoseInputUpdate "Use generic-lens or generic-optics with 'kinesisFirehoseInputUpdate' instead." #-}

-- | Input ID of the application input to be updated.
--
-- /Note:/ Consider using 'inputId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuInputId :: Lens.Lens' InputUpdate Lude.Text
iuInputId = Lens.lens (inputId :: InputUpdate -> Lude.Text) (\s a -> s {inputId = a} :: InputUpdate)
{-# DEPRECATED iuInputId "Use generic-lens or generic-optics with 'inputId' instead." #-}

instance Lude.ToJSON InputUpdate where
  toJSON InputUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InputProcessingConfigurationUpdate" Lude..=)
              Lude.<$> inputProcessingConfigurationUpdate,
            ("KinesisStreamsInputUpdate" Lude..=)
              Lude.<$> kinesisStreamsInputUpdate,
            ("InputParallelismUpdate" Lude..=) Lude.<$> inputParallelismUpdate,
            ("NamePrefixUpdate" Lude..=) Lude.<$> namePrefixUpdate,
            ("InputSchemaUpdate" Lude..=) Lude.<$> inputSchemaUpdate,
            ("KinesisFirehoseInputUpdate" Lude..=)
              Lude.<$> kinesisFirehoseInputUpdate,
            Lude.Just ("InputId" Lude..= inputId)
          ]
      )
