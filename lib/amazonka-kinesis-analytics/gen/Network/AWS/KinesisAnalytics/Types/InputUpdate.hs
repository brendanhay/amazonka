{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.InputUpdate
  ( InputUpdate (..)
  -- * Smart constructor
  , mkInputUpdate
  -- * Lenses
  , iuInputId
  , iuInputParallelismUpdate
  , iuInputProcessingConfigurationUpdate
  , iuInputSchemaUpdate
  , iuKinesisFirehoseInputUpdate
  , iuKinesisStreamsInputUpdate
  , iuNamePrefixUpdate
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.InputId as Types
import qualified Network.AWS.KinesisAnalytics.Types.InputParallelismUpdate as Types
import qualified Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationUpdate as Types
import qualified Network.AWS.KinesisAnalytics.Types.InputSchemaUpdate as Types
import qualified Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputUpdate as Types
import qualified Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputUpdate as Types
import qualified Network.AWS.KinesisAnalytics.Types.NamePrefixUpdate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes updates to a specific input configuration (identified by the @InputId@ of an application). 
--
-- /See:/ 'mkInputUpdate' smart constructor.
data InputUpdate = InputUpdate'
  { inputId :: Types.InputId
    -- ^ Input ID of the application input to be updated.
  , inputParallelismUpdate :: Core.Maybe Types.InputParallelismUpdate
    -- ^ Describes the parallelism updates (the number in-application streams Amazon Kinesis Analytics creates for the specific streaming source).
  , inputProcessingConfigurationUpdate :: Core.Maybe Types.InputProcessingConfigurationUpdate
    -- ^ Describes updates for an input processing configuration.
  , inputSchemaUpdate :: Core.Maybe Types.InputSchemaUpdate
    -- ^ Describes the data format on the streaming source, and how record elements on the streaming source map to columns of the in-application stream that is created.
  , kinesisFirehoseInputUpdate :: Core.Maybe Types.KinesisFirehoseInputUpdate
    -- ^ If an Amazon Kinesis Firehose delivery stream is the streaming source to be updated, provides an updated stream ARN and IAM role ARN.
  , kinesisStreamsInputUpdate :: Core.Maybe Types.KinesisStreamsInputUpdate
    -- ^ If an Amazon Kinesis stream is the streaming source to be updated, provides an updated stream Amazon Resource Name (ARN) and IAM role ARN.
  , namePrefixUpdate :: Core.Maybe Types.NamePrefixUpdate
    -- ^ Name prefix for in-application streams that Amazon Kinesis Analytics creates for the specific streaming source.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputUpdate' value with any optional fields omitted.
mkInputUpdate
    :: Types.InputId -- ^ 'inputId'
    -> InputUpdate
mkInputUpdate inputId
  = InputUpdate'{inputId, inputParallelismUpdate = Core.Nothing,
                 inputProcessingConfigurationUpdate = Core.Nothing,
                 inputSchemaUpdate = Core.Nothing,
                 kinesisFirehoseInputUpdate = Core.Nothing,
                 kinesisStreamsInputUpdate = Core.Nothing,
                 namePrefixUpdate = Core.Nothing}

-- | Input ID of the application input to be updated.
--
-- /Note:/ Consider using 'inputId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuInputId :: Lens.Lens' InputUpdate Types.InputId
iuInputId = Lens.field @"inputId"
{-# INLINEABLE iuInputId #-}
{-# DEPRECATED inputId "Use generic-lens or generic-optics with 'inputId' instead"  #-}

-- | Describes the parallelism updates (the number in-application streams Amazon Kinesis Analytics creates for the specific streaming source).
--
-- /Note:/ Consider using 'inputParallelismUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuInputParallelismUpdate :: Lens.Lens' InputUpdate (Core.Maybe Types.InputParallelismUpdate)
iuInputParallelismUpdate = Lens.field @"inputParallelismUpdate"
{-# INLINEABLE iuInputParallelismUpdate #-}
{-# DEPRECATED inputParallelismUpdate "Use generic-lens or generic-optics with 'inputParallelismUpdate' instead"  #-}

-- | Describes updates for an input processing configuration.
--
-- /Note:/ Consider using 'inputProcessingConfigurationUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuInputProcessingConfigurationUpdate :: Lens.Lens' InputUpdate (Core.Maybe Types.InputProcessingConfigurationUpdate)
iuInputProcessingConfigurationUpdate = Lens.field @"inputProcessingConfigurationUpdate"
{-# INLINEABLE iuInputProcessingConfigurationUpdate #-}
{-# DEPRECATED inputProcessingConfigurationUpdate "Use generic-lens or generic-optics with 'inputProcessingConfigurationUpdate' instead"  #-}

-- | Describes the data format on the streaming source, and how record elements on the streaming source map to columns of the in-application stream that is created.
--
-- /Note:/ Consider using 'inputSchemaUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuInputSchemaUpdate :: Lens.Lens' InputUpdate (Core.Maybe Types.InputSchemaUpdate)
iuInputSchemaUpdate = Lens.field @"inputSchemaUpdate"
{-# INLINEABLE iuInputSchemaUpdate #-}
{-# DEPRECATED inputSchemaUpdate "Use generic-lens or generic-optics with 'inputSchemaUpdate' instead"  #-}

-- | If an Amazon Kinesis Firehose delivery stream is the streaming source to be updated, provides an updated stream ARN and IAM role ARN.
--
-- /Note:/ Consider using 'kinesisFirehoseInputUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuKinesisFirehoseInputUpdate :: Lens.Lens' InputUpdate (Core.Maybe Types.KinesisFirehoseInputUpdate)
iuKinesisFirehoseInputUpdate = Lens.field @"kinesisFirehoseInputUpdate"
{-# INLINEABLE iuKinesisFirehoseInputUpdate #-}
{-# DEPRECATED kinesisFirehoseInputUpdate "Use generic-lens or generic-optics with 'kinesisFirehoseInputUpdate' instead"  #-}

-- | If an Amazon Kinesis stream is the streaming source to be updated, provides an updated stream Amazon Resource Name (ARN) and IAM role ARN.
--
-- /Note:/ Consider using 'kinesisStreamsInputUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuKinesisStreamsInputUpdate :: Lens.Lens' InputUpdate (Core.Maybe Types.KinesisStreamsInputUpdate)
iuKinesisStreamsInputUpdate = Lens.field @"kinesisStreamsInputUpdate"
{-# INLINEABLE iuKinesisStreamsInputUpdate #-}
{-# DEPRECATED kinesisStreamsInputUpdate "Use generic-lens or generic-optics with 'kinesisStreamsInputUpdate' instead"  #-}

-- | Name prefix for in-application streams that Amazon Kinesis Analytics creates for the specific streaming source.
--
-- /Note:/ Consider using 'namePrefixUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iuNamePrefixUpdate :: Lens.Lens' InputUpdate (Core.Maybe Types.NamePrefixUpdate)
iuNamePrefixUpdate = Lens.field @"namePrefixUpdate"
{-# INLINEABLE iuNamePrefixUpdate #-}
{-# DEPRECATED namePrefixUpdate "Use generic-lens or generic-optics with 'namePrefixUpdate' instead"  #-}

instance Core.FromJSON InputUpdate where
        toJSON InputUpdate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("InputId" Core..= inputId),
                  ("InputParallelismUpdate" Core..=) Core.<$> inputParallelismUpdate,
                  ("InputProcessingConfigurationUpdate" Core..=) Core.<$>
                    inputProcessingConfigurationUpdate,
                  ("InputSchemaUpdate" Core..=) Core.<$> inputSchemaUpdate,
                  ("KinesisFirehoseInputUpdate" Core..=) Core.<$>
                    kinesisFirehoseInputUpdate,
                  ("KinesisStreamsInputUpdate" Core..=) Core.<$>
                    kinesisStreamsInputUpdate,
                  ("NamePrefixUpdate" Core..=) Core.<$> namePrefixUpdate])
