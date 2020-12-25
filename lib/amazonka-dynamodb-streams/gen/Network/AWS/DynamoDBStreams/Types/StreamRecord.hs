{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.StreamRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.StreamRecord
  ( StreamRecord (..),

    -- * Smart constructor
    mkStreamRecord,

    -- * Lenses
    srApproximateCreationDateTime,
    srKeys,
    srNewImage,
    srOldImage,
    srSequenceNumber,
    srSizeBytes,
    srStreamViewType,
  )
where

import qualified Network.AWS.DynamoDBStreams.Types.AttributeName as Types
import qualified Network.AWS.DynamoDBStreams.Types.AttributeValue as Types
import qualified Network.AWS.DynamoDBStreams.Types.SequenceNumber as Types
import qualified Network.AWS.DynamoDBStreams.Types.StreamViewType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A description of a single data modification that was performed on an item in a DynamoDB table.
--
-- /See:/ 'mkStreamRecord' smart constructor.
data StreamRecord = StreamRecord'
  { -- | The approximate date and time when the stream record was created, in <http://www.epochconverter.com/ UNIX epoch time> format.
    approximateCreationDateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The primary key attribute(s) for the DynamoDB item that was modified.
    keys :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue),
    -- | The item in the DynamoDB table as it appeared after it was modified.
    newImage :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue),
    -- | The item in the DynamoDB table as it appeared before it was modified.
    oldImage :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue),
    -- | The sequence number of the stream record.
    sequenceNumber :: Core.Maybe Types.SequenceNumber,
    -- | The size of the stream record, in bytes.
    sizeBytes :: Core.Maybe Core.Natural,
    -- | The type of data from the modified DynamoDB item that was captured in this stream record:
    --
    --
    --     * @KEYS_ONLY@ - only the key attributes of the modified item.
    --
    --
    --     * @NEW_IMAGE@ - the entire item, as it appeared after it was modified.
    --
    --
    --     * @OLD_IMAGE@ - the entire item, as it appeared before it was modified.
    --
    --
    --     * @NEW_AND_OLD_IMAGES@ - both the new and the old item images of the item.
    streamViewType :: Core.Maybe Types.StreamViewType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StreamRecord' value with any optional fields omitted.
mkStreamRecord ::
  StreamRecord
mkStreamRecord =
  StreamRecord'
    { approximateCreationDateTime = Core.Nothing,
      keys = Core.Nothing,
      newImage = Core.Nothing,
      oldImage = Core.Nothing,
      sequenceNumber = Core.Nothing,
      sizeBytes = Core.Nothing,
      streamViewType = Core.Nothing
    }

-- | The approximate date and time when the stream record was created, in <http://www.epochconverter.com/ UNIX epoch time> format.
--
-- /Note:/ Consider using 'approximateCreationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srApproximateCreationDateTime :: Lens.Lens' StreamRecord (Core.Maybe Core.NominalDiffTime)
srApproximateCreationDateTime = Lens.field @"approximateCreationDateTime"
{-# DEPRECATED srApproximateCreationDateTime "Use generic-lens or generic-optics with 'approximateCreationDateTime' instead." #-}

-- | The primary key attribute(s) for the DynamoDB item that was modified.
--
-- /Note:/ Consider using 'keys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srKeys :: Lens.Lens' StreamRecord (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
srKeys = Lens.field @"keys"
{-# DEPRECATED srKeys "Use generic-lens or generic-optics with 'keys' instead." #-}

-- | The item in the DynamoDB table as it appeared after it was modified.
--
-- /Note:/ Consider using 'newImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srNewImage :: Lens.Lens' StreamRecord (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
srNewImage = Lens.field @"newImage"
{-# DEPRECATED srNewImage "Use generic-lens or generic-optics with 'newImage' instead." #-}

-- | The item in the DynamoDB table as it appeared before it was modified.
--
-- /Note:/ Consider using 'oldImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srOldImage :: Lens.Lens' StreamRecord (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
srOldImage = Lens.field @"oldImage"
{-# DEPRECATED srOldImage "Use generic-lens or generic-optics with 'oldImage' instead." #-}

-- | The sequence number of the stream record.
--
-- /Note:/ Consider using 'sequenceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srSequenceNumber :: Lens.Lens' StreamRecord (Core.Maybe Types.SequenceNumber)
srSequenceNumber = Lens.field @"sequenceNumber"
{-# DEPRECATED srSequenceNumber "Use generic-lens or generic-optics with 'sequenceNumber' instead." #-}

-- | The size of the stream record, in bytes.
--
-- /Note:/ Consider using 'sizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srSizeBytes :: Lens.Lens' StreamRecord (Core.Maybe Core.Natural)
srSizeBytes = Lens.field @"sizeBytes"
{-# DEPRECATED srSizeBytes "Use generic-lens or generic-optics with 'sizeBytes' instead." #-}

-- | The type of data from the modified DynamoDB item that was captured in this stream record:
--
--
--     * @KEYS_ONLY@ - only the key attributes of the modified item.
--
--
--     * @NEW_IMAGE@ - the entire item, as it appeared after it was modified.
--
--
--     * @OLD_IMAGE@ - the entire item, as it appeared before it was modified.
--
--
--     * @NEW_AND_OLD_IMAGES@ - both the new and the old item images of the item.
--
--
--
-- /Note:/ Consider using 'streamViewType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srStreamViewType :: Lens.Lens' StreamRecord (Core.Maybe Types.StreamViewType)
srStreamViewType = Lens.field @"streamViewType"
{-# DEPRECATED srStreamViewType "Use generic-lens or generic-optics with 'streamViewType' instead." #-}

instance Core.FromJSON StreamRecord where
  parseJSON =
    Core.withObject "StreamRecord" Core.$
      \x ->
        StreamRecord'
          Core.<$> (x Core..:? "ApproximateCreationDateTime")
          Core.<*> (x Core..:? "Keys")
          Core.<*> (x Core..:? "NewImage")
          Core.<*> (x Core..:? "OldImage")
          Core.<*> (x Core..:? "SequenceNumber")
          Core.<*> (x Core..:? "SizeBytes")
          Core.<*> (x Core..:? "StreamViewType")
