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
    srSizeBytes,
    srSequenceNumber,
    srApproximateCreationDateTime,
    srStreamViewType,
    srKeys,
    srOldImage,
    srNewImage,
  )
where

import Network.AWS.DynamoDBStreams.Types.AttributeValue
import Network.AWS.DynamoDBStreams.Types.StreamViewType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A description of a single data modification that was performed on an item in a DynamoDB table.
--
-- /See:/ 'mkStreamRecord' smart constructor.
data StreamRecord = StreamRecord'
  { sizeBytes ::
      Lude.Maybe Lude.Natural,
    sequenceNumber :: Lude.Maybe Lude.Text,
    approximateCreationDateTime :: Lude.Maybe Lude.Timestamp,
    streamViewType :: Lude.Maybe StreamViewType,
    keys :: Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)),
    oldImage :: Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)),
    newImage :: Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StreamRecord' with the minimum fields required to make a request.
--
-- * 'approximateCreationDateTime' - The approximate date and time when the stream record was created, in <http://www.epochconverter.com/ UNIX epoch time> format.
-- * 'keys' - The primary key attribute(s) for the DynamoDB item that was modified.
-- * 'newImage' - The item in the DynamoDB table as it appeared after it was modified.
-- * 'oldImage' - The item in the DynamoDB table as it appeared before it was modified.
-- * 'sequenceNumber' - The sequence number of the stream record.
-- * 'sizeBytes' - The size of the stream record, in bytes.
-- * 'streamViewType' - The type of data from the modified DynamoDB item that was captured in this stream record:
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
mkStreamRecord ::
  StreamRecord
mkStreamRecord =
  StreamRecord'
    { sizeBytes = Lude.Nothing,
      sequenceNumber = Lude.Nothing,
      approximateCreationDateTime = Lude.Nothing,
      streamViewType = Lude.Nothing,
      keys = Lude.Nothing,
      oldImage = Lude.Nothing,
      newImage = Lude.Nothing
    }

-- | The size of the stream record, in bytes.
--
-- /Note:/ Consider using 'sizeBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srSizeBytes :: Lens.Lens' StreamRecord (Lude.Maybe Lude.Natural)
srSizeBytes = Lens.lens (sizeBytes :: StreamRecord -> Lude.Maybe Lude.Natural) (\s a -> s {sizeBytes = a} :: StreamRecord)
{-# DEPRECATED srSizeBytes "Use generic-lens or generic-optics with 'sizeBytes' instead." #-}

-- | The sequence number of the stream record.
--
-- /Note:/ Consider using 'sequenceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srSequenceNumber :: Lens.Lens' StreamRecord (Lude.Maybe Lude.Text)
srSequenceNumber = Lens.lens (sequenceNumber :: StreamRecord -> Lude.Maybe Lude.Text) (\s a -> s {sequenceNumber = a} :: StreamRecord)
{-# DEPRECATED srSequenceNumber "Use generic-lens or generic-optics with 'sequenceNumber' instead." #-}

-- | The approximate date and time when the stream record was created, in <http://www.epochconverter.com/ UNIX epoch time> format.
--
-- /Note:/ Consider using 'approximateCreationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srApproximateCreationDateTime :: Lens.Lens' StreamRecord (Lude.Maybe Lude.Timestamp)
srApproximateCreationDateTime = Lens.lens (approximateCreationDateTime :: StreamRecord -> Lude.Maybe Lude.Timestamp) (\s a -> s {approximateCreationDateTime = a} :: StreamRecord)
{-# DEPRECATED srApproximateCreationDateTime "Use generic-lens or generic-optics with 'approximateCreationDateTime' instead." #-}

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
srStreamViewType :: Lens.Lens' StreamRecord (Lude.Maybe StreamViewType)
srStreamViewType = Lens.lens (streamViewType :: StreamRecord -> Lude.Maybe StreamViewType) (\s a -> s {streamViewType = a} :: StreamRecord)
{-# DEPRECATED srStreamViewType "Use generic-lens or generic-optics with 'streamViewType' instead." #-}

-- | The primary key attribute(s) for the DynamoDB item that was modified.
--
-- /Note:/ Consider using 'keys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srKeys :: Lens.Lens' StreamRecord (Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)))
srKeys = Lens.lens (keys :: StreamRecord -> Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue))) (\s a -> s {keys = a} :: StreamRecord)
{-# DEPRECATED srKeys "Use generic-lens or generic-optics with 'keys' instead." #-}

-- | The item in the DynamoDB table as it appeared before it was modified.
--
-- /Note:/ Consider using 'oldImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srOldImage :: Lens.Lens' StreamRecord (Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)))
srOldImage = Lens.lens (oldImage :: StreamRecord -> Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue))) (\s a -> s {oldImage = a} :: StreamRecord)
{-# DEPRECATED srOldImage "Use generic-lens or generic-optics with 'oldImage' instead." #-}

-- | The item in the DynamoDB table as it appeared after it was modified.
--
-- /Note:/ Consider using 'newImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srNewImage :: Lens.Lens' StreamRecord (Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)))
srNewImage = Lens.lens (newImage :: StreamRecord -> Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue))) (\s a -> s {newImage = a} :: StreamRecord)
{-# DEPRECATED srNewImage "Use generic-lens or generic-optics with 'newImage' instead." #-}

instance Lude.FromJSON StreamRecord where
  parseJSON =
    Lude.withObject
      "StreamRecord"
      ( \x ->
          StreamRecord'
            Lude.<$> (x Lude..:? "SizeBytes")
            Lude.<*> (x Lude..:? "SequenceNumber")
            Lude.<*> (x Lude..:? "ApproximateCreationDateTime")
            Lude.<*> (x Lude..:? "StreamViewType")
            Lude.<*> (x Lude..:? "Keys" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "OldImage" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "NewImage" Lude..!= Lude.mempty)
      )
