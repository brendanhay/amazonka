-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.PutRecordsRequestEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.PutRecordsRequestEntry
  ( PutRecordsRequestEntry (..),

    -- * Smart constructor
    mkPutRecordsRequestEntry,

    -- * Lenses
    prreExplicitHashKey,
    prreData,
    prrePartitionKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output for @PutRecords@ .
--
-- /See:/ 'mkPutRecordsRequestEntry' smart constructor.
data PutRecordsRequestEntry = PutRecordsRequestEntry'
  { explicitHashKey ::
      Lude.Maybe Lude.Text,
    data' :: Lude.Base64,
    partitionKey :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRecordsRequestEntry' with the minimum fields required to make a request.
--
-- * 'data'' - The data blob to put into the record, which is base64-encoded when the blob is serialized. When the data blob (the payload before base64-encoding) is added to the partition key size, the total size must not exceed the maximum record size (1 MiB).--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'explicitHashKey' - The hash value used to determine explicitly the shard that the data record is assigned to by overriding the partition key hash.
-- * 'partitionKey' - Determines which shard in the stream the data record is assigned to. Partition keys are Unicode strings with a maximum length limit of 256 characters for each key. Amazon Kinesis Data Streams uses the partition key as input to a hash function that maps the partition key and associated data to a specific shard. Specifically, an MD5 hash function is used to map partition keys to 128-bit integer values and to map associated data records to shards. As a result of this hashing mechanism, all data records with the same partition key map to the same shard within the stream.
mkPutRecordsRequestEntry ::
  -- | 'data''
  Lude.Base64 ->
  -- | 'partitionKey'
  Lude.Text ->
  PutRecordsRequestEntry
mkPutRecordsRequestEntry pData_ pPartitionKey_ =
  PutRecordsRequestEntry'
    { explicitHashKey = Lude.Nothing,
      data' = pData_,
      partitionKey = pPartitionKey_
    }

-- | The hash value used to determine explicitly the shard that the data record is assigned to by overriding the partition key hash.
--
-- /Note:/ Consider using 'explicitHashKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prreExplicitHashKey :: Lens.Lens' PutRecordsRequestEntry (Lude.Maybe Lude.Text)
prreExplicitHashKey = Lens.lens (explicitHashKey :: PutRecordsRequestEntry -> Lude.Maybe Lude.Text) (\s a -> s {explicitHashKey = a} :: PutRecordsRequestEntry)
{-# DEPRECATED prreExplicitHashKey "Use generic-lens or generic-optics with 'explicitHashKey' instead." #-}

-- | The data blob to put into the record, which is base64-encoded when the blob is serialized. When the data blob (the payload before base64-encoding) is added to the partition key size, the total size must not exceed the maximum record size (1 MiB).--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prreData :: Lens.Lens' PutRecordsRequestEntry Lude.Base64
prreData = Lens.lens (data' :: PutRecordsRequestEntry -> Lude.Base64) (\s a -> s {data' = a} :: PutRecordsRequestEntry)
{-# DEPRECATED prreData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | Determines which shard in the stream the data record is assigned to. Partition keys are Unicode strings with a maximum length limit of 256 characters for each key. Amazon Kinesis Data Streams uses the partition key as input to a hash function that maps the partition key and associated data to a specific shard. Specifically, an MD5 hash function is used to map partition keys to 128-bit integer values and to map associated data records to shards. As a result of this hashing mechanism, all data records with the same partition key map to the same shard within the stream.
--
-- /Note:/ Consider using 'partitionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrePartitionKey :: Lens.Lens' PutRecordsRequestEntry Lude.Text
prrePartitionKey = Lens.lens (partitionKey :: PutRecordsRequestEntry -> Lude.Text) (\s a -> s {partitionKey = a} :: PutRecordsRequestEntry)
{-# DEPRECATED prrePartitionKey "Use generic-lens or generic-optics with 'partitionKey' instead." #-}

instance Lude.ToJSON PutRecordsRequestEntry where
  toJSON PutRecordsRequestEntry' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ExplicitHashKey" Lude..=) Lude.<$> explicitHashKey,
            Lude.Just ("Data" Lude..= data'),
            Lude.Just ("PartitionKey" Lude..= partitionKey)
          ]
      )
