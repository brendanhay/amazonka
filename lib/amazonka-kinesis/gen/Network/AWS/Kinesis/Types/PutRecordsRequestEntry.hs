{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.PutRecordsRequestEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.PutRecordsRequestEntry
  ( PutRecordsRequestEntry (..)
  -- * Smart constructor
  , mkPutRecordsRequestEntry
  -- * Lenses
  , prreData
  , prrePartitionKey
  , prreExplicitHashKey
  ) where

import qualified Network.AWS.Kinesis.Types.ExplicitHashKey as Types
import qualified Network.AWS.Kinesis.Types.PartitionKey as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output for @PutRecords@ .
--
-- /See:/ 'mkPutRecordsRequestEntry' smart constructor.
data PutRecordsRequestEntry = PutRecordsRequestEntry'
  { data' :: Core.Base64
    -- ^ The data blob to put into the record, which is base64-encoded when the blob is serialized. When the data blob (the payload before base64-encoding) is added to the partition key size, the total size must not exceed the maximum record size (1 MiB).
  , partitionKey :: Types.PartitionKey
    -- ^ Determines which shard in the stream the data record is assigned to. Partition keys are Unicode strings with a maximum length limit of 256 characters for each key. Amazon Kinesis Data Streams uses the partition key as input to a hash function that maps the partition key and associated data to a specific shard. Specifically, an MD5 hash function is used to map partition keys to 128-bit integer values and to map associated data records to shards. As a result of this hashing mechanism, all data records with the same partition key map to the same shard within the stream.
  , explicitHashKey :: Core.Maybe Types.ExplicitHashKey
    -- ^ The hash value used to determine explicitly the shard that the data record is assigned to by overriding the partition key hash.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutRecordsRequestEntry' value with any optional fields omitted.
mkPutRecordsRequestEntry
    :: Core.Base64 -- ^ 'data\''
    -> Types.PartitionKey -- ^ 'partitionKey'
    -> PutRecordsRequestEntry
mkPutRecordsRequestEntry data' partitionKey
  = PutRecordsRequestEntry'{data', partitionKey,
                            explicitHashKey = Core.Nothing}

-- | The data blob to put into the record, which is base64-encoded when the blob is serialized. When the data blob (the payload before base64-encoding) is added to the partition key size, the total size must not exceed the maximum record size (1 MiB).--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prreData :: Lens.Lens' PutRecordsRequestEntry Core.Base64
prreData = Lens.field @"data'"
{-# INLINEABLE prreData #-}
{-# DEPRECATED data' "Use generic-lens or generic-optics with 'data'' instead"  #-}

-- | Determines which shard in the stream the data record is assigned to. Partition keys are Unicode strings with a maximum length limit of 256 characters for each key. Amazon Kinesis Data Streams uses the partition key as input to a hash function that maps the partition key and associated data to a specific shard. Specifically, an MD5 hash function is used to map partition keys to 128-bit integer values and to map associated data records to shards. As a result of this hashing mechanism, all data records with the same partition key map to the same shard within the stream.
--
-- /Note:/ Consider using 'partitionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrePartitionKey :: Lens.Lens' PutRecordsRequestEntry Types.PartitionKey
prrePartitionKey = Lens.field @"partitionKey"
{-# INLINEABLE prrePartitionKey #-}
{-# DEPRECATED partitionKey "Use generic-lens or generic-optics with 'partitionKey' instead"  #-}

-- | The hash value used to determine explicitly the shard that the data record is assigned to by overriding the partition key hash.
--
-- /Note:/ Consider using 'explicitHashKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prreExplicitHashKey :: Lens.Lens' PutRecordsRequestEntry (Core.Maybe Types.ExplicitHashKey)
prreExplicitHashKey = Lens.field @"explicitHashKey"
{-# INLINEABLE prreExplicitHashKey #-}
{-# DEPRECATED explicitHashKey "Use generic-lens or generic-optics with 'explicitHashKey' instead"  #-}

instance Core.FromJSON PutRecordsRequestEntry where
        toJSON PutRecordsRequestEntry{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Data" Core..= data'),
                  Core.Just ("PartitionKey" Core..= partitionKey),
                  ("ExplicitHashKey" Core..=) Core.<$> explicitHashKey])
