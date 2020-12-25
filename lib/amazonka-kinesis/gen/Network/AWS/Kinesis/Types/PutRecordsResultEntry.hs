{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.PutRecordsResultEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.PutRecordsResultEntry
  ( PutRecordsResultEntry (..),

    -- * Smart constructor
    mkPutRecordsResultEntry,

    -- * Lenses
    prreErrorCode,
    prreErrorMessage,
    prreSequenceNumber,
    prreShardId,
  )
where

import qualified Network.AWS.Kinesis.Types.ErrorCode as Types
import qualified Network.AWS.Kinesis.Types.ErrorMessage as Types
import qualified Network.AWS.Kinesis.Types.SequenceNumber as Types
import qualified Network.AWS.Kinesis.Types.ShardId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the result of an individual record from a @PutRecords@ request. A record that is successfully added to a stream includes @SequenceNumber@ and @ShardId@ in the result. A record that fails to be added to the stream includes @ErrorCode@ and @ErrorMessage@ in the result.
--
-- /See:/ 'mkPutRecordsResultEntry' smart constructor.
data PutRecordsResultEntry = PutRecordsResultEntry'
  { -- | The error code for an individual record result. @ErrorCodes@ can be either @ProvisionedThroughputExceededException@ or @InternalFailure@ .
    errorCode :: Core.Maybe Types.ErrorCode,
    -- | The error message for an individual record result. An @ErrorCode@ value of @ProvisionedThroughputExceededException@ has an error message that includes the account ID, stream name, and shard ID. An @ErrorCode@ value of @InternalFailure@ has the error message @"Internal Service Failure"@ .
    errorMessage :: Core.Maybe Types.ErrorMessage,
    -- | The sequence number for an individual record result.
    sequenceNumber :: Core.Maybe Types.SequenceNumber,
    -- | The shard ID for an individual record result.
    shardId :: Core.Maybe Types.ShardId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutRecordsResultEntry' value with any optional fields omitted.
mkPutRecordsResultEntry ::
  PutRecordsResultEntry
mkPutRecordsResultEntry =
  PutRecordsResultEntry'
    { errorCode = Core.Nothing,
      errorMessage = Core.Nothing,
      sequenceNumber = Core.Nothing,
      shardId = Core.Nothing
    }

-- | The error code for an individual record result. @ErrorCodes@ can be either @ProvisionedThroughputExceededException@ or @InternalFailure@ .
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prreErrorCode :: Lens.Lens' PutRecordsResultEntry (Core.Maybe Types.ErrorCode)
prreErrorCode = Lens.field @"errorCode"
{-# DEPRECATED prreErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message for an individual record result. An @ErrorCode@ value of @ProvisionedThroughputExceededException@ has an error message that includes the account ID, stream name, and shard ID. An @ErrorCode@ value of @InternalFailure@ has the error message @"Internal Service Failure"@ .
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prreErrorMessage :: Lens.Lens' PutRecordsResultEntry (Core.Maybe Types.ErrorMessage)
prreErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED prreErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The sequence number for an individual record result.
--
-- /Note:/ Consider using 'sequenceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prreSequenceNumber :: Lens.Lens' PutRecordsResultEntry (Core.Maybe Types.SequenceNumber)
prreSequenceNumber = Lens.field @"sequenceNumber"
{-# DEPRECATED prreSequenceNumber "Use generic-lens or generic-optics with 'sequenceNumber' instead." #-}

-- | The shard ID for an individual record result.
--
-- /Note:/ Consider using 'shardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prreShardId :: Lens.Lens' PutRecordsResultEntry (Core.Maybe Types.ShardId)
prreShardId = Lens.field @"shardId"
{-# DEPRECATED prreShardId "Use generic-lens or generic-optics with 'shardId' instead." #-}

instance Core.FromJSON PutRecordsResultEntry where
  parseJSON =
    Core.withObject "PutRecordsResultEntry" Core.$
      \x ->
        PutRecordsResultEntry'
          Core.<$> (x Core..:? "ErrorCode")
          Core.<*> (x Core..:? "ErrorMessage")
          Core.<*> (x Core..:? "SequenceNumber")
          Core.<*> (x Core..:? "ShardId")
