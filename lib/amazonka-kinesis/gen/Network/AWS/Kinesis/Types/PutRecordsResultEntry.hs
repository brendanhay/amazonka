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
    prreSequenceNumber,
    prreErrorCode,
    prreErrorMessage,
    prreShardId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the result of an individual record from a @PutRecords@ request. A record that is successfully added to a stream includes @SequenceNumber@ and @ShardId@ in the result. A record that fails to be added to the stream includes @ErrorCode@ and @ErrorMessage@ in the result.
--
-- /See:/ 'mkPutRecordsResultEntry' smart constructor.
data PutRecordsResultEntry = PutRecordsResultEntry'
  { sequenceNumber ::
      Lude.Maybe Lude.Text,
    errorCode :: Lude.Maybe Lude.Text,
    errorMessage :: Lude.Maybe Lude.Text,
    shardId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRecordsResultEntry' with the minimum fields required to make a request.
--
-- * 'errorCode' - The error code for an individual record result. @ErrorCodes@ can be either @ProvisionedThroughputExceededException@ or @InternalFailure@ .
-- * 'errorMessage' - The error message for an individual record result. An @ErrorCode@ value of @ProvisionedThroughputExceededException@ has an error message that includes the account ID, stream name, and shard ID. An @ErrorCode@ value of @InternalFailure@ has the error message @"Internal Service Failure"@ .
-- * 'sequenceNumber' - The sequence number for an individual record result.
-- * 'shardId' - The shard ID for an individual record result.
mkPutRecordsResultEntry ::
  PutRecordsResultEntry
mkPutRecordsResultEntry =
  PutRecordsResultEntry'
    { sequenceNumber = Lude.Nothing,
      errorCode = Lude.Nothing,
      errorMessage = Lude.Nothing,
      shardId = Lude.Nothing
    }

-- | The sequence number for an individual record result.
--
-- /Note:/ Consider using 'sequenceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prreSequenceNumber :: Lens.Lens' PutRecordsResultEntry (Lude.Maybe Lude.Text)
prreSequenceNumber = Lens.lens (sequenceNumber :: PutRecordsResultEntry -> Lude.Maybe Lude.Text) (\s a -> s {sequenceNumber = a} :: PutRecordsResultEntry)
{-# DEPRECATED prreSequenceNumber "Use generic-lens or generic-optics with 'sequenceNumber' instead." #-}

-- | The error code for an individual record result. @ErrorCodes@ can be either @ProvisionedThroughputExceededException@ or @InternalFailure@ .
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prreErrorCode :: Lens.Lens' PutRecordsResultEntry (Lude.Maybe Lude.Text)
prreErrorCode = Lens.lens (errorCode :: PutRecordsResultEntry -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: PutRecordsResultEntry)
{-# DEPRECATED prreErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message for an individual record result. An @ErrorCode@ value of @ProvisionedThroughputExceededException@ has an error message that includes the account ID, stream name, and shard ID. An @ErrorCode@ value of @InternalFailure@ has the error message @"Internal Service Failure"@ .
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prreErrorMessage :: Lens.Lens' PutRecordsResultEntry (Lude.Maybe Lude.Text)
prreErrorMessage = Lens.lens (errorMessage :: PutRecordsResultEntry -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: PutRecordsResultEntry)
{-# DEPRECATED prreErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The shard ID for an individual record result.
--
-- /Note:/ Consider using 'shardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prreShardId :: Lens.Lens' PutRecordsResultEntry (Lude.Maybe Lude.Text)
prreShardId = Lens.lens (shardId :: PutRecordsResultEntry -> Lude.Maybe Lude.Text) (\s a -> s {shardId = a} :: PutRecordsResultEntry)
{-# DEPRECATED prreShardId "Use generic-lens or generic-optics with 'shardId' instead." #-}

instance Lude.FromJSON PutRecordsResultEntry where
  parseJSON =
    Lude.withObject
      "PutRecordsResultEntry"
      ( \x ->
          PutRecordsResultEntry'
            Lude.<$> (x Lude..:? "SequenceNumber")
            Lude.<*> (x Lude..:? "ErrorCode")
            Lude.<*> (x Lude..:? "ErrorMessage")
            Lude.<*> (x Lude..:? "ShardId")
      )
