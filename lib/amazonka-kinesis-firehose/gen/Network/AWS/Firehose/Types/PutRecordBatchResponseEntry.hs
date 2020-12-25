{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.PutRecordBatchResponseEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.PutRecordBatchResponseEntry
  ( PutRecordBatchResponseEntry (..),

    -- * Smart constructor
    mkPutRecordBatchResponseEntry,

    -- * Lenses
    prbreErrorCode,
    prbreErrorMessage,
    prbreRecordId,
  )
where

import qualified Network.AWS.Firehose.Types.ErrorCode as Types
import qualified Network.AWS.Firehose.Types.ErrorMessage as Types
import qualified Network.AWS.Firehose.Types.RecordId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the result for an individual record from a 'PutRecordBatch' request. If the record is successfully added to your delivery stream, it receives a record ID. If the record fails to be added to your delivery stream, the result includes an error code and an error message.
--
-- /See:/ 'mkPutRecordBatchResponseEntry' smart constructor.
data PutRecordBatchResponseEntry = PutRecordBatchResponseEntry'
  { -- | The error code for an individual record result.
    errorCode :: Core.Maybe Types.ErrorCode,
    -- | The error message for an individual record result.
    errorMessage :: Core.Maybe Types.ErrorMessage,
    -- | The ID of the record.
    recordId :: Core.Maybe Types.RecordId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutRecordBatchResponseEntry' value with any optional fields omitted.
mkPutRecordBatchResponseEntry ::
  PutRecordBatchResponseEntry
mkPutRecordBatchResponseEntry =
  PutRecordBatchResponseEntry'
    { errorCode = Core.Nothing,
      errorMessage = Core.Nothing,
      recordId = Core.Nothing
    }

-- | The error code for an individual record result.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prbreErrorCode :: Lens.Lens' PutRecordBatchResponseEntry (Core.Maybe Types.ErrorCode)
prbreErrorCode = Lens.field @"errorCode"
{-# DEPRECATED prbreErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message for an individual record result.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prbreErrorMessage :: Lens.Lens' PutRecordBatchResponseEntry (Core.Maybe Types.ErrorMessage)
prbreErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED prbreErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The ID of the record.
--
-- /Note:/ Consider using 'recordId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prbreRecordId :: Lens.Lens' PutRecordBatchResponseEntry (Core.Maybe Types.RecordId)
prbreRecordId = Lens.field @"recordId"
{-# DEPRECATED prbreRecordId "Use generic-lens or generic-optics with 'recordId' instead." #-}

instance Core.FromJSON PutRecordBatchResponseEntry where
  parseJSON =
    Core.withObject "PutRecordBatchResponseEntry" Core.$
      \x ->
        PutRecordBatchResponseEntry'
          Core.<$> (x Core..:? "ErrorCode")
          Core.<*> (x Core..:? "ErrorMessage")
          Core.<*> (x Core..:? "RecordId")
