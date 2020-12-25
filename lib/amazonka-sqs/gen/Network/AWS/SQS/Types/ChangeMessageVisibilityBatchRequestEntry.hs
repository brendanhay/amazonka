{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.ChangeMessageVisibilityBatchRequestEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.ChangeMessageVisibilityBatchRequestEntry
  ( ChangeMessageVisibilityBatchRequestEntry (..),

    -- * Smart constructor
    mkChangeMessageVisibilityBatchRequestEntry,

    -- * Lenses
    cId,
    cReceiptHandle,
    cVisibilityTimeout,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SQS.Types.String as Types

-- | Encloses a receipt handle and an entry id for each message in @'ChangeMessageVisibilityBatch' .@
--
-- /Important:/ All of the following list parameters must be prefixed with @ChangeMessageVisibilityBatchRequestEntry.n@ , where @n@ is an integer value starting with @1@ . For example, a parameter list for this action might look like this:
-- @&ChangeMessageVisibilityBatchRequestEntry.1.Id=change_visibility_msg_2@
-- @&ChangeMessageVisibilityBatchRequestEntry.1.ReceiptHandle=your_receipt_handle@
-- @&ChangeMessageVisibilityBatchRequestEntry.1.VisibilityTimeout=45@
--
-- /See:/ 'mkChangeMessageVisibilityBatchRequestEntry' smart constructor.
data ChangeMessageVisibilityBatchRequestEntry = ChangeMessageVisibilityBatchRequestEntry'
  { -- | An identifier for this particular receipt handle used to communicate the result.
    id :: Types.String,
    -- | A receipt handle.
    receiptHandle :: Types.String,
    -- | The new value (in seconds) for the message's visibility timeout.
    visibilityTimeout :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChangeMessageVisibilityBatchRequestEntry' value with any optional fields omitted.
mkChangeMessageVisibilityBatchRequestEntry ::
  -- | 'id'
  Types.String ->
  -- | 'receiptHandle'
  Types.String ->
  ChangeMessageVisibilityBatchRequestEntry
mkChangeMessageVisibilityBatchRequestEntry id receiptHandle =
  ChangeMessageVisibilityBatchRequestEntry'
    { id,
      receiptHandle,
      visibilityTimeout = Core.Nothing
    }

-- | An identifier for this particular receipt handle used to communicate the result.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cId :: Lens.Lens' ChangeMessageVisibilityBatchRequestEntry Types.String
cId = Lens.field @"id"
{-# DEPRECATED cId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A receipt handle.
--
-- /Note:/ Consider using 'receiptHandle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cReceiptHandle :: Lens.Lens' ChangeMessageVisibilityBatchRequestEntry Types.String
cReceiptHandle = Lens.field @"receiptHandle"
{-# DEPRECATED cReceiptHandle "Use generic-lens or generic-optics with 'receiptHandle' instead." #-}

-- | The new value (in seconds) for the message's visibility timeout.
--
-- /Note:/ Consider using 'visibilityTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVisibilityTimeout :: Lens.Lens' ChangeMessageVisibilityBatchRequestEntry (Core.Maybe Core.Int)
cVisibilityTimeout = Lens.field @"visibilityTimeout"
{-# DEPRECATED cVisibilityTimeout "Use generic-lens or generic-optics with 'visibilityTimeout' instead." #-}
