{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.ChangeMessageVisibilityBatchRequestEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SQS.Types.ChangeMessageVisibilityBatchRequestEntry
  ( ChangeMessageVisibilityBatchRequestEntry (..)
  -- * Smart constructor
  , mkChangeMessageVisibilityBatchRequestEntry
  -- * Lenses
  , cId
  , cReceiptHandle
  , cVisibilityTimeout
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Encloses a receipt handle and an entry id for each message in @'ChangeMessageVisibilityBatch' .@ 
--
-- /Important:/ All of the following list parameters must be prefixed with @ChangeMessageVisibilityBatchRequestEntry.n@ , where @n@ is an integer value starting with @1@ . For example, a parameter list for this action might look like this:
-- @&ChangeMessageVisibilityBatchRequestEntry.1.Id=change_visibility_msg_2@ 
-- @&ChangeMessageVisibilityBatchRequestEntry.1.ReceiptHandle=your_receipt_handle@ 
-- @&ChangeMessageVisibilityBatchRequestEntry.1.VisibilityTimeout=45@ 
--
-- /See:/ 'mkChangeMessageVisibilityBatchRequestEntry' smart constructor.
data ChangeMessageVisibilityBatchRequestEntry = ChangeMessageVisibilityBatchRequestEntry'
  { id :: Core.Text
    -- ^ An identifier for this particular receipt handle used to communicate the result.
  , receiptHandle :: Core.Text
    -- ^ A receipt handle.
  , visibilityTimeout :: Core.Maybe Core.Int
    -- ^ The new value (in seconds) for the message's visibility timeout.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChangeMessageVisibilityBatchRequestEntry' value with any optional fields omitted.
mkChangeMessageVisibilityBatchRequestEntry
    :: Core.Text -- ^ 'id'
    -> Core.Text -- ^ 'receiptHandle'
    -> ChangeMessageVisibilityBatchRequestEntry
mkChangeMessageVisibilityBatchRequestEntry id receiptHandle
  = ChangeMessageVisibilityBatchRequestEntry'{id, receiptHandle,
                                              visibilityTimeout = Core.Nothing}

-- | An identifier for this particular receipt handle used to communicate the result.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cId :: Lens.Lens' ChangeMessageVisibilityBatchRequestEntry Core.Text
cId = Lens.field @"id"
{-# INLINEABLE cId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | A receipt handle.
--
-- /Note:/ Consider using 'receiptHandle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cReceiptHandle :: Lens.Lens' ChangeMessageVisibilityBatchRequestEntry Core.Text
cReceiptHandle = Lens.field @"receiptHandle"
{-# INLINEABLE cReceiptHandle #-}
{-# DEPRECATED receiptHandle "Use generic-lens or generic-optics with 'receiptHandle' instead"  #-}

-- | The new value (in seconds) for the message's visibility timeout.
--
-- /Note:/ Consider using 'visibilityTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVisibilityTimeout :: Lens.Lens' ChangeMessageVisibilityBatchRequestEntry (Core.Maybe Core.Int)
cVisibilityTimeout = Lens.field @"visibilityTimeout"
{-# INLINEABLE cVisibilityTimeout #-}
{-# DEPRECATED visibilityTimeout "Use generic-lens or generic-optics with 'visibilityTimeout' instead"  #-}

instance Core.ToQuery ChangeMessageVisibilityBatchRequestEntry
         where
        toQuery ChangeMessageVisibilityBatchRequestEntry{..}
          = Core.toQueryPair "Id" id Core.<>
              Core.toQueryPair "ReceiptHandle" receiptHandle
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VisibilityTimeout")
                visibilityTimeout
