{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.DeleteMessageBatchRequestEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SQS.Types.DeleteMessageBatchRequestEntry
  ( DeleteMessageBatchRequestEntry (..)
  -- * Smart constructor
  , mkDeleteMessageBatchRequestEntry
  -- * Lenses
  , dmbreId
  , dmbreReceiptHandle
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Encloses a receipt handle and an identifier for it.
--
-- /See:/ 'mkDeleteMessageBatchRequestEntry' smart constructor.
data DeleteMessageBatchRequestEntry = DeleteMessageBatchRequestEntry'
  { id :: Core.Text
    -- ^ An identifier for this particular receipt handle. This is used to communicate the result.
  , receiptHandle :: Core.Text
    -- ^ A receipt handle.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMessageBatchRequestEntry' value with any optional fields omitted.
mkDeleteMessageBatchRequestEntry
    :: Core.Text -- ^ 'id'
    -> Core.Text -- ^ 'receiptHandle'
    -> DeleteMessageBatchRequestEntry
mkDeleteMessageBatchRequestEntry id receiptHandle
  = DeleteMessageBatchRequestEntry'{id, receiptHandle}

-- | An identifier for this particular receipt handle. This is used to communicate the result.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmbreId :: Lens.Lens' DeleteMessageBatchRequestEntry Core.Text
dmbreId = Lens.field @"id"
{-# INLINEABLE dmbreId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | A receipt handle.
--
-- /Note:/ Consider using 'receiptHandle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmbreReceiptHandle :: Lens.Lens' DeleteMessageBatchRequestEntry Core.Text
dmbreReceiptHandle = Lens.field @"receiptHandle"
{-# INLINEABLE dmbreReceiptHandle #-}
{-# DEPRECATED receiptHandle "Use generic-lens or generic-optics with 'receiptHandle' instead"  #-}

instance Core.ToQuery DeleteMessageBatchRequestEntry where
        toQuery DeleteMessageBatchRequestEntry{..}
          = Core.toQueryPair "Id" id Core.<>
              Core.toQueryPair "ReceiptHandle" receiptHandle
