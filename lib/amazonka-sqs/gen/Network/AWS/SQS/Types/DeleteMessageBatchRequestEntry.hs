{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.DeleteMessageBatchRequestEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.DeleteMessageBatchRequestEntry
  ( DeleteMessageBatchRequestEntry (..),

    -- * Smart constructor
    mkDeleteMessageBatchRequestEntry,

    -- * Lenses
    dmbreId,
    dmbreReceiptHandle,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SQS.Types.String as Types

-- | Encloses a receipt handle and an identifier for it.
--
-- /See:/ 'mkDeleteMessageBatchRequestEntry' smart constructor.
data DeleteMessageBatchRequestEntry = DeleteMessageBatchRequestEntry'
  { -- | An identifier for this particular receipt handle. This is used to communicate the result.
    id :: Types.String,
    -- | A receipt handle.
    receiptHandle :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMessageBatchRequestEntry' value with any optional fields omitted.
mkDeleteMessageBatchRequestEntry ::
  -- | 'id'
  Types.String ->
  -- | 'receiptHandle'
  Types.String ->
  DeleteMessageBatchRequestEntry
mkDeleteMessageBatchRequestEntry id receiptHandle =
  DeleteMessageBatchRequestEntry' {id, receiptHandle}

-- | An identifier for this particular receipt handle. This is used to communicate the result.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmbreId :: Lens.Lens' DeleteMessageBatchRequestEntry Types.String
dmbreId = Lens.field @"id"
{-# DEPRECATED dmbreId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A receipt handle.
--
-- /Note:/ Consider using 'receiptHandle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmbreReceiptHandle :: Lens.Lens' DeleteMessageBatchRequestEntry Types.String
dmbreReceiptHandle = Lens.field @"receiptHandle"
{-# DEPRECATED dmbreReceiptHandle "Use generic-lens or generic-optics with 'receiptHandle' instead." #-}
