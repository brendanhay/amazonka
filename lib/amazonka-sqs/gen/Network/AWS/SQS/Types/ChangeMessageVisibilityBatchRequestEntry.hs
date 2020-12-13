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
    cVisibilityTimeout,
    cId,
    cReceiptHandle,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Encloses a receipt handle and an entry id for each message in @'ChangeMessageVisibilityBatch' .@
--
-- /Important:/ All of the following list parameters must be prefixed with @ChangeMessageVisibilityBatchRequestEntry.n@ , where @n@ is an integer value starting with @1@ . For example, a parameter list for this action might look like this:
-- @&ChangeMessageVisibilityBatchRequestEntry.1.Id=change_visibility_msg_2@
-- @&ChangeMessageVisibilityBatchRequestEntry.1.ReceiptHandle=your_receipt_handle@
-- @&ChangeMessageVisibilityBatchRequestEntry.1.VisibilityTimeout=45@
--
-- /See:/ 'mkChangeMessageVisibilityBatchRequestEntry' smart constructor.
data ChangeMessageVisibilityBatchRequestEntry = ChangeMessageVisibilityBatchRequestEntry'
  { -- | The new value (in seconds) for the message's visibility timeout.
    visibilityTimeout :: Lude.Maybe Lude.Int,
    -- | An identifier for this particular receipt handle used to communicate the result.
    id :: Lude.Text,
    -- | A receipt handle.
    receiptHandle :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChangeMessageVisibilityBatchRequestEntry' with the minimum fields required to make a request.
--
-- * 'visibilityTimeout' - The new value (in seconds) for the message's visibility timeout.
-- * 'id' - An identifier for this particular receipt handle used to communicate the result.
-- * 'receiptHandle' - A receipt handle.
mkChangeMessageVisibilityBatchRequestEntry ::
  -- | 'id'
  Lude.Text ->
  -- | 'receiptHandle'
  Lude.Text ->
  ChangeMessageVisibilityBatchRequestEntry
mkChangeMessageVisibilityBatchRequestEntry pId_ pReceiptHandle_ =
  ChangeMessageVisibilityBatchRequestEntry'
    { visibilityTimeout =
        Lude.Nothing,
      id = pId_,
      receiptHandle = pReceiptHandle_
    }

-- | The new value (in seconds) for the message's visibility timeout.
--
-- /Note:/ Consider using 'visibilityTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVisibilityTimeout :: Lens.Lens' ChangeMessageVisibilityBatchRequestEntry (Lude.Maybe Lude.Int)
cVisibilityTimeout = Lens.lens (visibilityTimeout :: ChangeMessageVisibilityBatchRequestEntry -> Lude.Maybe Lude.Int) (\s a -> s {visibilityTimeout = a} :: ChangeMessageVisibilityBatchRequestEntry)
{-# DEPRECATED cVisibilityTimeout "Use generic-lens or generic-optics with 'visibilityTimeout' instead." #-}

-- | An identifier for this particular receipt handle used to communicate the result.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cId :: Lens.Lens' ChangeMessageVisibilityBatchRequestEntry Lude.Text
cId = Lens.lens (id :: ChangeMessageVisibilityBatchRequestEntry -> Lude.Text) (\s a -> s {id = a} :: ChangeMessageVisibilityBatchRequestEntry)
{-# DEPRECATED cId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A receipt handle.
--
-- /Note:/ Consider using 'receiptHandle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cReceiptHandle :: Lens.Lens' ChangeMessageVisibilityBatchRequestEntry Lude.Text
cReceiptHandle = Lens.lens (receiptHandle :: ChangeMessageVisibilityBatchRequestEntry -> Lude.Text) (\s a -> s {receiptHandle = a} :: ChangeMessageVisibilityBatchRequestEntry)
{-# DEPRECATED cReceiptHandle "Use generic-lens or generic-optics with 'receiptHandle' instead." #-}

instance Lude.ToQuery ChangeMessageVisibilityBatchRequestEntry where
  toQuery ChangeMessageVisibilityBatchRequestEntry' {..} =
    Lude.mconcat
      [ "VisibilityTimeout" Lude.=: visibilityTimeout,
        "Id" Lude.=: id,
        "ReceiptHandle" Lude.=: receiptHandle
      ]
