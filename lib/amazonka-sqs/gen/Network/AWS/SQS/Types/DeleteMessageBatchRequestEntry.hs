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
import qualified Network.AWS.Prelude as Lude

-- | Encloses a receipt handle and an identifier for it.
--
-- /See:/ 'mkDeleteMessageBatchRequestEntry' smart constructor.
data DeleteMessageBatchRequestEntry = DeleteMessageBatchRequestEntry'
  { id ::
      Lude.Text,
    receiptHandle :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMessageBatchRequestEntry' with the minimum fields required to make a request.
--
-- * 'id' - An identifier for this particular receipt handle. This is used to communicate the result.
-- * 'receiptHandle' - A receipt handle.
mkDeleteMessageBatchRequestEntry ::
  -- | 'id'
  Lude.Text ->
  -- | 'receiptHandle'
  Lude.Text ->
  DeleteMessageBatchRequestEntry
mkDeleteMessageBatchRequestEntry pId_ pReceiptHandle_ =
  DeleteMessageBatchRequestEntry'
    { id = pId_,
      receiptHandle = pReceiptHandle_
    }

-- | An identifier for this particular receipt handle. This is used to communicate the result.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmbreId :: Lens.Lens' DeleteMessageBatchRequestEntry Lude.Text
dmbreId = Lens.lens (id :: DeleteMessageBatchRequestEntry -> Lude.Text) (\s a -> s {id = a} :: DeleteMessageBatchRequestEntry)
{-# DEPRECATED dmbreId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A receipt handle.
--
-- /Note:/ Consider using 'receiptHandle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmbreReceiptHandle :: Lens.Lens' DeleteMessageBatchRequestEntry Lude.Text
dmbreReceiptHandle = Lens.lens (receiptHandle :: DeleteMessageBatchRequestEntry -> Lude.Text) (\s a -> s {receiptHandle = a} :: DeleteMessageBatchRequestEntry)
{-# DEPRECATED dmbreReceiptHandle "Use generic-lens or generic-optics with 'receiptHandle' instead." #-}

instance Lude.ToQuery DeleteMessageBatchRequestEntry where
  toQuery DeleteMessageBatchRequestEntry' {..} =
    Lude.mconcat
      ["Id" Lude.=: id, "ReceiptHandle" Lude.=: receiptHandle]
