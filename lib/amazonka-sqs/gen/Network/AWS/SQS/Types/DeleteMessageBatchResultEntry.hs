{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.DeleteMessageBatchResultEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.DeleteMessageBatchResultEntry
  ( DeleteMessageBatchResultEntry (..),

    -- * Smart constructor
    mkDeleteMessageBatchResultEntry,

    -- * Lenses
    dId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Encloses the @Id@ of an entry in @'DeleteMessageBatch' .@
--
-- /See:/ 'mkDeleteMessageBatchResultEntry' smart constructor.
newtype DeleteMessageBatchResultEntry = DeleteMessageBatchResultEntry'
  { -- | Represents a successfully deleted message.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMessageBatchResultEntry' with the minimum fields required to make a request.
--
-- * 'id' - Represents a successfully deleted message.
mkDeleteMessageBatchResultEntry ::
  -- | 'id'
  Lude.Text ->
  DeleteMessageBatchResultEntry
mkDeleteMessageBatchResultEntry pId_ =
  DeleteMessageBatchResultEntry' {id = pId_}

-- | Represents a successfully deleted message.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dId :: Lens.Lens' DeleteMessageBatchResultEntry Lude.Text
dId = Lens.lens (id :: DeleteMessageBatchResultEntry -> Lude.Text) (\s a -> s {id = a} :: DeleteMessageBatchResultEntry)
{-# DEPRECATED dId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromXML DeleteMessageBatchResultEntry where
  parseXML x =
    DeleteMessageBatchResultEntry' Lude.<$> (x Lude..@ "Id")
