-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.ChangeMessageVisibilityBatchResultEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.ChangeMessageVisibilityBatchResultEntry
  ( ChangeMessageVisibilityBatchResultEntry (..),

    -- * Smart constructor
    mkChangeMessageVisibilityBatchResultEntry,

    -- * Lenses
    cmvbreId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Encloses the @Id@ of an entry in @'ChangeMessageVisibilityBatch' .@
--
-- /See:/ 'mkChangeMessageVisibilityBatchResultEntry' smart constructor.
newtype ChangeMessageVisibilityBatchResultEntry = ChangeMessageVisibilityBatchResultEntry'
  { id ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChangeMessageVisibilityBatchResultEntry' with the minimum fields required to make a request.
--
-- * 'id' - Represents a message whose visibility timeout has been changed successfully.
mkChangeMessageVisibilityBatchResultEntry ::
  -- | 'id'
  Lude.Text ->
  ChangeMessageVisibilityBatchResultEntry
mkChangeMessageVisibilityBatchResultEntry pId_ =
  ChangeMessageVisibilityBatchResultEntry' {id = pId_}

-- | Represents a message whose visibility timeout has been changed successfully.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmvbreId :: Lens.Lens' ChangeMessageVisibilityBatchResultEntry Lude.Text
cmvbreId = Lens.lens (id :: ChangeMessageVisibilityBatchResultEntry -> Lude.Text) (\s a -> s {id = a} :: ChangeMessageVisibilityBatchResultEntry)
{-# DEPRECATED cmvbreId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromXML ChangeMessageVisibilityBatchResultEntry where
  parseXML x =
    ChangeMessageVisibilityBatchResultEntry'
      Lude.<$> (x Lude..@ "Id")
