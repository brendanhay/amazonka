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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SQS.Types.String as Types

-- | Encloses the @Id@ of an entry in @'DeleteMessageBatch' .@
--
-- /See:/ 'mkDeleteMessageBatchResultEntry' smart constructor.
newtype DeleteMessageBatchResultEntry = DeleteMessageBatchResultEntry'
  { -- | Represents a successfully deleted message.
    id :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMessageBatchResultEntry' value with any optional fields omitted.
mkDeleteMessageBatchResultEntry ::
  -- | 'id'
  Types.String ->
  DeleteMessageBatchResultEntry
mkDeleteMessageBatchResultEntry id =
  DeleteMessageBatchResultEntry' {id}

-- | Represents a successfully deleted message.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dId :: Lens.Lens' DeleteMessageBatchResultEntry Types.String
dId = Lens.field @"id"
{-# DEPRECATED dId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.FromXML DeleteMessageBatchResultEntry where
  parseXML x =
    DeleteMessageBatchResultEntry' Core.<$> (x Core..@ "Id")
