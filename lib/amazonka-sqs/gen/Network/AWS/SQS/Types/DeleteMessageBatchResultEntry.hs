{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.DeleteMessageBatchResultEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SQS.Types.DeleteMessageBatchResultEntry
  ( DeleteMessageBatchResultEntry (..)
  -- * Smart constructor
  , mkDeleteMessageBatchResultEntry
  -- * Lenses
  , dId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Encloses the @Id@ of an entry in @'DeleteMessageBatch' .@ 
--
-- /See:/ 'mkDeleteMessageBatchResultEntry' smart constructor.
newtype DeleteMessageBatchResultEntry = DeleteMessageBatchResultEntry'
  { id :: Core.Text
    -- ^ Represents a successfully deleted message.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMessageBatchResultEntry' value with any optional fields omitted.
mkDeleteMessageBatchResultEntry
    :: Core.Text -- ^ 'id'
    -> DeleteMessageBatchResultEntry
mkDeleteMessageBatchResultEntry id
  = DeleteMessageBatchResultEntry'{id}

-- | Represents a successfully deleted message.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dId :: Lens.Lens' DeleteMessageBatchResultEntry Core.Text
dId = Lens.field @"id"
{-# INLINEABLE dId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.FromXML DeleteMessageBatchResultEntry where
        parseXML x
          = DeleteMessageBatchResultEntry' Core.<$> (x Core..@ "Id")
