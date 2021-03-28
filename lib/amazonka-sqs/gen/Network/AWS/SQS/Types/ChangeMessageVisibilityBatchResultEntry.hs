{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.ChangeMessageVisibilityBatchResultEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SQS.Types.ChangeMessageVisibilityBatchResultEntry
  ( ChangeMessageVisibilityBatchResultEntry (..)
  -- * Smart constructor
  , mkChangeMessageVisibilityBatchResultEntry
  -- * Lenses
  , cmvbreId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Encloses the @Id@ of an entry in @'ChangeMessageVisibilityBatch' .@ 
--
-- /See:/ 'mkChangeMessageVisibilityBatchResultEntry' smart constructor.
newtype ChangeMessageVisibilityBatchResultEntry = ChangeMessageVisibilityBatchResultEntry'
  { id :: Core.Text
    -- ^ Represents a message whose visibility timeout has been changed successfully.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ChangeMessageVisibilityBatchResultEntry' value with any optional fields omitted.
mkChangeMessageVisibilityBatchResultEntry
    :: Core.Text -- ^ 'id'
    -> ChangeMessageVisibilityBatchResultEntry
mkChangeMessageVisibilityBatchResultEntry id
  = ChangeMessageVisibilityBatchResultEntry'{id}

-- | Represents a message whose visibility timeout has been changed successfully.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmvbreId :: Lens.Lens' ChangeMessageVisibilityBatchResultEntry Core.Text
cmvbreId = Lens.field @"id"
{-# INLINEABLE cmvbreId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.FromXML ChangeMessageVisibilityBatchResultEntry where
        parseXML x
          = ChangeMessageVisibilityBatchResultEntry' Core.<$>
              (x Core..@ "Id")
