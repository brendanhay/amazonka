{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SQS.Types.String as Types

-- | Encloses the @Id@ of an entry in @'ChangeMessageVisibilityBatch' .@
--
-- /See:/ 'mkChangeMessageVisibilityBatchResultEntry' smart constructor.
newtype ChangeMessageVisibilityBatchResultEntry = ChangeMessageVisibilityBatchResultEntry'
  { -- | Represents a message whose visibility timeout has been changed successfully.
    id :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ChangeMessageVisibilityBatchResultEntry' value with any optional fields omitted.
mkChangeMessageVisibilityBatchResultEntry ::
  -- | 'id'
  Types.String ->
  ChangeMessageVisibilityBatchResultEntry
mkChangeMessageVisibilityBatchResultEntry id =
  ChangeMessageVisibilityBatchResultEntry' {id}

-- | Represents a message whose visibility timeout has been changed successfully.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmvbreId :: Lens.Lens' ChangeMessageVisibilityBatchResultEntry Types.String
cmvbreId = Lens.field @"id"
{-# DEPRECATED cmvbreId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.FromXML ChangeMessageVisibilityBatchResultEntry where
  parseXML x =
    ChangeMessageVisibilityBatchResultEntry'
      Core.<$> (x Core..@ "Id")
