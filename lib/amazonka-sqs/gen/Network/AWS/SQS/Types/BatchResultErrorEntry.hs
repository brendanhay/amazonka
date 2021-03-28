{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.BatchResultErrorEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SQS.Types.BatchResultErrorEntry
  ( BatchResultErrorEntry (..)
  -- * Smart constructor
  , mkBatchResultErrorEntry
  -- * Lenses
  , breeId
  , breeSenderFault
  , breeCode
  , breeMessage
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Gives a detailed description of the result of an action on each entry in the request.
--
-- /See:/ 'mkBatchResultErrorEntry' smart constructor.
data BatchResultErrorEntry = BatchResultErrorEntry'
  { id :: Core.Text
    -- ^ The @Id@ of an entry in a batch request.
  , senderFault :: Core.Bool
    -- ^ Specifies whether the error happened due to the caller of the batch API action.
  , code :: Core.Text
    -- ^ An error code representing why the action failed on this entry.
  , message :: Core.Maybe Core.Text
    -- ^ A message explaining why the action failed on this entry.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchResultErrorEntry' value with any optional fields omitted.
mkBatchResultErrorEntry
    :: Core.Text -- ^ 'id'
    -> Core.Bool -- ^ 'senderFault'
    -> Core.Text -- ^ 'code'
    -> BatchResultErrorEntry
mkBatchResultErrorEntry id senderFault code
  = BatchResultErrorEntry'{id, senderFault, code,
                           message = Core.Nothing}

-- | The @Id@ of an entry in a batch request.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
breeId :: Lens.Lens' BatchResultErrorEntry Core.Text
breeId = Lens.field @"id"
{-# INLINEABLE breeId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | Specifies whether the error happened due to the caller of the batch API action.
--
-- /Note:/ Consider using 'senderFault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
breeSenderFault :: Lens.Lens' BatchResultErrorEntry Core.Bool
breeSenderFault = Lens.field @"senderFault"
{-# INLINEABLE breeSenderFault #-}
{-# DEPRECATED senderFault "Use generic-lens or generic-optics with 'senderFault' instead"  #-}

-- | An error code representing why the action failed on this entry.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
breeCode :: Lens.Lens' BatchResultErrorEntry Core.Text
breeCode = Lens.field @"code"
{-# INLINEABLE breeCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | A message explaining why the action failed on this entry.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
breeMessage :: Lens.Lens' BatchResultErrorEntry (Core.Maybe Core.Text)
breeMessage = Lens.field @"message"
{-# INLINEABLE breeMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromXML BatchResultErrorEntry where
        parseXML x
          = BatchResultErrorEntry' Core.<$>
              (x Core..@ "Id") Core.<*> x Core..@ "SenderFault" Core.<*>
                x Core..@ "Code"
                Core.<*> x Core..@? "Message"
