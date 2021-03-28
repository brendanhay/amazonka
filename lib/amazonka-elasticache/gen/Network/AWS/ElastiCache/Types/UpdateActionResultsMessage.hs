{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.UpdateActionResultsMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.UpdateActionResultsMessage
  ( UpdateActionResultsMessage (..)
  -- * Smart constructor
  , mkUpdateActionResultsMessage
  -- * Lenses
  , uarmProcessedUpdateActions
  , uarmUnprocessedUpdateActions
  ) where

import qualified Network.AWS.ElastiCache.Types.ProcessedUpdateAction as Types
import qualified Network.AWS.ElastiCache.Types.UnprocessedUpdateAction as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkUpdateActionResultsMessage' smart constructor.
data UpdateActionResultsMessage = UpdateActionResultsMessage'
  { processedUpdateActions :: Core.Maybe [Types.ProcessedUpdateAction]
    -- ^ Update actions that have been processed successfully
  , unprocessedUpdateActions :: Core.Maybe [Types.UnprocessedUpdateAction]
    -- ^ Update actions that haven't been processed successfully
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateActionResultsMessage' value with any optional fields omitted.
mkUpdateActionResultsMessage
    :: UpdateActionResultsMessage
mkUpdateActionResultsMessage
  = UpdateActionResultsMessage'{processedUpdateActions =
                                  Core.Nothing,
                                unprocessedUpdateActions = Core.Nothing}

-- | Update actions that have been processed successfully
--
-- /Note:/ Consider using 'processedUpdateActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarmProcessedUpdateActions :: Lens.Lens' UpdateActionResultsMessage (Core.Maybe [Types.ProcessedUpdateAction])
uarmProcessedUpdateActions = Lens.field @"processedUpdateActions"
{-# INLINEABLE uarmProcessedUpdateActions #-}
{-# DEPRECATED processedUpdateActions "Use generic-lens or generic-optics with 'processedUpdateActions' instead"  #-}

-- | Update actions that haven't been processed successfully
--
-- /Note:/ Consider using 'unprocessedUpdateActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarmUnprocessedUpdateActions :: Lens.Lens' UpdateActionResultsMessage (Core.Maybe [Types.UnprocessedUpdateAction])
uarmUnprocessedUpdateActions = Lens.field @"unprocessedUpdateActions"
{-# INLINEABLE uarmUnprocessedUpdateActions #-}
{-# DEPRECATED unprocessedUpdateActions "Use generic-lens or generic-optics with 'unprocessedUpdateActions' instead"  #-}

instance Core.FromXML UpdateActionResultsMessage where
        parseXML x
          = UpdateActionResultsMessage' Core.<$>
              (x Core..@? "ProcessedUpdateActions" Core..<@>
                 Core.parseXMLList "ProcessedUpdateAction")
                Core.<*>
                x Core..@? "UnprocessedUpdateActions" Core..<@>
                  Core.parseXMLList "UnprocessedUpdateAction"
