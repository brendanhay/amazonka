{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails
  ( HistoryEventExecutionDataDetails (..)
  -- * Smart constructor
  , mkHistoryEventExecutionDataDetails
  -- * Lenses
  , heeddTruncated
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides details about input or output in an execution history event.
--
-- /See:/ 'mkHistoryEventExecutionDataDetails' smart constructor.
newtype HistoryEventExecutionDataDetails = HistoryEventExecutionDataDetails'
  { truncated :: Core.Maybe Core.Bool
    -- ^ Indicates whether input or output was truncated in the response. Always @false@ for API calls.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'HistoryEventExecutionDataDetails' value with any optional fields omitted.
mkHistoryEventExecutionDataDetails
    :: HistoryEventExecutionDataDetails
mkHistoryEventExecutionDataDetails
  = HistoryEventExecutionDataDetails'{truncated = Core.Nothing}

-- | Indicates whether input or output was truncated in the response. Always @false@ for API calls.
--
-- /Note:/ Consider using 'truncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heeddTruncated :: Lens.Lens' HistoryEventExecutionDataDetails (Core.Maybe Core.Bool)
heeddTruncated = Lens.field @"truncated"
{-# INLINEABLE heeddTruncated #-}
{-# DEPRECATED truncated "Use generic-lens or generic-optics with 'truncated' instead"  #-}

instance Core.FromJSON HistoryEventExecutionDataDetails where
        parseJSON
          = Core.withObject "HistoryEventExecutionDataDetails" Core.$
              \ x ->
                HistoryEventExecutionDataDetails' Core.<$> (x Core..:? "truncated")
