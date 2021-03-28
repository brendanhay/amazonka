{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneyStateRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.JourneyStateRequest
  ( JourneyStateRequest (..)
  -- * Smart constructor
  , mkJourneyStateRequest
  -- * Lenses
  , jsrState
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.State as Types
import qualified Network.AWS.Prelude as Core

-- | Changes the status of a journey.
--
-- /See:/ 'mkJourneyStateRequest' smart constructor.
newtype JourneyStateRequest = JourneyStateRequest'
  { state :: Core.Maybe Types.State
    -- ^ The status of the journey. Currently, the only supported value is CANCELLED.
--
-- If you cancel a journey, Amazon Pinpoint continues to perform activities that are currently in progress, until those activities are complete. Amazon Pinpoint also continues to collect and aggregate analytics data for those activities, until they are complete, and any activities that were complete when you cancelled the journey.
-- After you cancel a journey, you can't add, change, or remove any activities from the journey. In addition, Amazon Pinpoint stops evaluating the journey and doesn't perform any activities that haven't started.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'JourneyStateRequest' value with any optional fields omitted.
mkJourneyStateRequest
    :: JourneyStateRequest
mkJourneyStateRequest = JourneyStateRequest'{state = Core.Nothing}

-- | The status of the journey. Currently, the only supported value is CANCELLED.
--
-- If you cancel a journey, Amazon Pinpoint continues to perform activities that are currently in progress, until those activities are complete. Amazon Pinpoint also continues to collect and aggregate analytics data for those activities, until they are complete, and any activities that were complete when you cancelled the journey.
-- After you cancel a journey, you can't add, change, or remove any activities from the journey. In addition, Amazon Pinpoint stops evaluating the journey and doesn't perform any activities that haven't started.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsrState :: Lens.Lens' JourneyStateRequest (Core.Maybe Types.State)
jsrState = Lens.field @"state"
{-# INLINEABLE jsrState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

instance Core.FromJSON JourneyStateRequest where
        toJSON JourneyStateRequest{..}
          = Core.object (Core.catMaybes [("State" Core..=) Core.<$> state])
