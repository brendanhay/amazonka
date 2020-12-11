-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneyStateRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyStateRequest
  ( JourneyStateRequest (..),

    -- * Smart constructor
    mkJourneyStateRequest,

    -- * Lenses
    jsrState,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.State
import qualified Network.AWS.Prelude as Lude

-- | Changes the status of a journey.
--
-- /See:/ 'mkJourneyStateRequest' smart constructor.
newtype JourneyStateRequest = JourneyStateRequest'
  { state ::
      Lude.Maybe State
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JourneyStateRequest' with the minimum fields required to make a request.
--
-- * 'state' - The status of the journey. Currently, the only supported value is CANCELLED.
--
-- If you cancel a journey, Amazon Pinpoint continues to perform activities that are currently in progress, until those activities are complete. Amazon Pinpoint also continues to collect and aggregate analytics data for those activities, until they are complete, and any activities that were complete when you cancelled the journey.
-- After you cancel a journey, you can't add, change, or remove any activities from the journey. In addition, Amazon Pinpoint stops evaluating the journey and doesn't perform any activities that haven't started.
mkJourneyStateRequest ::
  JourneyStateRequest
mkJourneyStateRequest = JourneyStateRequest' {state = Lude.Nothing}

-- | The status of the journey. Currently, the only supported value is CANCELLED.
--
-- If you cancel a journey, Amazon Pinpoint continues to perform activities that are currently in progress, until those activities are complete. Amazon Pinpoint also continues to collect and aggregate analytics data for those activities, until they are complete, and any activities that were complete when you cancelled the journey.
-- After you cancel a journey, you can't add, change, or remove any activities from the journey. In addition, Amazon Pinpoint stops evaluating the journey and doesn't perform any activities that haven't started.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsrState :: Lens.Lens' JourneyStateRequest (Lude.Maybe State)
jsrState = Lens.lens (state :: JourneyStateRequest -> Lude.Maybe State) (\s a -> s {state = a} :: JourneyStateRequest)
{-# DEPRECATED jsrState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Lude.ToJSON JourneyStateRequest where
  toJSON JourneyStateRequest' {..} =
    Lude.object (Lude.catMaybes [("State" Lude..=) Lude.<$> state])
