{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ActivityScheduledEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ActivityScheduledEventDetails
  ( ActivityScheduledEventDetails (..),

    -- * Smart constructor
    mkActivityScheduledEventDetails,

    -- * Lenses
    asedHeartbeatInSeconds,
    asedInputDetails,
    asedInput,
    asedTimeoutInSeconds,
    asedResource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about an activity scheduled during an execution.
--
-- /See:/ 'mkActivityScheduledEventDetails' smart constructor.
data ActivityScheduledEventDetails = ActivityScheduledEventDetails'
  { heartbeatInSeconds ::
      Lude.Maybe Lude.Integer,
    inputDetails ::
      Lude.Maybe
        HistoryEventExecutionDataDetails,
    input ::
      Lude.Maybe
        (Lude.Sensitive Lude.Text),
    timeoutInSeconds ::
      Lude.Maybe Lude.Integer,
    resource :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActivityScheduledEventDetails' with the minimum fields required to make a request.
--
-- * 'heartbeatInSeconds' - The maximum allowed duration between two heartbeats for the activity task.
-- * 'input' - The JSON data input to the activity task. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
-- * 'inputDetails' - Contains details about the input for an execution history event.
-- * 'resource' - The Amazon Resource Name (ARN) of the scheduled activity.
-- * 'timeoutInSeconds' - The maximum allowed duration of the activity task.
mkActivityScheduledEventDetails ::
  -- | 'resource'
  Lude.Text ->
  ActivityScheduledEventDetails
mkActivityScheduledEventDetails pResource_ =
  ActivityScheduledEventDetails'
    { heartbeatInSeconds = Lude.Nothing,
      inputDetails = Lude.Nothing,
      input = Lude.Nothing,
      timeoutInSeconds = Lude.Nothing,
      resource = pResource_
    }

-- | The maximum allowed duration between two heartbeats for the activity task.
--
-- /Note:/ Consider using 'heartbeatInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asedHeartbeatInSeconds :: Lens.Lens' ActivityScheduledEventDetails (Lude.Maybe Lude.Integer)
asedHeartbeatInSeconds = Lens.lens (heartbeatInSeconds :: ActivityScheduledEventDetails -> Lude.Maybe Lude.Integer) (\s a -> s {heartbeatInSeconds = a} :: ActivityScheduledEventDetails)
{-# DEPRECATED asedHeartbeatInSeconds "Use generic-lens or generic-optics with 'heartbeatInSeconds' instead." #-}

-- | Contains details about the input for an execution history event.
--
-- /Note:/ Consider using 'inputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asedInputDetails :: Lens.Lens' ActivityScheduledEventDetails (Lude.Maybe HistoryEventExecutionDataDetails)
asedInputDetails = Lens.lens (inputDetails :: ActivityScheduledEventDetails -> Lude.Maybe HistoryEventExecutionDataDetails) (\s a -> s {inputDetails = a} :: ActivityScheduledEventDetails)
{-# DEPRECATED asedInputDetails "Use generic-lens or generic-optics with 'inputDetails' instead." #-}

-- | The JSON data input to the activity task. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asedInput :: Lens.Lens' ActivityScheduledEventDetails (Lude.Maybe (Lude.Sensitive Lude.Text))
asedInput = Lens.lens (input :: ActivityScheduledEventDetails -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {input = a} :: ActivityScheduledEventDetails)
{-# DEPRECATED asedInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The maximum allowed duration of the activity task.
--
-- /Note:/ Consider using 'timeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asedTimeoutInSeconds :: Lens.Lens' ActivityScheduledEventDetails (Lude.Maybe Lude.Integer)
asedTimeoutInSeconds = Lens.lens (timeoutInSeconds :: ActivityScheduledEventDetails -> Lude.Maybe Lude.Integer) (\s a -> s {timeoutInSeconds = a} :: ActivityScheduledEventDetails)
{-# DEPRECATED asedTimeoutInSeconds "Use generic-lens or generic-optics with 'timeoutInSeconds' instead." #-}

-- | The Amazon Resource Name (ARN) of the scheduled activity.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asedResource :: Lens.Lens' ActivityScheduledEventDetails Lude.Text
asedResource = Lens.lens (resource :: ActivityScheduledEventDetails -> Lude.Text) (\s a -> s {resource = a} :: ActivityScheduledEventDetails)
{-# DEPRECATED asedResource "Use generic-lens or generic-optics with 'resource' instead." #-}

instance Lude.FromJSON ActivityScheduledEventDetails where
  parseJSON =
    Lude.withObject
      "ActivityScheduledEventDetails"
      ( \x ->
          ActivityScheduledEventDetails'
            Lude.<$> (x Lude..:? "heartbeatInSeconds")
            Lude.<*> (x Lude..:? "inputDetails")
            Lude.<*> (x Lude..:? "input")
            Lude.<*> (x Lude..:? "timeoutInSeconds")
            Lude.<*> (x Lude..: "resource")
      )
