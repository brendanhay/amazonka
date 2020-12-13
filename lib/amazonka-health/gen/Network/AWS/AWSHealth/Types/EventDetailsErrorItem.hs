{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EventDetailsErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EventDetailsErrorItem
  ( EventDetailsErrorItem (..),

    -- * Smart constructor
    mkEventDetailsErrorItem,

    -- * Lenses
    edeiEventARN,
    edeiErrorName,
    edeiErrorMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Error information returned when a <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetails.html DescribeEventDetails> operation cannot find a specified event.
--
-- /See:/ 'mkEventDetailsErrorItem' smart constructor.
data EventDetailsErrorItem = EventDetailsErrorItem'
  { -- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
    eventARN :: Lude.Maybe Lude.Text,
    -- | The name of the error.
    errorName :: Lude.Maybe Lude.Text,
    -- | A message that describes the error.
    errorMessage :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventDetailsErrorItem' with the minimum fields required to make a request.
--
-- * 'eventARN' - The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
-- * 'errorName' - The name of the error.
-- * 'errorMessage' - A message that describes the error.
mkEventDetailsErrorItem ::
  EventDetailsErrorItem
mkEventDetailsErrorItem =
  EventDetailsErrorItem'
    { eventARN = Lude.Nothing,
      errorName = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
--
-- /Note:/ Consider using 'eventARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edeiEventARN :: Lens.Lens' EventDetailsErrorItem (Lude.Maybe Lude.Text)
edeiEventARN = Lens.lens (eventARN :: EventDetailsErrorItem -> Lude.Maybe Lude.Text) (\s a -> s {eventARN = a} :: EventDetailsErrorItem)
{-# DEPRECATED edeiEventARN "Use generic-lens or generic-optics with 'eventARN' instead." #-}

-- | The name of the error.
--
-- /Note:/ Consider using 'errorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edeiErrorName :: Lens.Lens' EventDetailsErrorItem (Lude.Maybe Lude.Text)
edeiErrorName = Lens.lens (errorName :: EventDetailsErrorItem -> Lude.Maybe Lude.Text) (\s a -> s {errorName = a} :: EventDetailsErrorItem)
{-# DEPRECATED edeiErrorName "Use generic-lens or generic-optics with 'errorName' instead." #-}

-- | A message that describes the error.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edeiErrorMessage :: Lens.Lens' EventDetailsErrorItem (Lude.Maybe Lude.Text)
edeiErrorMessage = Lens.lens (errorMessage :: EventDetailsErrorItem -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: EventDetailsErrorItem)
{-# DEPRECATED edeiErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON EventDetailsErrorItem where
  parseJSON =
    Lude.withObject
      "EventDetailsErrorItem"
      ( \x ->
          EventDetailsErrorItem'
            Lude.<$> (x Lude..:? "eventArn")
            Lude.<*> (x Lude..:? "errorName")
            Lude.<*> (x Lude..:? "errorMessage")
      )
