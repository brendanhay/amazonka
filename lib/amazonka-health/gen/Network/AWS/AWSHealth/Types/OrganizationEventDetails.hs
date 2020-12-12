{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.OrganizationEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.OrganizationEventDetails
  ( OrganizationEventDetails (..),

    -- * Smart constructor
    mkOrganizationEventDetails,

    -- * Lenses
    oedEvent,
    oedEventDescription,
    oedAwsAccountId,
    oedEventMetadata,
  )
where

import Network.AWS.AWSHealth.Types.Event
import Network.AWS.AWSHealth.Types.EventDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Detailed information about an event. A combination of an <https://docs.aws.amazon.com/health/latest/APIReference/API_Event.html Event> object, an <https://docs.aws.amazon.com/health/latest/APIReference/API_EventDescription.html EventDescription> object, and additional metadata about the event. Returned by the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization> operation.
--
-- /See:/ 'mkOrganizationEventDetails' smart constructor.
data OrganizationEventDetails = OrganizationEventDetails'
  { event ::
      Lude.Maybe Event,
    eventDescription ::
      Lude.Maybe EventDescription,
    awsAccountId :: Lude.Maybe Lude.Text,
    eventMetadata ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OrganizationEventDetails' with the minimum fields required to make a request.
--
-- * 'awsAccountId' - The 12-digit AWS account numbers that contains the affected entities.
-- * 'event' - Undocumented field.
-- * 'eventDescription' - Undocumented field.
-- * 'eventMetadata' - Additional metadata about the event.
mkOrganizationEventDetails ::
  OrganizationEventDetails
mkOrganizationEventDetails =
  OrganizationEventDetails'
    { event = Lude.Nothing,
      eventDescription = Lude.Nothing,
      awsAccountId = Lude.Nothing,
      eventMetadata = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'event' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oedEvent :: Lens.Lens' OrganizationEventDetails (Lude.Maybe Event)
oedEvent = Lens.lens (event :: OrganizationEventDetails -> Lude.Maybe Event) (\s a -> s {event = a} :: OrganizationEventDetails)
{-# DEPRECATED oedEvent "Use generic-lens or generic-optics with 'event' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'eventDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oedEventDescription :: Lens.Lens' OrganizationEventDetails (Lude.Maybe EventDescription)
oedEventDescription = Lens.lens (eventDescription :: OrganizationEventDetails -> Lude.Maybe EventDescription) (\s a -> s {eventDescription = a} :: OrganizationEventDetails)
{-# DEPRECATED oedEventDescription "Use generic-lens or generic-optics with 'eventDescription' instead." #-}

-- | The 12-digit AWS account numbers that contains the affected entities.
--
-- /Note:/ Consider using 'awsAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oedAwsAccountId :: Lens.Lens' OrganizationEventDetails (Lude.Maybe Lude.Text)
oedAwsAccountId = Lens.lens (awsAccountId :: OrganizationEventDetails -> Lude.Maybe Lude.Text) (\s a -> s {awsAccountId = a} :: OrganizationEventDetails)
{-# DEPRECATED oedAwsAccountId "Use generic-lens or generic-optics with 'awsAccountId' instead." #-}

-- | Additional metadata about the event.
--
-- /Note:/ Consider using 'eventMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oedEventMetadata :: Lens.Lens' OrganizationEventDetails (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
oedEventMetadata = Lens.lens (eventMetadata :: OrganizationEventDetails -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {eventMetadata = a} :: OrganizationEventDetails)
{-# DEPRECATED oedEventMetadata "Use generic-lens or generic-optics with 'eventMetadata' instead." #-}

instance Lude.FromJSON OrganizationEventDetails where
  parseJSON =
    Lude.withObject
      "OrganizationEventDetails"
      ( \x ->
          OrganizationEventDetails'
            Lude.<$> (x Lude..:? "event")
            Lude.<*> (x Lude..:? "eventDescription")
            Lude.<*> (x Lude..:? "awsAccountId")
            Lude.<*> (x Lude..:? "eventMetadata" Lude..!= Lude.mempty)
      )
