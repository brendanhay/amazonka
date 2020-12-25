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
    oedAwsAccountId,
    oedEvent,
    oedEventDescription,
    oedEventMetadata,
  )
where

import qualified Network.AWS.AWSHealth.Types.AwsAccountId as Types
import qualified Network.AWS.AWSHealth.Types.Event as Types
import qualified Network.AWS.AWSHealth.Types.EventDescription as Types
import qualified Network.AWS.AWSHealth.Types.MetadataKey as Types
import qualified Network.AWS.AWSHealth.Types.MetadataValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Detailed information about an event. A combination of an <https://docs.aws.amazon.com/health/latest/APIReference/API_Event.html Event> object, an <https://docs.aws.amazon.com/health/latest/APIReference/API_EventDescription.html EventDescription> object, and additional metadata about the event. Returned by the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization> operation.
--
-- /See:/ 'mkOrganizationEventDetails' smart constructor.
data OrganizationEventDetails = OrganizationEventDetails'
  { -- | The 12-digit AWS account numbers that contains the affected entities.
    awsAccountId :: Core.Maybe Types.AwsAccountId,
    event :: Core.Maybe Types.Event,
    eventDescription :: Core.Maybe Types.EventDescription,
    -- | Additional metadata about the event.
    eventMetadata :: Core.Maybe (Core.HashMap Types.MetadataKey Types.MetadataValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'OrganizationEventDetails' value with any optional fields omitted.
mkOrganizationEventDetails ::
  OrganizationEventDetails
mkOrganizationEventDetails =
  OrganizationEventDetails'
    { awsAccountId = Core.Nothing,
      event = Core.Nothing,
      eventDescription = Core.Nothing,
      eventMetadata = Core.Nothing
    }

-- | The 12-digit AWS account numbers that contains the affected entities.
--
-- /Note:/ Consider using 'awsAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oedAwsAccountId :: Lens.Lens' OrganizationEventDetails (Core.Maybe Types.AwsAccountId)
oedAwsAccountId = Lens.field @"awsAccountId"
{-# DEPRECATED oedAwsAccountId "Use generic-lens or generic-optics with 'awsAccountId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'event' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oedEvent :: Lens.Lens' OrganizationEventDetails (Core.Maybe Types.Event)
oedEvent = Lens.field @"event"
{-# DEPRECATED oedEvent "Use generic-lens or generic-optics with 'event' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'eventDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oedEventDescription :: Lens.Lens' OrganizationEventDetails (Core.Maybe Types.EventDescription)
oedEventDescription = Lens.field @"eventDescription"
{-# DEPRECATED oedEventDescription "Use generic-lens or generic-optics with 'eventDescription' instead." #-}

-- | Additional metadata about the event.
--
-- /Note:/ Consider using 'eventMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oedEventMetadata :: Lens.Lens' OrganizationEventDetails (Core.Maybe (Core.HashMap Types.MetadataKey Types.MetadataValue))
oedEventMetadata = Lens.field @"eventMetadata"
{-# DEPRECATED oedEventMetadata "Use generic-lens or generic-optics with 'eventMetadata' instead." #-}

instance Core.FromJSON OrganizationEventDetails where
  parseJSON =
    Core.withObject "OrganizationEventDetails" Core.$
      \x ->
        OrganizationEventDetails'
          Core.<$> (x Core..:? "awsAccountId")
          Core.<*> (x Core..:? "event")
          Core.<*> (x Core..:? "eventDescription")
          Core.<*> (x Core..:? "eventMetadata")
