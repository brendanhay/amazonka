{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.OrganizationEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.OrganizationEventDetails where

import Network.AWS.AWSHealth.Types.Event
import Network.AWS.AWSHealth.Types.EventDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Detailed information about an event. A combination of an
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_Event.html Event>
-- object, an
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_EventDescription.html EventDescription>
-- object, and additional metadata about the event. Returned by the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization>
-- operation.
--
-- /See:/ 'newOrganizationEventDetails' smart constructor.
data OrganizationEventDetails = OrganizationEventDetails'
  { -- | The 12-digit AWS account numbers that contains the affected entities.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | Additional metadata about the event.
    eventMetadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    eventDescription :: Prelude.Maybe EventDescription,
    event :: Prelude.Maybe Event
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OrganizationEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'organizationEventDetails_awsAccountId' - The 12-digit AWS account numbers that contains the affected entities.
--
-- 'eventMetadata', 'organizationEventDetails_eventMetadata' - Additional metadata about the event.
--
-- 'eventDescription', 'organizationEventDetails_eventDescription' - Undocumented member.
--
-- 'event', 'organizationEventDetails_event' - Undocumented member.
newOrganizationEventDetails ::
  OrganizationEventDetails
newOrganizationEventDetails =
  OrganizationEventDetails'
    { awsAccountId =
        Prelude.Nothing,
      eventMetadata = Prelude.Nothing,
      eventDescription = Prelude.Nothing,
      event = Prelude.Nothing
    }

-- | The 12-digit AWS account numbers that contains the affected entities.
organizationEventDetails_awsAccountId :: Lens.Lens' OrganizationEventDetails (Prelude.Maybe Prelude.Text)
organizationEventDetails_awsAccountId = Lens.lens (\OrganizationEventDetails' {awsAccountId} -> awsAccountId) (\s@OrganizationEventDetails' {} a -> s {awsAccountId = a} :: OrganizationEventDetails)

-- | Additional metadata about the event.
organizationEventDetails_eventMetadata :: Lens.Lens' OrganizationEventDetails (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
organizationEventDetails_eventMetadata = Lens.lens (\OrganizationEventDetails' {eventMetadata} -> eventMetadata) (\s@OrganizationEventDetails' {} a -> s {eventMetadata = a} :: OrganizationEventDetails) Prelude.. Lens.mapping Prelude._Coerce

-- | Undocumented member.
organizationEventDetails_eventDescription :: Lens.Lens' OrganizationEventDetails (Prelude.Maybe EventDescription)
organizationEventDetails_eventDescription = Lens.lens (\OrganizationEventDetails' {eventDescription} -> eventDescription) (\s@OrganizationEventDetails' {} a -> s {eventDescription = a} :: OrganizationEventDetails)

-- | Undocumented member.
organizationEventDetails_event :: Lens.Lens' OrganizationEventDetails (Prelude.Maybe Event)
organizationEventDetails_event = Lens.lens (\OrganizationEventDetails' {event} -> event) (\s@OrganizationEventDetails' {} a -> s {event = a} :: OrganizationEventDetails)

instance Prelude.FromJSON OrganizationEventDetails where
  parseJSON =
    Prelude.withObject
      "OrganizationEventDetails"
      ( \x ->
          OrganizationEventDetails'
            Prelude.<$> (x Prelude..:? "awsAccountId")
            Prelude.<*> ( x Prelude..:? "eventMetadata"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "eventDescription")
            Prelude.<*> (x Prelude..:? "event")
      )

instance Prelude.Hashable OrganizationEventDetails

instance Prelude.NFData OrganizationEventDetails
