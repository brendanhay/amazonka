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
-- Module      : Amazonka.AWSHealth.Types.OrganizationEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AWSHealth.Types.OrganizationEventDetails where

import Amazonka.AWSHealth.Types.Event
import Amazonka.AWSHealth.Types.EventDescription
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

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
  { event :: Prelude.Maybe Event,
    eventDescription :: Prelude.Maybe EventDescription,
    -- | The 12-digit AWS account numbers that contains the affected entities.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | Additional metadata about the event.
    eventMetadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'event', 'organizationEventDetails_event' - Undocumented member.
--
-- 'eventDescription', 'organizationEventDetails_eventDescription' - Undocumented member.
--
-- 'awsAccountId', 'organizationEventDetails_awsAccountId' - The 12-digit AWS account numbers that contains the affected entities.
--
-- 'eventMetadata', 'organizationEventDetails_eventMetadata' - Additional metadata about the event.
newOrganizationEventDetails ::
  OrganizationEventDetails
newOrganizationEventDetails =
  OrganizationEventDetails'
    { event = Prelude.Nothing,
      eventDescription = Prelude.Nothing,
      awsAccountId = Prelude.Nothing,
      eventMetadata = Prelude.Nothing
    }

-- | Undocumented member.
organizationEventDetails_event :: Lens.Lens' OrganizationEventDetails (Prelude.Maybe Event)
organizationEventDetails_event = Lens.lens (\OrganizationEventDetails' {event} -> event) (\s@OrganizationEventDetails' {} a -> s {event = a} :: OrganizationEventDetails)

-- | Undocumented member.
organizationEventDetails_eventDescription :: Lens.Lens' OrganizationEventDetails (Prelude.Maybe EventDescription)
organizationEventDetails_eventDescription = Lens.lens (\OrganizationEventDetails' {eventDescription} -> eventDescription) (\s@OrganizationEventDetails' {} a -> s {eventDescription = a} :: OrganizationEventDetails)

-- | The 12-digit AWS account numbers that contains the affected entities.
organizationEventDetails_awsAccountId :: Lens.Lens' OrganizationEventDetails (Prelude.Maybe Prelude.Text)
organizationEventDetails_awsAccountId = Lens.lens (\OrganizationEventDetails' {awsAccountId} -> awsAccountId) (\s@OrganizationEventDetails' {} a -> s {awsAccountId = a} :: OrganizationEventDetails)

-- | Additional metadata about the event.
organizationEventDetails_eventMetadata :: Lens.Lens' OrganizationEventDetails (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
organizationEventDetails_eventMetadata = Lens.lens (\OrganizationEventDetails' {eventMetadata} -> eventMetadata) (\s@OrganizationEventDetails' {} a -> s {eventMetadata = a} :: OrganizationEventDetails) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON OrganizationEventDetails where
  parseJSON =
    Core.withObject
      "OrganizationEventDetails"
      ( \x ->
          OrganizationEventDetails'
            Prelude.<$> (x Core..:? "event")
            Prelude.<*> (x Core..:? "eventDescription")
            Prelude.<*> (x Core..:? "awsAccountId")
            Prelude.<*> (x Core..:? "eventMetadata" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable OrganizationEventDetails

instance Prelude.NFData OrganizationEventDetails
