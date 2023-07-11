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
-- Module      : Amazonka.ElasticBeanstalk.Types.EventDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.EventDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types.EventSeverity
import qualified Amazonka.Prelude as Prelude

-- | Describes an event.
--
-- /See:/ 'newEventDescription' smart constructor.
data EventDescription = EventDescription'
  { -- | The application associated with the event.
    applicationName :: Prelude.Maybe Prelude.Text,
    -- | The name of the environment associated with this event.
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | The date when the event occurred.
    eventDate :: Prelude.Maybe Data.ISO8601,
    -- | The event message.
    message :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the platform version.
    platformArn :: Prelude.Maybe Prelude.Text,
    -- | The web service request ID for the activity of this event.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The severity level of this event.
    severity :: Prelude.Maybe EventSeverity,
    -- | The name of the configuration associated with this event.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | The release label for the application version associated with this
    -- event.
    versionLabel :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'eventDescription_applicationName' - The application associated with the event.
--
-- 'environmentName', 'eventDescription_environmentName' - The name of the environment associated with this event.
--
-- 'eventDate', 'eventDescription_eventDate' - The date when the event occurred.
--
-- 'message', 'eventDescription_message' - The event message.
--
-- 'platformArn', 'eventDescription_platformArn' - The ARN of the platform version.
--
-- 'requestId', 'eventDescription_requestId' - The web service request ID for the activity of this event.
--
-- 'severity', 'eventDescription_severity' - The severity level of this event.
--
-- 'templateName', 'eventDescription_templateName' - The name of the configuration associated with this event.
--
-- 'versionLabel', 'eventDescription_versionLabel' - The release label for the application version associated with this
-- event.
newEventDescription ::
  EventDescription
newEventDescription =
  EventDescription'
    { applicationName =
        Prelude.Nothing,
      environmentName = Prelude.Nothing,
      eventDate = Prelude.Nothing,
      message = Prelude.Nothing,
      platformArn = Prelude.Nothing,
      requestId = Prelude.Nothing,
      severity = Prelude.Nothing,
      templateName = Prelude.Nothing,
      versionLabel = Prelude.Nothing
    }

-- | The application associated with the event.
eventDescription_applicationName :: Lens.Lens' EventDescription (Prelude.Maybe Prelude.Text)
eventDescription_applicationName = Lens.lens (\EventDescription' {applicationName} -> applicationName) (\s@EventDescription' {} a -> s {applicationName = a} :: EventDescription)

-- | The name of the environment associated with this event.
eventDescription_environmentName :: Lens.Lens' EventDescription (Prelude.Maybe Prelude.Text)
eventDescription_environmentName = Lens.lens (\EventDescription' {environmentName} -> environmentName) (\s@EventDescription' {} a -> s {environmentName = a} :: EventDescription)

-- | The date when the event occurred.
eventDescription_eventDate :: Lens.Lens' EventDescription (Prelude.Maybe Prelude.UTCTime)
eventDescription_eventDate = Lens.lens (\EventDescription' {eventDate} -> eventDate) (\s@EventDescription' {} a -> s {eventDate = a} :: EventDescription) Prelude.. Lens.mapping Data._Time

-- | The event message.
eventDescription_message :: Lens.Lens' EventDescription (Prelude.Maybe Prelude.Text)
eventDescription_message = Lens.lens (\EventDescription' {message} -> message) (\s@EventDescription' {} a -> s {message = a} :: EventDescription)

-- | The ARN of the platform version.
eventDescription_platformArn :: Lens.Lens' EventDescription (Prelude.Maybe Prelude.Text)
eventDescription_platformArn = Lens.lens (\EventDescription' {platformArn} -> platformArn) (\s@EventDescription' {} a -> s {platformArn = a} :: EventDescription)

-- | The web service request ID for the activity of this event.
eventDescription_requestId :: Lens.Lens' EventDescription (Prelude.Maybe Prelude.Text)
eventDescription_requestId = Lens.lens (\EventDescription' {requestId} -> requestId) (\s@EventDescription' {} a -> s {requestId = a} :: EventDescription)

-- | The severity level of this event.
eventDescription_severity :: Lens.Lens' EventDescription (Prelude.Maybe EventSeverity)
eventDescription_severity = Lens.lens (\EventDescription' {severity} -> severity) (\s@EventDescription' {} a -> s {severity = a} :: EventDescription)

-- | The name of the configuration associated with this event.
eventDescription_templateName :: Lens.Lens' EventDescription (Prelude.Maybe Prelude.Text)
eventDescription_templateName = Lens.lens (\EventDescription' {templateName} -> templateName) (\s@EventDescription' {} a -> s {templateName = a} :: EventDescription)

-- | The release label for the application version associated with this
-- event.
eventDescription_versionLabel :: Lens.Lens' EventDescription (Prelude.Maybe Prelude.Text)
eventDescription_versionLabel = Lens.lens (\EventDescription' {versionLabel} -> versionLabel) (\s@EventDescription' {} a -> s {versionLabel = a} :: EventDescription)

instance Data.FromXML EventDescription where
  parseXML x =
    EventDescription'
      Prelude.<$> (x Data..@? "ApplicationName")
      Prelude.<*> (x Data..@? "EnvironmentName")
      Prelude.<*> (x Data..@? "EventDate")
      Prelude.<*> (x Data..@? "Message")
      Prelude.<*> (x Data..@? "PlatformArn")
      Prelude.<*> (x Data..@? "RequestId")
      Prelude.<*> (x Data..@? "Severity")
      Prelude.<*> (x Data..@? "TemplateName")
      Prelude.<*> (x Data..@? "VersionLabel")

instance Prelude.Hashable EventDescription where
  hashWithSalt _salt EventDescription' {..} =
    _salt
      `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` eventDate
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` platformArn
      `Prelude.hashWithSalt` requestId
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` versionLabel

instance Prelude.NFData EventDescription where
  rnf EventDescription' {..} =
    Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf eventDate
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf platformArn
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf versionLabel
