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
-- Module      : Network.AWS.ElasticBeanstalk.Types.EventDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EventDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types.EventSeverity
import qualified Network.AWS.Lens as Lens

-- | Describes an event.
--
-- /See:/ 'newEventDescription' smart constructor.
data EventDescription = EventDescription'
  { -- | The name of the configuration associated with this event.
    templateName :: Core.Maybe Core.Text,
    -- | The severity level of this event.
    severity :: Core.Maybe EventSeverity,
    -- | The event message.
    message :: Core.Maybe Core.Text,
    -- | The date when the event occurred.
    eventDate :: Core.Maybe Core.ISO8601,
    -- | The name of the environment associated with this event.
    environmentName :: Core.Maybe Core.Text,
    -- | The ARN of the platform version.
    platformArn :: Core.Maybe Core.Text,
    -- | The release label for the application version associated with this
    -- event.
    versionLabel :: Core.Maybe Core.Text,
    -- | The web service request ID for the activity of this event.
    requestId :: Core.Maybe Core.Text,
    -- | The application associated with the event.
    applicationName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EventDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'eventDescription_templateName' - The name of the configuration associated with this event.
--
-- 'severity', 'eventDescription_severity' - The severity level of this event.
--
-- 'message', 'eventDescription_message' - The event message.
--
-- 'eventDate', 'eventDescription_eventDate' - The date when the event occurred.
--
-- 'environmentName', 'eventDescription_environmentName' - The name of the environment associated with this event.
--
-- 'platformArn', 'eventDescription_platformArn' - The ARN of the platform version.
--
-- 'versionLabel', 'eventDescription_versionLabel' - The release label for the application version associated with this
-- event.
--
-- 'requestId', 'eventDescription_requestId' - The web service request ID for the activity of this event.
--
-- 'applicationName', 'eventDescription_applicationName' - The application associated with the event.
newEventDescription ::
  EventDescription
newEventDescription =
  EventDescription'
    { templateName = Core.Nothing,
      severity = Core.Nothing,
      message = Core.Nothing,
      eventDate = Core.Nothing,
      environmentName = Core.Nothing,
      platformArn = Core.Nothing,
      versionLabel = Core.Nothing,
      requestId = Core.Nothing,
      applicationName = Core.Nothing
    }

-- | The name of the configuration associated with this event.
eventDescription_templateName :: Lens.Lens' EventDescription (Core.Maybe Core.Text)
eventDescription_templateName = Lens.lens (\EventDescription' {templateName} -> templateName) (\s@EventDescription' {} a -> s {templateName = a} :: EventDescription)

-- | The severity level of this event.
eventDescription_severity :: Lens.Lens' EventDescription (Core.Maybe EventSeverity)
eventDescription_severity = Lens.lens (\EventDescription' {severity} -> severity) (\s@EventDescription' {} a -> s {severity = a} :: EventDescription)

-- | The event message.
eventDescription_message :: Lens.Lens' EventDescription (Core.Maybe Core.Text)
eventDescription_message = Lens.lens (\EventDescription' {message} -> message) (\s@EventDescription' {} a -> s {message = a} :: EventDescription)

-- | The date when the event occurred.
eventDescription_eventDate :: Lens.Lens' EventDescription (Core.Maybe Core.UTCTime)
eventDescription_eventDate = Lens.lens (\EventDescription' {eventDate} -> eventDate) (\s@EventDescription' {} a -> s {eventDate = a} :: EventDescription) Core.. Lens.mapping Core._Time

-- | The name of the environment associated with this event.
eventDescription_environmentName :: Lens.Lens' EventDescription (Core.Maybe Core.Text)
eventDescription_environmentName = Lens.lens (\EventDescription' {environmentName} -> environmentName) (\s@EventDescription' {} a -> s {environmentName = a} :: EventDescription)

-- | The ARN of the platform version.
eventDescription_platformArn :: Lens.Lens' EventDescription (Core.Maybe Core.Text)
eventDescription_platformArn = Lens.lens (\EventDescription' {platformArn} -> platformArn) (\s@EventDescription' {} a -> s {platformArn = a} :: EventDescription)

-- | The release label for the application version associated with this
-- event.
eventDescription_versionLabel :: Lens.Lens' EventDescription (Core.Maybe Core.Text)
eventDescription_versionLabel = Lens.lens (\EventDescription' {versionLabel} -> versionLabel) (\s@EventDescription' {} a -> s {versionLabel = a} :: EventDescription)

-- | The web service request ID for the activity of this event.
eventDescription_requestId :: Lens.Lens' EventDescription (Core.Maybe Core.Text)
eventDescription_requestId = Lens.lens (\EventDescription' {requestId} -> requestId) (\s@EventDescription' {} a -> s {requestId = a} :: EventDescription)

-- | The application associated with the event.
eventDescription_applicationName :: Lens.Lens' EventDescription (Core.Maybe Core.Text)
eventDescription_applicationName = Lens.lens (\EventDescription' {applicationName} -> applicationName) (\s@EventDescription' {} a -> s {applicationName = a} :: EventDescription)

instance Core.FromXML EventDescription where
  parseXML x =
    EventDescription'
      Core.<$> (x Core..@? "TemplateName")
      Core.<*> (x Core..@? "Severity")
      Core.<*> (x Core..@? "Message")
      Core.<*> (x Core..@? "EventDate")
      Core.<*> (x Core..@? "EnvironmentName")
      Core.<*> (x Core..@? "PlatformArn")
      Core.<*> (x Core..@? "VersionLabel")
      Core.<*> (x Core..@? "RequestId")
      Core.<*> (x Core..@? "ApplicationName")

instance Core.Hashable EventDescription

instance Core.NFData EventDescription
