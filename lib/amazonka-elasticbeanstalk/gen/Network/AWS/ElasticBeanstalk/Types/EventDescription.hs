{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EventDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EventDescription
  ( EventDescription (..),

    -- * Smart constructor
    mkEventDescription,

    -- * Lenses
    edApplicationName,
    edEnvironmentName,
    edEventDate,
    edMessage,
    edPlatformArn,
    edRequestId,
    edSeverity,
    edTemplateName,
    edVersionLabel,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.ApplicationName as Types
import qualified Network.AWS.ElasticBeanstalk.Types.EnvironmentName as Types
import qualified Network.AWS.ElasticBeanstalk.Types.EventSeverity as Types
import qualified Network.AWS.ElasticBeanstalk.Types.Message as Types
import qualified Network.AWS.ElasticBeanstalk.Types.PlatformArn as Types
import qualified Network.AWS.ElasticBeanstalk.Types.RequestId as Types
import qualified Network.AWS.ElasticBeanstalk.Types.TemplateName as Types
import qualified Network.AWS.ElasticBeanstalk.Types.VersionLabel as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an event.
--
-- /See:/ 'mkEventDescription' smart constructor.
data EventDescription = EventDescription'
  { -- | The application associated with the event.
    applicationName :: Core.Maybe Types.ApplicationName,
    -- | The name of the environment associated with this event.
    environmentName :: Core.Maybe Types.EnvironmentName,
    -- | The date when the event occurred.
    eventDate :: Core.Maybe Core.UTCTime,
    -- | The event message.
    message :: Core.Maybe Types.Message,
    -- | The ARN of the platform version.
    platformArn :: Core.Maybe Types.PlatformArn,
    -- | The web service request ID for the activity of this event.
    requestId :: Core.Maybe Types.RequestId,
    -- | The severity level of this event.
    severity :: Core.Maybe Types.EventSeverity,
    -- | The name of the configuration associated with this event.
    templateName :: Core.Maybe Types.TemplateName,
    -- | The release label for the application version associated with this event.
    versionLabel :: Core.Maybe Types.VersionLabel
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'EventDescription' value with any optional fields omitted.
mkEventDescription ::
  EventDescription
mkEventDescription =
  EventDescription'
    { applicationName = Core.Nothing,
      environmentName = Core.Nothing,
      eventDate = Core.Nothing,
      message = Core.Nothing,
      platformArn = Core.Nothing,
      requestId = Core.Nothing,
      severity = Core.Nothing,
      templateName = Core.Nothing,
      versionLabel = Core.Nothing
    }

-- | The application associated with the event.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edApplicationName :: Lens.Lens' EventDescription (Core.Maybe Types.ApplicationName)
edApplicationName = Lens.field @"applicationName"
{-# DEPRECATED edApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The name of the environment associated with this event.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edEnvironmentName :: Lens.Lens' EventDescription (Core.Maybe Types.EnvironmentName)
edEnvironmentName = Lens.field @"environmentName"
{-# DEPRECATED edEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The date when the event occurred.
--
-- /Note:/ Consider using 'eventDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edEventDate :: Lens.Lens' EventDescription (Core.Maybe Core.UTCTime)
edEventDate = Lens.field @"eventDate"
{-# DEPRECATED edEventDate "Use generic-lens or generic-optics with 'eventDate' instead." #-}

-- | The event message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edMessage :: Lens.Lens' EventDescription (Core.Maybe Types.Message)
edMessage = Lens.field @"message"
{-# DEPRECATED edMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The ARN of the platform version.
--
-- /Note:/ Consider using 'platformArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edPlatformArn :: Lens.Lens' EventDescription (Core.Maybe Types.PlatformArn)
edPlatformArn = Lens.field @"platformArn"
{-# DEPRECATED edPlatformArn "Use generic-lens or generic-optics with 'platformArn' instead." #-}

-- | The web service request ID for the activity of this event.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edRequestId :: Lens.Lens' EventDescription (Core.Maybe Types.RequestId)
edRequestId = Lens.field @"requestId"
{-# DEPRECATED edRequestId "Use generic-lens or generic-optics with 'requestId' instead." #-}

-- | The severity level of this event.
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edSeverity :: Lens.Lens' EventDescription (Core.Maybe Types.EventSeverity)
edSeverity = Lens.field @"severity"
{-# DEPRECATED edSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | The name of the configuration associated with this event.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edTemplateName :: Lens.Lens' EventDescription (Core.Maybe Types.TemplateName)
edTemplateName = Lens.field @"templateName"
{-# DEPRECATED edTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The release label for the application version associated with this event.
--
-- /Note:/ Consider using 'versionLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edVersionLabel :: Lens.Lens' EventDescription (Core.Maybe Types.VersionLabel)
edVersionLabel = Lens.field @"versionLabel"
{-# DEPRECATED edVersionLabel "Use generic-lens or generic-optics with 'versionLabel' instead." #-}

instance Core.FromXML EventDescription where
  parseXML x =
    EventDescription'
      Core.<$> (x Core..@? "ApplicationName")
      Core.<*> (x Core..@? "EnvironmentName")
      Core.<*> (x Core..@? "EventDate")
      Core.<*> (x Core..@? "Message")
      Core.<*> (x Core..@? "PlatformArn")
      Core.<*> (x Core..@? "RequestId")
      Core.<*> (x Core..@? "Severity")
      Core.<*> (x Core..@? "TemplateName")
      Core.<*> (x Core..@? "VersionLabel")
