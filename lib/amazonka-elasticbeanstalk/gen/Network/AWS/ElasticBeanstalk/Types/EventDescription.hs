{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EventDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.EventDescription
  ( EventDescription (..)
  -- * Smart constructor
  , mkEventDescription
  -- * Lenses
  , edApplicationName
  , edEnvironmentName
  , edEventDate
  , edMessage
  , edPlatformArn
  , edRequestId
  , edSeverity
  , edTemplateName
  , edVersionLabel
  ) where

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
  { applicationName :: Core.Maybe Types.ApplicationName
    -- ^ The application associated with the event.
  , environmentName :: Core.Maybe Types.EnvironmentName
    -- ^ The name of the environment associated with this event.
  , eventDate :: Core.Maybe Core.UTCTime
    -- ^ The date when the event occurred.
  , message :: Core.Maybe Types.Message
    -- ^ The event message.
  , platformArn :: Core.Maybe Types.PlatformArn
    -- ^ The ARN of the platform version.
  , requestId :: Core.Maybe Types.RequestId
    -- ^ The web service request ID for the activity of this event.
  , severity :: Core.Maybe Types.EventSeverity
    -- ^ The severity level of this event.
  , templateName :: Core.Maybe Types.TemplateName
    -- ^ The name of the configuration associated with this event.
  , versionLabel :: Core.Maybe Types.VersionLabel
    -- ^ The release label for the application version associated with this event.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'EventDescription' value with any optional fields omitted.
mkEventDescription
    :: EventDescription
mkEventDescription
  = EventDescription'{applicationName = Core.Nothing,
                      environmentName = Core.Nothing, eventDate = Core.Nothing,
                      message = Core.Nothing, platformArn = Core.Nothing,
                      requestId = Core.Nothing, severity = Core.Nothing,
                      templateName = Core.Nothing, versionLabel = Core.Nothing}

-- | The application associated with the event.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edApplicationName :: Lens.Lens' EventDescription (Core.Maybe Types.ApplicationName)
edApplicationName = Lens.field @"applicationName"
{-# INLINEABLE edApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | The name of the environment associated with this event.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edEnvironmentName :: Lens.Lens' EventDescription (Core.Maybe Types.EnvironmentName)
edEnvironmentName = Lens.field @"environmentName"
{-# INLINEABLE edEnvironmentName #-}
{-# DEPRECATED environmentName "Use generic-lens or generic-optics with 'environmentName' instead"  #-}

-- | The date when the event occurred.
--
-- /Note:/ Consider using 'eventDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edEventDate :: Lens.Lens' EventDescription (Core.Maybe Core.UTCTime)
edEventDate = Lens.field @"eventDate"
{-# INLINEABLE edEventDate #-}
{-# DEPRECATED eventDate "Use generic-lens or generic-optics with 'eventDate' instead"  #-}

-- | The event message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edMessage :: Lens.Lens' EventDescription (Core.Maybe Types.Message)
edMessage = Lens.field @"message"
{-# INLINEABLE edMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | The ARN of the platform version.
--
-- /Note:/ Consider using 'platformArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edPlatformArn :: Lens.Lens' EventDescription (Core.Maybe Types.PlatformArn)
edPlatformArn = Lens.field @"platformArn"
{-# INLINEABLE edPlatformArn #-}
{-# DEPRECATED platformArn "Use generic-lens or generic-optics with 'platformArn' instead"  #-}

-- | The web service request ID for the activity of this event.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edRequestId :: Lens.Lens' EventDescription (Core.Maybe Types.RequestId)
edRequestId = Lens.field @"requestId"
{-# INLINEABLE edRequestId #-}
{-# DEPRECATED requestId "Use generic-lens or generic-optics with 'requestId' instead"  #-}

-- | The severity level of this event.
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edSeverity :: Lens.Lens' EventDescription (Core.Maybe Types.EventSeverity)
edSeverity = Lens.field @"severity"
{-# INLINEABLE edSeverity #-}
{-# DEPRECATED severity "Use generic-lens or generic-optics with 'severity' instead"  #-}

-- | The name of the configuration associated with this event.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edTemplateName :: Lens.Lens' EventDescription (Core.Maybe Types.TemplateName)
edTemplateName = Lens.field @"templateName"
{-# INLINEABLE edTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

-- | The release label for the application version associated with this event.
--
-- /Note:/ Consider using 'versionLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edVersionLabel :: Lens.Lens' EventDescription (Core.Maybe Types.VersionLabel)
edVersionLabel = Lens.field @"versionLabel"
{-# INLINEABLE edVersionLabel #-}
{-# DEPRECATED versionLabel "Use generic-lens or generic-optics with 'versionLabel' instead"  #-}

instance Core.FromXML EventDescription where
        parseXML x
          = EventDescription' Core.<$>
              (x Core..@? "ApplicationName") Core.<*>
                x Core..@? "EnvironmentName"
                Core.<*> x Core..@? "EventDate"
                Core.<*> x Core..@? "Message"
                Core.<*> x Core..@? "PlatformArn"
                Core.<*> x Core..@? "RequestId"
                Core.<*> x Core..@? "Severity"
                Core.<*> x Core..@? "TemplateName"
                Core.<*> x Core..@? "VersionLabel"
