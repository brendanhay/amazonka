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
    edRequestId,
    edTemplateName,
    edSeverity,
    edVersionLabel,
    edPlatformARN,
    edEnvironmentName,
    edApplicationName,
    edEventDate,
    edMessage,
  )
where

import Network.AWS.ElasticBeanstalk.Types.EventSeverity
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an event.
--
-- /See:/ 'mkEventDescription' smart constructor.
data EventDescription = EventDescription'
  { requestId ::
      Lude.Maybe Lude.Text,
    templateName :: Lude.Maybe Lude.Text,
    severity :: Lude.Maybe EventSeverity,
    versionLabel :: Lude.Maybe Lude.Text,
    platformARN :: Lude.Maybe Lude.Text,
    environmentName :: Lude.Maybe Lude.Text,
    applicationName :: Lude.Maybe Lude.Text,
    eventDate :: Lude.Maybe Lude.DateTime,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventDescription' with the minimum fields required to make a request.
--
-- * 'applicationName' - The application associated with the event.
-- * 'environmentName' - The name of the environment associated with this event.
-- * 'eventDate' - The date when the event occurred.
-- * 'message' - The event message.
-- * 'platformARN' - The ARN of the platform version.
-- * 'requestId' - The web service request ID for the activity of this event.
-- * 'severity' - The severity level of this event.
-- * 'templateName' - The name of the configuration associated with this event.
-- * 'versionLabel' - The release label for the application version associated with this event.
mkEventDescription ::
  EventDescription
mkEventDescription =
  EventDescription'
    { requestId = Lude.Nothing,
      templateName = Lude.Nothing,
      severity = Lude.Nothing,
      versionLabel = Lude.Nothing,
      platformARN = Lude.Nothing,
      environmentName = Lude.Nothing,
      applicationName = Lude.Nothing,
      eventDate = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The web service request ID for the activity of this event.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edRequestId :: Lens.Lens' EventDescription (Lude.Maybe Lude.Text)
edRequestId = Lens.lens (requestId :: EventDescription -> Lude.Maybe Lude.Text) (\s a -> s {requestId = a} :: EventDescription)
{-# DEPRECATED edRequestId "Use generic-lens or generic-optics with 'requestId' instead." #-}

-- | The name of the configuration associated with this event.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edTemplateName :: Lens.Lens' EventDescription (Lude.Maybe Lude.Text)
edTemplateName = Lens.lens (templateName :: EventDescription -> Lude.Maybe Lude.Text) (\s a -> s {templateName = a} :: EventDescription)
{-# DEPRECATED edTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The severity level of this event.
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edSeverity :: Lens.Lens' EventDescription (Lude.Maybe EventSeverity)
edSeverity = Lens.lens (severity :: EventDescription -> Lude.Maybe EventSeverity) (\s a -> s {severity = a} :: EventDescription)
{-# DEPRECATED edSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | The release label for the application version associated with this event.
--
-- /Note:/ Consider using 'versionLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edVersionLabel :: Lens.Lens' EventDescription (Lude.Maybe Lude.Text)
edVersionLabel = Lens.lens (versionLabel :: EventDescription -> Lude.Maybe Lude.Text) (\s a -> s {versionLabel = a} :: EventDescription)
{-# DEPRECATED edVersionLabel "Use generic-lens or generic-optics with 'versionLabel' instead." #-}

-- | The ARN of the platform version.
--
-- /Note:/ Consider using 'platformARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edPlatformARN :: Lens.Lens' EventDescription (Lude.Maybe Lude.Text)
edPlatformARN = Lens.lens (platformARN :: EventDescription -> Lude.Maybe Lude.Text) (\s a -> s {platformARN = a} :: EventDescription)
{-# DEPRECATED edPlatformARN "Use generic-lens or generic-optics with 'platformARN' instead." #-}

-- | The name of the environment associated with this event.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edEnvironmentName :: Lens.Lens' EventDescription (Lude.Maybe Lude.Text)
edEnvironmentName = Lens.lens (environmentName :: EventDescription -> Lude.Maybe Lude.Text) (\s a -> s {environmentName = a} :: EventDescription)
{-# DEPRECATED edEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The application associated with the event.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edApplicationName :: Lens.Lens' EventDescription (Lude.Maybe Lude.Text)
edApplicationName = Lens.lens (applicationName :: EventDescription -> Lude.Maybe Lude.Text) (\s a -> s {applicationName = a} :: EventDescription)
{-# DEPRECATED edApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The date when the event occurred.
--
-- /Note:/ Consider using 'eventDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edEventDate :: Lens.Lens' EventDescription (Lude.Maybe Lude.DateTime)
edEventDate = Lens.lens (eventDate :: EventDescription -> Lude.Maybe Lude.DateTime) (\s a -> s {eventDate = a} :: EventDescription)
{-# DEPRECATED edEventDate "Use generic-lens or generic-optics with 'eventDate' instead." #-}

-- | The event message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edMessage :: Lens.Lens' EventDescription (Lude.Maybe Lude.Text)
edMessage = Lens.lens (message :: EventDescription -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: EventDescription)
{-# DEPRECATED edMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML EventDescription where
  parseXML x =
    EventDescription'
      Lude.<$> (x Lude..@? "RequestId")
      Lude.<*> (x Lude..@? "TemplateName")
      Lude.<*> (x Lude..@? "Severity")
      Lude.<*> (x Lude..@? "VersionLabel")
      Lude.<*> (x Lude..@? "PlatformArn")
      Lude.<*> (x Lude..@? "EnvironmentName")
      Lude.<*> (x Lude..@? "ApplicationName")
      Lude.<*> (x Lude..@? "EventDate")
      Lude.<*> (x Lude..@? "Message")
