{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Event
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Event
  ( Event (..),

    -- * Smart constructor
    mkEvent,

    -- * Lenses
    eEventType,
    eTimestamp,
    eAppPackageName,
    eAppTitle,
    eAppVersionCode,
    eAttributes,
    eClientSdkVersion,
    eMetrics,
    eSdkName,
    eSession,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.Session as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies information about an event that reports data to Amazon Pinpoint.
--
-- /See:/ 'mkEvent' smart constructor.
data Event = Event'
  { -- | The name of the event.
    eventType :: Core.Text,
    -- | The date and time, in ISO 8601 format, when the event occurred.
    timestamp :: Core.Text,
    -- | The package name of the app that's recording the event.
    appPackageName :: Core.Maybe Core.Text,
    -- | The title of the app that's recording the event.
    appTitle :: Core.Maybe Core.Text,
    -- | The version number of the app that's recording the event.
    appVersionCode :: Core.Maybe Core.Text,
    -- | One or more custom attributes that are associated with the event.
    attributes :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The version of the SDK that's running on the client device.
    clientSdkVersion :: Core.Maybe Core.Text,
    -- | One or more custom metrics that are associated with the event.
    metrics :: Core.Maybe (Core.HashMap Core.Text Core.Double),
    -- | The name of the SDK that's being used to record the event.
    sdkName :: Core.Maybe Core.Text,
    -- | Information about the session in which the event occurred.
    session :: Core.Maybe Types.Session
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Event' value with any optional fields omitted.
mkEvent ::
  -- | 'eventType'
  Core.Text ->
  -- | 'timestamp'
  Core.Text ->
  Event
mkEvent eventType timestamp =
  Event'
    { eventType,
      timestamp,
      appPackageName = Core.Nothing,
      appTitle = Core.Nothing,
      appVersionCode = Core.Nothing,
      attributes = Core.Nothing,
      clientSdkVersion = Core.Nothing,
      metrics = Core.Nothing,
      sdkName = Core.Nothing,
      session = Core.Nothing
    }

-- | The name of the event.
--
-- /Note:/ Consider using 'eventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventType :: Lens.Lens' Event Core.Text
eEventType = Lens.field @"eventType"
{-# DEPRECATED eEventType "Use generic-lens or generic-optics with 'eventType' instead." #-}

-- | The date and time, in ISO 8601 format, when the event occurred.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eTimestamp :: Lens.Lens' Event Core.Text
eTimestamp = Lens.field @"timestamp"
{-# DEPRECATED eTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The package name of the app that's recording the event.
--
-- /Note:/ Consider using 'appPackageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAppPackageName :: Lens.Lens' Event (Core.Maybe Core.Text)
eAppPackageName = Lens.field @"appPackageName"
{-# DEPRECATED eAppPackageName "Use generic-lens or generic-optics with 'appPackageName' instead." #-}

-- | The title of the app that's recording the event.
--
-- /Note:/ Consider using 'appTitle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAppTitle :: Lens.Lens' Event (Core.Maybe Core.Text)
eAppTitle = Lens.field @"appTitle"
{-# DEPRECATED eAppTitle "Use generic-lens or generic-optics with 'appTitle' instead." #-}

-- | The version number of the app that's recording the event.
--
-- /Note:/ Consider using 'appVersionCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAppVersionCode :: Lens.Lens' Event (Core.Maybe Core.Text)
eAppVersionCode = Lens.field @"appVersionCode"
{-# DEPRECATED eAppVersionCode "Use generic-lens or generic-optics with 'appVersionCode' instead." #-}

-- | One or more custom attributes that are associated with the event.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAttributes :: Lens.Lens' Event (Core.Maybe (Core.HashMap Core.Text Core.Text))
eAttributes = Lens.field @"attributes"
{-# DEPRECATED eAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The version of the SDK that's running on the client device.
--
-- /Note:/ Consider using 'clientSdkVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eClientSdkVersion :: Lens.Lens' Event (Core.Maybe Core.Text)
eClientSdkVersion = Lens.field @"clientSdkVersion"
{-# DEPRECATED eClientSdkVersion "Use generic-lens or generic-optics with 'clientSdkVersion' instead." #-}

-- | One or more custom metrics that are associated with the event.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMetrics :: Lens.Lens' Event (Core.Maybe (Core.HashMap Core.Text Core.Double))
eMetrics = Lens.field @"metrics"
{-# DEPRECATED eMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | The name of the SDK that's being used to record the event.
--
-- /Note:/ Consider using 'sdkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSdkName :: Lens.Lens' Event (Core.Maybe Core.Text)
eSdkName = Lens.field @"sdkName"
{-# DEPRECATED eSdkName "Use generic-lens or generic-optics with 'sdkName' instead." #-}

-- | Information about the session in which the event occurred.
--
-- /Note:/ Consider using 'session' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSession :: Lens.Lens' Event (Core.Maybe Types.Session)
eSession = Lens.field @"session"
{-# DEPRECATED eSession "Use generic-lens or generic-optics with 'session' instead." #-}

instance Core.FromJSON Event where
  toJSON Event {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("EventType" Core..= eventType),
            Core.Just ("Timestamp" Core..= timestamp),
            ("AppPackageName" Core..=) Core.<$> appPackageName,
            ("AppTitle" Core..=) Core.<$> appTitle,
            ("AppVersionCode" Core..=) Core.<$> appVersionCode,
            ("Attributes" Core..=) Core.<$> attributes,
            ("ClientSdkVersion" Core..=) Core.<$> clientSdkVersion,
            ("Metrics" Core..=) Core.<$> metrics,
            ("SdkName" Core..=) Core.<$> sdkName,
            ("Session" Core..=) Core.<$> session
          ]
      )
