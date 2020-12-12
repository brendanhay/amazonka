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
    eClientSDKVersion,
    eMetrics,
    eAppVersionCode,
    eAppTitle,
    eAppPackageName,
    eAttributes,
    eSDKName,
    eSession,
    eEventType,
    eTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Session
import qualified Network.AWS.Prelude as Lude

-- | Specifies information about an event that reports data to Amazon Pinpoint.
--
-- /See:/ 'mkEvent' smart constructor.
data Event = Event'
  { clientSDKVersion :: Lude.Maybe Lude.Text,
    metrics :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Double)),
    appVersionCode :: Lude.Maybe Lude.Text,
    appTitle :: Lude.Maybe Lude.Text,
    appPackageName :: Lude.Maybe Lude.Text,
    attributes :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    sdkName :: Lude.Maybe Lude.Text,
    session :: Lude.Maybe Session,
    eventType :: Lude.Text,
    timestamp :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Event' with the minimum fields required to make a request.
--
-- * 'appPackageName' - The package name of the app that's recording the event.
-- * 'appTitle' - The title of the app that's recording the event.
-- * 'appVersionCode' - The version number of the app that's recording the event.
-- * 'attributes' - One or more custom attributes that are associated with the event.
-- * 'clientSDKVersion' - The version of the SDK that's running on the client device.
-- * 'eventType' - The name of the event.
-- * 'metrics' - One or more custom metrics that are associated with the event.
-- * 'sdkName' - The name of the SDK that's being used to record the event.
-- * 'session' - Information about the session in which the event occurred.
-- * 'timestamp' - The date and time, in ISO 8601 format, when the event occurred.
mkEvent ::
  -- | 'eventType'
  Lude.Text ->
  -- | 'timestamp'
  Lude.Text ->
  Event
mkEvent pEventType_ pTimestamp_ =
  Event'
    { clientSDKVersion = Lude.Nothing,
      metrics = Lude.Nothing,
      appVersionCode = Lude.Nothing,
      appTitle = Lude.Nothing,
      appPackageName = Lude.Nothing,
      attributes = Lude.Nothing,
      sdkName = Lude.Nothing,
      session = Lude.Nothing,
      eventType = pEventType_,
      timestamp = pTimestamp_
    }

-- | The version of the SDK that's running on the client device.
--
-- /Note:/ Consider using 'clientSDKVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eClientSDKVersion :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eClientSDKVersion = Lens.lens (clientSDKVersion :: Event -> Lude.Maybe Lude.Text) (\s a -> s {clientSDKVersion = a} :: Event)
{-# DEPRECATED eClientSDKVersion "Use generic-lens or generic-optics with 'clientSDKVersion' instead." #-}

-- | One or more custom metrics that are associated with the event.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMetrics :: Lens.Lens' Event (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Double)))
eMetrics = Lens.lens (metrics :: Event -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Double))) (\s a -> s {metrics = a} :: Event)
{-# DEPRECATED eMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | The version number of the app that's recording the event.
--
-- /Note:/ Consider using 'appVersionCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAppVersionCode :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eAppVersionCode = Lens.lens (appVersionCode :: Event -> Lude.Maybe Lude.Text) (\s a -> s {appVersionCode = a} :: Event)
{-# DEPRECATED eAppVersionCode "Use generic-lens or generic-optics with 'appVersionCode' instead." #-}

-- | The title of the app that's recording the event.
--
-- /Note:/ Consider using 'appTitle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAppTitle :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eAppTitle = Lens.lens (appTitle :: Event -> Lude.Maybe Lude.Text) (\s a -> s {appTitle = a} :: Event)
{-# DEPRECATED eAppTitle "Use generic-lens or generic-optics with 'appTitle' instead." #-}

-- | The package name of the app that's recording the event.
--
-- /Note:/ Consider using 'appPackageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAppPackageName :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eAppPackageName = Lens.lens (appPackageName :: Event -> Lude.Maybe Lude.Text) (\s a -> s {appPackageName = a} :: Event)
{-# DEPRECATED eAppPackageName "Use generic-lens or generic-optics with 'appPackageName' instead." #-}

-- | One or more custom attributes that are associated with the event.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAttributes :: Lens.Lens' Event (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
eAttributes = Lens.lens (attributes :: Event -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: Event)
{-# DEPRECATED eAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The name of the SDK that's being used to record the event.
--
-- /Note:/ Consider using 'sdkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSDKName :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eSDKName = Lens.lens (sdkName :: Event -> Lude.Maybe Lude.Text) (\s a -> s {sdkName = a} :: Event)
{-# DEPRECATED eSDKName "Use generic-lens or generic-optics with 'sdkName' instead." #-}

-- | Information about the session in which the event occurred.
--
-- /Note:/ Consider using 'session' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSession :: Lens.Lens' Event (Lude.Maybe Session)
eSession = Lens.lens (session :: Event -> Lude.Maybe Session) (\s a -> s {session = a} :: Event)
{-# DEPRECATED eSession "Use generic-lens or generic-optics with 'session' instead." #-}

-- | The name of the event.
--
-- /Note:/ Consider using 'eventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventType :: Lens.Lens' Event Lude.Text
eEventType = Lens.lens (eventType :: Event -> Lude.Text) (\s a -> s {eventType = a} :: Event)
{-# DEPRECATED eEventType "Use generic-lens or generic-optics with 'eventType' instead." #-}

-- | The date and time, in ISO 8601 format, when the event occurred.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eTimestamp :: Lens.Lens' Event Lude.Text
eTimestamp = Lens.lens (timestamp :: Event -> Lude.Text) (\s a -> s {timestamp = a} :: Event)
{-# DEPRECATED eTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.ToJSON Event where
  toJSON Event' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClientSdkVersion" Lude..=) Lude.<$> clientSDKVersion,
            ("Metrics" Lude..=) Lude.<$> metrics,
            ("AppVersionCode" Lude..=) Lude.<$> appVersionCode,
            ("AppTitle" Lude..=) Lude.<$> appTitle,
            ("AppPackageName" Lude..=) Lude.<$> appPackageName,
            ("Attributes" Lude..=) Lude.<$> attributes,
            ("SdkName" Lude..=) Lude.<$> sdkName,
            ("Session" Lude..=) Lude.<$> session,
            Lude.Just ("EventType" Lude..= eventType),
            Lude.Just ("Timestamp" Lude..= timestamp)
          ]
      )
