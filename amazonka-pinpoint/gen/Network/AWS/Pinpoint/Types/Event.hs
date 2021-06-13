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
-- Module      : Network.AWS.Pinpoint.Types.Event
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Event where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Session
import qualified Network.AWS.Prelude as Prelude

-- | Specifies information about an event that reports data to Amazon
-- Pinpoint.
--
-- /See:/ 'newEvent' smart constructor.
data Event = Event'
  { -- | The version of the SDK that\'s running on the client device.
    clientSdkVersion :: Prelude.Maybe Prelude.Text,
    -- | The title of the app that\'s recording the event.
    appTitle :: Prelude.Maybe Prelude.Text,
    -- | The name of the SDK that\'s being used to record the event.
    sdkName :: Prelude.Maybe Prelude.Text,
    -- | One or more custom attributes that are associated with the event.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | One or more custom metrics that are associated with the event.
    metrics :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double),
    -- | The package name of the app that\'s recording the event.
    appPackageName :: Prelude.Maybe Prelude.Text,
    -- | The version number of the app that\'s recording the event.
    appVersionCode :: Prelude.Maybe Prelude.Text,
    -- | Information about the session in which the event occurred.
    session :: Prelude.Maybe Session,
    -- | The name of the event.
    eventType :: Prelude.Text,
    -- | The date and time, in ISO 8601 format, when the event occurred.
    timestamp :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Event' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientSdkVersion', 'event_clientSdkVersion' - The version of the SDK that\'s running on the client device.
--
-- 'appTitle', 'event_appTitle' - The title of the app that\'s recording the event.
--
-- 'sdkName', 'event_sdkName' - The name of the SDK that\'s being used to record the event.
--
-- 'attributes', 'event_attributes' - One or more custom attributes that are associated with the event.
--
-- 'metrics', 'event_metrics' - One or more custom metrics that are associated with the event.
--
-- 'appPackageName', 'event_appPackageName' - The package name of the app that\'s recording the event.
--
-- 'appVersionCode', 'event_appVersionCode' - The version number of the app that\'s recording the event.
--
-- 'session', 'event_session' - Information about the session in which the event occurred.
--
-- 'eventType', 'event_eventType' - The name of the event.
--
-- 'timestamp', 'event_timestamp' - The date and time, in ISO 8601 format, when the event occurred.
newEvent ::
  -- | 'eventType'
  Prelude.Text ->
  -- | 'timestamp'
  Prelude.Text ->
  Event
newEvent pEventType_ pTimestamp_ =
  Event'
    { clientSdkVersion = Prelude.Nothing,
      appTitle = Prelude.Nothing,
      sdkName = Prelude.Nothing,
      attributes = Prelude.Nothing,
      metrics = Prelude.Nothing,
      appPackageName = Prelude.Nothing,
      appVersionCode = Prelude.Nothing,
      session = Prelude.Nothing,
      eventType = pEventType_,
      timestamp = pTimestamp_
    }

-- | The version of the SDK that\'s running on the client device.
event_clientSdkVersion :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_clientSdkVersion = Lens.lens (\Event' {clientSdkVersion} -> clientSdkVersion) (\s@Event' {} a -> s {clientSdkVersion = a} :: Event)

-- | The title of the app that\'s recording the event.
event_appTitle :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_appTitle = Lens.lens (\Event' {appTitle} -> appTitle) (\s@Event' {} a -> s {appTitle = a} :: Event)

-- | The name of the SDK that\'s being used to record the event.
event_sdkName :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_sdkName = Lens.lens (\Event' {sdkName} -> sdkName) (\s@Event' {} a -> s {sdkName = a} :: Event)

-- | One or more custom attributes that are associated with the event.
event_attributes :: Lens.Lens' Event (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
event_attributes = Lens.lens (\Event' {attributes} -> attributes) (\s@Event' {} a -> s {attributes = a} :: Event) Prelude.. Lens.mapping Lens._Coerce

-- | One or more custom metrics that are associated with the event.
event_metrics :: Lens.Lens' Event (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double))
event_metrics = Lens.lens (\Event' {metrics} -> metrics) (\s@Event' {} a -> s {metrics = a} :: Event) Prelude.. Lens.mapping Lens._Coerce

-- | The package name of the app that\'s recording the event.
event_appPackageName :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_appPackageName = Lens.lens (\Event' {appPackageName} -> appPackageName) (\s@Event' {} a -> s {appPackageName = a} :: Event)

-- | The version number of the app that\'s recording the event.
event_appVersionCode :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_appVersionCode = Lens.lens (\Event' {appVersionCode} -> appVersionCode) (\s@Event' {} a -> s {appVersionCode = a} :: Event)

-- | Information about the session in which the event occurred.
event_session :: Lens.Lens' Event (Prelude.Maybe Session)
event_session = Lens.lens (\Event' {session} -> session) (\s@Event' {} a -> s {session = a} :: Event)

-- | The name of the event.
event_eventType :: Lens.Lens' Event Prelude.Text
event_eventType = Lens.lens (\Event' {eventType} -> eventType) (\s@Event' {} a -> s {eventType = a} :: Event)

-- | The date and time, in ISO 8601 format, when the event occurred.
event_timestamp :: Lens.Lens' Event Prelude.Text
event_timestamp = Lens.lens (\Event' {timestamp} -> timestamp) (\s@Event' {} a -> s {timestamp = a} :: Event)

instance Prelude.Hashable Event

instance Prelude.NFData Event

instance Core.ToJSON Event where
  toJSON Event' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ClientSdkVersion" Core..=)
              Prelude.<$> clientSdkVersion,
            ("AppTitle" Core..=) Prelude.<$> appTitle,
            ("SdkName" Core..=) Prelude.<$> sdkName,
            ("Attributes" Core..=) Prelude.<$> attributes,
            ("Metrics" Core..=) Prelude.<$> metrics,
            ("AppPackageName" Core..=)
              Prelude.<$> appPackageName,
            ("AppVersionCode" Core..=)
              Prelude.<$> appVersionCode,
            ("Session" Core..=) Prelude.<$> session,
            Prelude.Just ("EventType" Core..= eventType),
            Prelude.Just ("Timestamp" Core..= timestamp)
          ]
      )
