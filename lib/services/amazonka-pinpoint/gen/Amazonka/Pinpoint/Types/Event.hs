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
-- Module      : Amazonka.Pinpoint.Types.Event
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.Event where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.Session
import qualified Amazonka.Prelude as Prelude

-- | Specifies information about an event that reports data to Amazon
-- Pinpoint.
--
-- /See:/ 'newEvent' smart constructor.
data Event = Event'
  { -- | The package name of the app that\'s recording the event.
    appPackageName :: Prelude.Maybe Prelude.Text,
    -- | The title of the app that\'s recording the event.
    appTitle :: Prelude.Maybe Prelude.Text,
    -- | The version number of the app that\'s recording the event.
    appVersionCode :: Prelude.Maybe Prelude.Text,
    -- | One or more custom attributes that are associated with the event.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The version of the SDK that\'s running on the client device.
    clientSdkVersion :: Prelude.Maybe Prelude.Text,
    -- | One or more custom metrics that are associated with the event.
    metrics :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double),
    -- | The name of the SDK that\'s being used to record the event.
    sdkName :: Prelude.Maybe Prelude.Text,
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
-- 'appPackageName', 'event_appPackageName' - The package name of the app that\'s recording the event.
--
-- 'appTitle', 'event_appTitle' - The title of the app that\'s recording the event.
--
-- 'appVersionCode', 'event_appVersionCode' - The version number of the app that\'s recording the event.
--
-- 'attributes', 'event_attributes' - One or more custom attributes that are associated with the event.
--
-- 'clientSdkVersion', 'event_clientSdkVersion' - The version of the SDK that\'s running on the client device.
--
-- 'metrics', 'event_metrics' - One or more custom metrics that are associated with the event.
--
-- 'sdkName', 'event_sdkName' - The name of the SDK that\'s being used to record the event.
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
    { appPackageName = Prelude.Nothing,
      appTitle = Prelude.Nothing,
      appVersionCode = Prelude.Nothing,
      attributes = Prelude.Nothing,
      clientSdkVersion = Prelude.Nothing,
      metrics = Prelude.Nothing,
      sdkName = Prelude.Nothing,
      session = Prelude.Nothing,
      eventType = pEventType_,
      timestamp = pTimestamp_
    }

-- | The package name of the app that\'s recording the event.
event_appPackageName :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_appPackageName = Lens.lens (\Event' {appPackageName} -> appPackageName) (\s@Event' {} a -> s {appPackageName = a} :: Event)

-- | The title of the app that\'s recording the event.
event_appTitle :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_appTitle = Lens.lens (\Event' {appTitle} -> appTitle) (\s@Event' {} a -> s {appTitle = a} :: Event)

-- | The version number of the app that\'s recording the event.
event_appVersionCode :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_appVersionCode = Lens.lens (\Event' {appVersionCode} -> appVersionCode) (\s@Event' {} a -> s {appVersionCode = a} :: Event)

-- | One or more custom attributes that are associated with the event.
event_attributes :: Lens.Lens' Event (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
event_attributes = Lens.lens (\Event' {attributes} -> attributes) (\s@Event' {} a -> s {attributes = a} :: Event) Prelude.. Lens.mapping Lens.coerced

-- | The version of the SDK that\'s running on the client device.
event_clientSdkVersion :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_clientSdkVersion = Lens.lens (\Event' {clientSdkVersion} -> clientSdkVersion) (\s@Event' {} a -> s {clientSdkVersion = a} :: Event)

-- | One or more custom metrics that are associated with the event.
event_metrics :: Lens.Lens' Event (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double))
event_metrics = Lens.lens (\Event' {metrics} -> metrics) (\s@Event' {} a -> s {metrics = a} :: Event) Prelude.. Lens.mapping Lens.coerced

-- | The name of the SDK that\'s being used to record the event.
event_sdkName :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_sdkName = Lens.lens (\Event' {sdkName} -> sdkName) (\s@Event' {} a -> s {sdkName = a} :: Event)

-- | Information about the session in which the event occurred.
event_session :: Lens.Lens' Event (Prelude.Maybe Session)
event_session = Lens.lens (\Event' {session} -> session) (\s@Event' {} a -> s {session = a} :: Event)

-- | The name of the event.
event_eventType :: Lens.Lens' Event Prelude.Text
event_eventType = Lens.lens (\Event' {eventType} -> eventType) (\s@Event' {} a -> s {eventType = a} :: Event)

-- | The date and time, in ISO 8601 format, when the event occurred.
event_timestamp :: Lens.Lens' Event Prelude.Text
event_timestamp = Lens.lens (\Event' {timestamp} -> timestamp) (\s@Event' {} a -> s {timestamp = a} :: Event)

instance Prelude.Hashable Event where
  hashWithSalt _salt Event' {..} =
    _salt
      `Prelude.hashWithSalt` appPackageName
      `Prelude.hashWithSalt` appTitle
      `Prelude.hashWithSalt` appVersionCode
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` clientSdkVersion
      `Prelude.hashWithSalt` metrics
      `Prelude.hashWithSalt` sdkName
      `Prelude.hashWithSalt` session
      `Prelude.hashWithSalt` eventType
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData Event where
  rnf Event' {..} =
    Prelude.rnf appPackageName `Prelude.seq`
      Prelude.rnf appTitle `Prelude.seq`
        Prelude.rnf appVersionCode `Prelude.seq`
          Prelude.rnf attributes `Prelude.seq`
            Prelude.rnf clientSdkVersion `Prelude.seq`
              Prelude.rnf metrics `Prelude.seq`
                Prelude.rnf sdkName `Prelude.seq`
                  Prelude.rnf session `Prelude.seq`
                    Prelude.rnf eventType `Prelude.seq`
                      Prelude.rnf timestamp

instance Data.ToJSON Event where
  toJSON Event' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AppPackageName" Data..=)
              Prelude.<$> appPackageName,
            ("AppTitle" Data..=) Prelude.<$> appTitle,
            ("AppVersionCode" Data..=)
              Prelude.<$> appVersionCode,
            ("Attributes" Data..=) Prelude.<$> attributes,
            ("ClientSdkVersion" Data..=)
              Prelude.<$> clientSdkVersion,
            ("Metrics" Data..=) Prelude.<$> metrics,
            ("SdkName" Data..=) Prelude.<$> sdkName,
            ("Session" Data..=) Prelude.<$> session,
            Prelude.Just ("EventType" Data..= eventType),
            Prelude.Just ("Timestamp" Data..= timestamp)
          ]
      )
