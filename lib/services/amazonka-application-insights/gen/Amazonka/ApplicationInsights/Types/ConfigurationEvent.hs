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
-- Module      : Amazonka.ApplicationInsights.Types.ConfigurationEvent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationInsights.Types.ConfigurationEvent where

import Amazonka.ApplicationInsights.Types.ConfigurationEventResourceType
import Amazonka.ApplicationInsights.Types.ConfigurationEventStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The event information.
--
-- /See:/ 'newConfigurationEvent' smart constructor.
data ConfigurationEvent = ConfigurationEvent'
  { -- | The name of the resource Application Insights attempted to configure.
    eventResourceName :: Prelude.Maybe Prelude.Text,
    -- | The resource monitored by Application Insights.
    monitoredResourceARN :: Prelude.Maybe Prelude.Text,
    -- | The status of the configuration update event. Possible values include
    -- INFO, WARN, and ERROR.
    eventStatus :: Prelude.Maybe ConfigurationEventStatus,
    -- | The resource type that Application Insights attempted to configure, for
    -- example, CLOUDWATCH_ALARM.
    eventResourceType :: Prelude.Maybe ConfigurationEventResourceType,
    -- | The timestamp of the event.
    eventTime :: Prelude.Maybe Core.POSIX,
    -- | The details of the event in plain text.
    eventDetail :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigurationEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventResourceName', 'configurationEvent_eventResourceName' - The name of the resource Application Insights attempted to configure.
--
-- 'monitoredResourceARN', 'configurationEvent_monitoredResourceARN' - The resource monitored by Application Insights.
--
-- 'eventStatus', 'configurationEvent_eventStatus' - The status of the configuration update event. Possible values include
-- INFO, WARN, and ERROR.
--
-- 'eventResourceType', 'configurationEvent_eventResourceType' - The resource type that Application Insights attempted to configure, for
-- example, CLOUDWATCH_ALARM.
--
-- 'eventTime', 'configurationEvent_eventTime' - The timestamp of the event.
--
-- 'eventDetail', 'configurationEvent_eventDetail' - The details of the event in plain text.
newConfigurationEvent ::
  ConfigurationEvent
newConfigurationEvent =
  ConfigurationEvent'
    { eventResourceName =
        Prelude.Nothing,
      monitoredResourceARN = Prelude.Nothing,
      eventStatus = Prelude.Nothing,
      eventResourceType = Prelude.Nothing,
      eventTime = Prelude.Nothing,
      eventDetail = Prelude.Nothing
    }

-- | The name of the resource Application Insights attempted to configure.
configurationEvent_eventResourceName :: Lens.Lens' ConfigurationEvent (Prelude.Maybe Prelude.Text)
configurationEvent_eventResourceName = Lens.lens (\ConfigurationEvent' {eventResourceName} -> eventResourceName) (\s@ConfigurationEvent' {} a -> s {eventResourceName = a} :: ConfigurationEvent)

-- | The resource monitored by Application Insights.
configurationEvent_monitoredResourceARN :: Lens.Lens' ConfigurationEvent (Prelude.Maybe Prelude.Text)
configurationEvent_monitoredResourceARN = Lens.lens (\ConfigurationEvent' {monitoredResourceARN} -> monitoredResourceARN) (\s@ConfigurationEvent' {} a -> s {monitoredResourceARN = a} :: ConfigurationEvent)

-- | The status of the configuration update event. Possible values include
-- INFO, WARN, and ERROR.
configurationEvent_eventStatus :: Lens.Lens' ConfigurationEvent (Prelude.Maybe ConfigurationEventStatus)
configurationEvent_eventStatus = Lens.lens (\ConfigurationEvent' {eventStatus} -> eventStatus) (\s@ConfigurationEvent' {} a -> s {eventStatus = a} :: ConfigurationEvent)

-- | The resource type that Application Insights attempted to configure, for
-- example, CLOUDWATCH_ALARM.
configurationEvent_eventResourceType :: Lens.Lens' ConfigurationEvent (Prelude.Maybe ConfigurationEventResourceType)
configurationEvent_eventResourceType = Lens.lens (\ConfigurationEvent' {eventResourceType} -> eventResourceType) (\s@ConfigurationEvent' {} a -> s {eventResourceType = a} :: ConfigurationEvent)

-- | The timestamp of the event.
configurationEvent_eventTime :: Lens.Lens' ConfigurationEvent (Prelude.Maybe Prelude.UTCTime)
configurationEvent_eventTime = Lens.lens (\ConfigurationEvent' {eventTime} -> eventTime) (\s@ConfigurationEvent' {} a -> s {eventTime = a} :: ConfigurationEvent) Prelude.. Lens.mapping Core._Time

-- | The details of the event in plain text.
configurationEvent_eventDetail :: Lens.Lens' ConfigurationEvent (Prelude.Maybe Prelude.Text)
configurationEvent_eventDetail = Lens.lens (\ConfigurationEvent' {eventDetail} -> eventDetail) (\s@ConfigurationEvent' {} a -> s {eventDetail = a} :: ConfigurationEvent)

instance Core.FromJSON ConfigurationEvent where
  parseJSON =
    Core.withObject
      "ConfigurationEvent"
      ( \x ->
          ConfigurationEvent'
            Prelude.<$> (x Core..:? "EventResourceName")
            Prelude.<*> (x Core..:? "MonitoredResourceARN")
            Prelude.<*> (x Core..:? "EventStatus")
            Prelude.<*> (x Core..:? "EventResourceType")
            Prelude.<*> (x Core..:? "EventTime")
            Prelude.<*> (x Core..:? "EventDetail")
      )

instance Prelude.Hashable ConfigurationEvent where
  hashWithSalt _salt ConfigurationEvent' {..} =
    _salt `Prelude.hashWithSalt` eventResourceName
      `Prelude.hashWithSalt` monitoredResourceARN
      `Prelude.hashWithSalt` eventStatus
      `Prelude.hashWithSalt` eventResourceType
      `Prelude.hashWithSalt` eventTime
      `Prelude.hashWithSalt` eventDetail

instance Prelude.NFData ConfigurationEvent where
  rnf ConfigurationEvent' {..} =
    Prelude.rnf eventResourceName
      `Prelude.seq` Prelude.rnf monitoredResourceARN
      `Prelude.seq` Prelude.rnf eventStatus
      `Prelude.seq` Prelude.rnf eventResourceType
      `Prelude.seq` Prelude.rnf eventTime
      `Prelude.seq` Prelude.rnf eventDetail
