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
-- Module      : Amazonka.Rum.Types.AppMonitor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rum.Types.AppMonitor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rum.Types.AppMonitorConfiguration
import Amazonka.Rum.Types.CustomEvents
import Amazonka.Rum.Types.DataStorage
import Amazonka.Rum.Types.StateEnum

-- | A RUM app monitor collects telemetry data from your application and
-- sends that data to RUM. The data includes performance and reliability
-- information such as page load time, client-side errors, and user
-- behavior.
--
-- /See:/ 'newAppMonitor' smart constructor.
data AppMonitor = AppMonitor'
  { -- | A structure that contains much of the configuration data for the app
    -- monitor.
    appMonitorConfiguration :: Prelude.Maybe AppMonitorConfiguration,
    -- | The date and time that this app monitor was created.
    created :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether this app monitor allows the web client to define and
    -- send custom events.
    --
    -- For more information about custom events, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-RUM-custom-events.html Send custom events>.
    customEvents :: Prelude.Maybe CustomEvents,
    -- | A structure that contains information about whether this app monitor
    -- stores a copy of the telemetry data that RUM collects using CloudWatch
    -- Logs.
    dataStorage :: Prelude.Maybe DataStorage,
    -- | The top-level internet domain name for which your application has
    -- administrative authority.
    domain :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of this app monitor.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time of the most recent changes to this app monitor\'s
    -- configuration.
    lastModified :: Prelude.Maybe Prelude.Text,
    -- | The name of the app monitor.
    name :: Prelude.Maybe Prelude.Text,
    -- | The current state of the app monitor.
    state :: Prelude.Maybe StateEnum,
    -- | The list of tag keys and values associated with this app monitor.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppMonitor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appMonitorConfiguration', 'appMonitor_appMonitorConfiguration' - A structure that contains much of the configuration data for the app
-- monitor.
--
-- 'created', 'appMonitor_created' - The date and time that this app monitor was created.
--
-- 'customEvents', 'appMonitor_customEvents' - Specifies whether this app monitor allows the web client to define and
-- send custom events.
--
-- For more information about custom events, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-RUM-custom-events.html Send custom events>.
--
-- 'dataStorage', 'appMonitor_dataStorage' - A structure that contains information about whether this app monitor
-- stores a copy of the telemetry data that RUM collects using CloudWatch
-- Logs.
--
-- 'domain', 'appMonitor_domain' - The top-level internet domain name for which your application has
-- administrative authority.
--
-- 'id', 'appMonitor_id' - The unique ID of this app monitor.
--
-- 'lastModified', 'appMonitor_lastModified' - The date and time of the most recent changes to this app monitor\'s
-- configuration.
--
-- 'name', 'appMonitor_name' - The name of the app monitor.
--
-- 'state', 'appMonitor_state' - The current state of the app monitor.
--
-- 'tags', 'appMonitor_tags' - The list of tag keys and values associated with this app monitor.
newAppMonitor ::
  AppMonitor
newAppMonitor =
  AppMonitor'
    { appMonitorConfiguration =
        Prelude.Nothing,
      created = Prelude.Nothing,
      customEvents = Prelude.Nothing,
      dataStorage = Prelude.Nothing,
      domain = Prelude.Nothing,
      id = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      name = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | A structure that contains much of the configuration data for the app
-- monitor.
appMonitor_appMonitorConfiguration :: Lens.Lens' AppMonitor (Prelude.Maybe AppMonitorConfiguration)
appMonitor_appMonitorConfiguration = Lens.lens (\AppMonitor' {appMonitorConfiguration} -> appMonitorConfiguration) (\s@AppMonitor' {} a -> s {appMonitorConfiguration = a} :: AppMonitor)

-- | The date and time that this app monitor was created.
appMonitor_created :: Lens.Lens' AppMonitor (Prelude.Maybe Prelude.Text)
appMonitor_created = Lens.lens (\AppMonitor' {created} -> created) (\s@AppMonitor' {} a -> s {created = a} :: AppMonitor)

-- | Specifies whether this app monitor allows the web client to define and
-- send custom events.
--
-- For more information about custom events, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-RUM-custom-events.html Send custom events>.
appMonitor_customEvents :: Lens.Lens' AppMonitor (Prelude.Maybe CustomEvents)
appMonitor_customEvents = Lens.lens (\AppMonitor' {customEvents} -> customEvents) (\s@AppMonitor' {} a -> s {customEvents = a} :: AppMonitor)

-- | A structure that contains information about whether this app monitor
-- stores a copy of the telemetry data that RUM collects using CloudWatch
-- Logs.
appMonitor_dataStorage :: Lens.Lens' AppMonitor (Prelude.Maybe DataStorage)
appMonitor_dataStorage = Lens.lens (\AppMonitor' {dataStorage} -> dataStorage) (\s@AppMonitor' {} a -> s {dataStorage = a} :: AppMonitor)

-- | The top-level internet domain name for which your application has
-- administrative authority.
appMonitor_domain :: Lens.Lens' AppMonitor (Prelude.Maybe Prelude.Text)
appMonitor_domain = Lens.lens (\AppMonitor' {domain} -> domain) (\s@AppMonitor' {} a -> s {domain = a} :: AppMonitor)

-- | The unique ID of this app monitor.
appMonitor_id :: Lens.Lens' AppMonitor (Prelude.Maybe Prelude.Text)
appMonitor_id = Lens.lens (\AppMonitor' {id} -> id) (\s@AppMonitor' {} a -> s {id = a} :: AppMonitor)

-- | The date and time of the most recent changes to this app monitor\'s
-- configuration.
appMonitor_lastModified :: Lens.Lens' AppMonitor (Prelude.Maybe Prelude.Text)
appMonitor_lastModified = Lens.lens (\AppMonitor' {lastModified} -> lastModified) (\s@AppMonitor' {} a -> s {lastModified = a} :: AppMonitor)

-- | The name of the app monitor.
appMonitor_name :: Lens.Lens' AppMonitor (Prelude.Maybe Prelude.Text)
appMonitor_name = Lens.lens (\AppMonitor' {name} -> name) (\s@AppMonitor' {} a -> s {name = a} :: AppMonitor)

-- | The current state of the app monitor.
appMonitor_state :: Lens.Lens' AppMonitor (Prelude.Maybe StateEnum)
appMonitor_state = Lens.lens (\AppMonitor' {state} -> state) (\s@AppMonitor' {} a -> s {state = a} :: AppMonitor)

-- | The list of tag keys and values associated with this app monitor.
appMonitor_tags :: Lens.Lens' AppMonitor (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
appMonitor_tags = Lens.lens (\AppMonitor' {tags} -> tags) (\s@AppMonitor' {} a -> s {tags = a} :: AppMonitor) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AppMonitor where
  parseJSON =
    Data.withObject
      "AppMonitor"
      ( \x ->
          AppMonitor'
            Prelude.<$> (x Data..:? "AppMonitorConfiguration")
            Prelude.<*> (x Data..:? "Created")
            Prelude.<*> (x Data..:? "CustomEvents")
            Prelude.<*> (x Data..:? "DataStorage")
            Prelude.<*> (x Data..:? "Domain")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "LastModified")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AppMonitor where
  hashWithSalt _salt AppMonitor' {..} =
    _salt
      `Prelude.hashWithSalt` appMonitorConfiguration
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` customEvents
      `Prelude.hashWithSalt` dataStorage
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastModified
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags

instance Prelude.NFData AppMonitor where
  rnf AppMonitor' {..} =
    Prelude.rnf appMonitorConfiguration
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf customEvents
      `Prelude.seq` Prelude.rnf dataStorage
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastModified
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
