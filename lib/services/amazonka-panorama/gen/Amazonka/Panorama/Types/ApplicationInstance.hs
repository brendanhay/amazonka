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
-- Module      : Amazonka.Panorama.Types.ApplicationInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.ApplicationInstance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types.ApplicationInstanceHealthStatus
import Amazonka.Panorama.Types.ApplicationInstanceStatus
import Amazonka.Panorama.Types.ReportedRuntimeContextState
import qualified Amazonka.Prelude as Prelude

-- | An application instance on a device.
--
-- /See:/ 'newApplicationInstance' smart constructor.
data ApplicationInstance = ApplicationInstance'
  { -- | The application instance\'s ID.
    applicationInstanceId :: Prelude.Maybe Prelude.Text,
    -- | The application instance\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | When the application instance was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The device\'s ID.
    defaultRuntimeContextDevice :: Prelude.Maybe Prelude.Text,
    -- | The device\'s name.
    defaultRuntimeContextDeviceName :: Prelude.Maybe Prelude.Text,
    -- | The application instance\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The application instance\'s health status.
    healthStatus :: Prelude.Maybe ApplicationInstanceHealthStatus,
    -- | The application instance\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The application\'s state.
    runtimeContextStates :: Prelude.Maybe [ReportedRuntimeContextState],
    -- | The application instance\'s status.
    status :: Prelude.Maybe ApplicationInstanceStatus,
    -- | The application instance\'s status description.
    statusDescription :: Prelude.Maybe Prelude.Text,
    -- | The application instance\'s tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationInstanceId', 'applicationInstance_applicationInstanceId' - The application instance\'s ID.
--
-- 'arn', 'applicationInstance_arn' - The application instance\'s ARN.
--
-- 'createdTime', 'applicationInstance_createdTime' - When the application instance was created.
--
-- 'defaultRuntimeContextDevice', 'applicationInstance_defaultRuntimeContextDevice' - The device\'s ID.
--
-- 'defaultRuntimeContextDeviceName', 'applicationInstance_defaultRuntimeContextDeviceName' - The device\'s name.
--
-- 'description', 'applicationInstance_description' - The application instance\'s description.
--
-- 'healthStatus', 'applicationInstance_healthStatus' - The application instance\'s health status.
--
-- 'name', 'applicationInstance_name' - The application instance\'s name.
--
-- 'runtimeContextStates', 'applicationInstance_runtimeContextStates' - The application\'s state.
--
-- 'status', 'applicationInstance_status' - The application instance\'s status.
--
-- 'statusDescription', 'applicationInstance_statusDescription' - The application instance\'s status description.
--
-- 'tags', 'applicationInstance_tags' - The application instance\'s tags.
newApplicationInstance ::
  ApplicationInstance
newApplicationInstance =
  ApplicationInstance'
    { applicationInstanceId =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      defaultRuntimeContextDevice = Prelude.Nothing,
      defaultRuntimeContextDeviceName = Prelude.Nothing,
      description = Prelude.Nothing,
      healthStatus = Prelude.Nothing,
      name = Prelude.Nothing,
      runtimeContextStates = Prelude.Nothing,
      status = Prelude.Nothing,
      statusDescription = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The application instance\'s ID.
applicationInstance_applicationInstanceId :: Lens.Lens' ApplicationInstance (Prelude.Maybe Prelude.Text)
applicationInstance_applicationInstanceId = Lens.lens (\ApplicationInstance' {applicationInstanceId} -> applicationInstanceId) (\s@ApplicationInstance' {} a -> s {applicationInstanceId = a} :: ApplicationInstance)

-- | The application instance\'s ARN.
applicationInstance_arn :: Lens.Lens' ApplicationInstance (Prelude.Maybe Prelude.Text)
applicationInstance_arn = Lens.lens (\ApplicationInstance' {arn} -> arn) (\s@ApplicationInstance' {} a -> s {arn = a} :: ApplicationInstance)

-- | When the application instance was created.
applicationInstance_createdTime :: Lens.Lens' ApplicationInstance (Prelude.Maybe Prelude.UTCTime)
applicationInstance_createdTime = Lens.lens (\ApplicationInstance' {createdTime} -> createdTime) (\s@ApplicationInstance' {} a -> s {createdTime = a} :: ApplicationInstance) Prelude.. Lens.mapping Data._Time

-- | The device\'s ID.
applicationInstance_defaultRuntimeContextDevice :: Lens.Lens' ApplicationInstance (Prelude.Maybe Prelude.Text)
applicationInstance_defaultRuntimeContextDevice = Lens.lens (\ApplicationInstance' {defaultRuntimeContextDevice} -> defaultRuntimeContextDevice) (\s@ApplicationInstance' {} a -> s {defaultRuntimeContextDevice = a} :: ApplicationInstance)

-- | The device\'s name.
applicationInstance_defaultRuntimeContextDeviceName :: Lens.Lens' ApplicationInstance (Prelude.Maybe Prelude.Text)
applicationInstance_defaultRuntimeContextDeviceName = Lens.lens (\ApplicationInstance' {defaultRuntimeContextDeviceName} -> defaultRuntimeContextDeviceName) (\s@ApplicationInstance' {} a -> s {defaultRuntimeContextDeviceName = a} :: ApplicationInstance)

-- | The application instance\'s description.
applicationInstance_description :: Lens.Lens' ApplicationInstance (Prelude.Maybe Prelude.Text)
applicationInstance_description = Lens.lens (\ApplicationInstance' {description} -> description) (\s@ApplicationInstance' {} a -> s {description = a} :: ApplicationInstance)

-- | The application instance\'s health status.
applicationInstance_healthStatus :: Lens.Lens' ApplicationInstance (Prelude.Maybe ApplicationInstanceHealthStatus)
applicationInstance_healthStatus = Lens.lens (\ApplicationInstance' {healthStatus} -> healthStatus) (\s@ApplicationInstance' {} a -> s {healthStatus = a} :: ApplicationInstance)

-- | The application instance\'s name.
applicationInstance_name :: Lens.Lens' ApplicationInstance (Prelude.Maybe Prelude.Text)
applicationInstance_name = Lens.lens (\ApplicationInstance' {name} -> name) (\s@ApplicationInstance' {} a -> s {name = a} :: ApplicationInstance)

-- | The application\'s state.
applicationInstance_runtimeContextStates :: Lens.Lens' ApplicationInstance (Prelude.Maybe [ReportedRuntimeContextState])
applicationInstance_runtimeContextStates = Lens.lens (\ApplicationInstance' {runtimeContextStates} -> runtimeContextStates) (\s@ApplicationInstance' {} a -> s {runtimeContextStates = a} :: ApplicationInstance) Prelude.. Lens.mapping Lens.coerced

-- | The application instance\'s status.
applicationInstance_status :: Lens.Lens' ApplicationInstance (Prelude.Maybe ApplicationInstanceStatus)
applicationInstance_status = Lens.lens (\ApplicationInstance' {status} -> status) (\s@ApplicationInstance' {} a -> s {status = a} :: ApplicationInstance)

-- | The application instance\'s status description.
applicationInstance_statusDescription :: Lens.Lens' ApplicationInstance (Prelude.Maybe Prelude.Text)
applicationInstance_statusDescription = Lens.lens (\ApplicationInstance' {statusDescription} -> statusDescription) (\s@ApplicationInstance' {} a -> s {statusDescription = a} :: ApplicationInstance)

-- | The application instance\'s tags.
applicationInstance_tags :: Lens.Lens' ApplicationInstance (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
applicationInstance_tags = Lens.lens (\ApplicationInstance' {tags} -> tags) (\s@ApplicationInstance' {} a -> s {tags = a} :: ApplicationInstance) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ApplicationInstance where
  parseJSON =
    Data.withObject
      "ApplicationInstance"
      ( \x ->
          ApplicationInstance'
            Prelude.<$> (x Data..:? "ApplicationInstanceId")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "DefaultRuntimeContextDevice")
            Prelude.<*> (x Data..:? "DefaultRuntimeContextDeviceName")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "HealthStatus")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> ( x Data..:? "RuntimeContextStates"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusDescription")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ApplicationInstance where
  hashWithSalt _salt ApplicationInstance' {..} =
    _salt `Prelude.hashWithSalt` applicationInstanceId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` defaultRuntimeContextDevice
      `Prelude.hashWithSalt` defaultRuntimeContextDeviceName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` healthStatus
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` runtimeContextStates
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusDescription
      `Prelude.hashWithSalt` tags

instance Prelude.NFData ApplicationInstance where
  rnf ApplicationInstance' {..} =
    Prelude.rnf applicationInstanceId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf defaultRuntimeContextDevice
      `Prelude.seq` Prelude.rnf defaultRuntimeContextDeviceName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf healthStatus
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf runtimeContextStates
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusDescription
      `Prelude.seq` Prelude.rnf tags
