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
-- Module      : Amazonka.DevOpsGuru.Types.MonitoredResourceIdentifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.MonitoredResourceIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.ResourceCollection
import Amazonka.DevOpsGuru.Types.ResourcePermission
import qualified Amazonka.Prelude as Prelude

-- | Information about the resource that is being monitored, including the
-- name of the resource, the type of resource, and whether or not
-- permission is given to DevOps Guru to access that resource.
--
-- /See:/ 'newMonitoredResourceIdentifier' smart constructor.
data MonitoredResourceIdentifier = MonitoredResourceIdentifier'
  { -- | The time at which DevOps Guru last updated this resource.
    lastUpdated :: Prelude.Maybe Data.POSIX,
    -- | The name of the resource being monitored.
    monitoredResourceName :: Prelude.Maybe Prelude.Text,
    resourceCollection :: Prelude.Maybe ResourceCollection,
    -- | The permission status of a resource.
    resourcePermission :: Prelude.Maybe ResourcePermission,
    -- | The type of resource being monitored.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitoredResourceIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdated', 'monitoredResourceIdentifier_lastUpdated' - The time at which DevOps Guru last updated this resource.
--
-- 'monitoredResourceName', 'monitoredResourceIdentifier_monitoredResourceName' - The name of the resource being monitored.
--
-- 'resourceCollection', 'monitoredResourceIdentifier_resourceCollection' - Undocumented member.
--
-- 'resourcePermission', 'monitoredResourceIdentifier_resourcePermission' - The permission status of a resource.
--
-- 'type'', 'monitoredResourceIdentifier_type' - The type of resource being monitored.
newMonitoredResourceIdentifier ::
  MonitoredResourceIdentifier
newMonitoredResourceIdentifier =
  MonitoredResourceIdentifier'
    { lastUpdated =
        Prelude.Nothing,
      monitoredResourceName = Prelude.Nothing,
      resourceCollection = Prelude.Nothing,
      resourcePermission = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The time at which DevOps Guru last updated this resource.
monitoredResourceIdentifier_lastUpdated :: Lens.Lens' MonitoredResourceIdentifier (Prelude.Maybe Prelude.UTCTime)
monitoredResourceIdentifier_lastUpdated = Lens.lens (\MonitoredResourceIdentifier' {lastUpdated} -> lastUpdated) (\s@MonitoredResourceIdentifier' {} a -> s {lastUpdated = a} :: MonitoredResourceIdentifier) Prelude.. Lens.mapping Data._Time

-- | The name of the resource being monitored.
monitoredResourceIdentifier_monitoredResourceName :: Lens.Lens' MonitoredResourceIdentifier (Prelude.Maybe Prelude.Text)
monitoredResourceIdentifier_monitoredResourceName = Lens.lens (\MonitoredResourceIdentifier' {monitoredResourceName} -> monitoredResourceName) (\s@MonitoredResourceIdentifier' {} a -> s {monitoredResourceName = a} :: MonitoredResourceIdentifier)

-- | Undocumented member.
monitoredResourceIdentifier_resourceCollection :: Lens.Lens' MonitoredResourceIdentifier (Prelude.Maybe ResourceCollection)
monitoredResourceIdentifier_resourceCollection = Lens.lens (\MonitoredResourceIdentifier' {resourceCollection} -> resourceCollection) (\s@MonitoredResourceIdentifier' {} a -> s {resourceCollection = a} :: MonitoredResourceIdentifier)

-- | The permission status of a resource.
monitoredResourceIdentifier_resourcePermission :: Lens.Lens' MonitoredResourceIdentifier (Prelude.Maybe ResourcePermission)
monitoredResourceIdentifier_resourcePermission = Lens.lens (\MonitoredResourceIdentifier' {resourcePermission} -> resourcePermission) (\s@MonitoredResourceIdentifier' {} a -> s {resourcePermission = a} :: MonitoredResourceIdentifier)

-- | The type of resource being monitored.
monitoredResourceIdentifier_type :: Lens.Lens' MonitoredResourceIdentifier (Prelude.Maybe Prelude.Text)
monitoredResourceIdentifier_type = Lens.lens (\MonitoredResourceIdentifier' {type'} -> type') (\s@MonitoredResourceIdentifier' {} a -> s {type' = a} :: MonitoredResourceIdentifier)

instance Data.FromJSON MonitoredResourceIdentifier where
  parseJSON =
    Data.withObject
      "MonitoredResourceIdentifier"
      ( \x ->
          MonitoredResourceIdentifier'
            Prelude.<$> (x Data..:? "LastUpdated")
            Prelude.<*> (x Data..:? "MonitoredResourceName")
            Prelude.<*> (x Data..:? "ResourceCollection")
            Prelude.<*> (x Data..:? "ResourcePermission")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable MonitoredResourceIdentifier where
  hashWithSalt _salt MonitoredResourceIdentifier' {..} =
    _salt `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` monitoredResourceName
      `Prelude.hashWithSalt` resourceCollection
      `Prelude.hashWithSalt` resourcePermission
      `Prelude.hashWithSalt` type'

instance Prelude.NFData MonitoredResourceIdentifier where
  rnf MonitoredResourceIdentifier' {..} =
    Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf monitoredResourceName
      `Prelude.seq` Prelude.rnf resourceCollection
      `Prelude.seq` Prelude.rnf resourcePermission
      `Prelude.seq` Prelude.rnf type'
