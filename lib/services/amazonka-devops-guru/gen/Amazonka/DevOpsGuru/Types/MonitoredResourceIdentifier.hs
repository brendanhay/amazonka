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
import Amazonka.DevOpsGuru.Types.ResourcePermission
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the resource that is being monitored, including the
-- name of the resource, the type of resource, and whether or not
-- permission is given to DevOps Guru to access that resource.
--
-- /See:/ 'newMonitoredResourceIdentifier' smart constructor.
data MonitoredResourceIdentifier = MonitoredResourceIdentifier'
  { -- | The type of resource being monitored.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The permission status of a resource.
    resourcePermission :: Prelude.Maybe ResourcePermission,
    -- | The name of the resource being monitored.
    monitoredResourceName :: Prelude.Maybe Prelude.Text
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
-- 'type'', 'monitoredResourceIdentifier_type' - The type of resource being monitored.
--
-- 'resourcePermission', 'monitoredResourceIdentifier_resourcePermission' - The permission status of a resource.
--
-- 'monitoredResourceName', 'monitoredResourceIdentifier_monitoredResourceName' - The name of the resource being monitored.
newMonitoredResourceIdentifier ::
  MonitoredResourceIdentifier
newMonitoredResourceIdentifier =
  MonitoredResourceIdentifier'
    { type' =
        Prelude.Nothing,
      resourcePermission = Prelude.Nothing,
      monitoredResourceName = Prelude.Nothing
    }

-- | The type of resource being monitored.
monitoredResourceIdentifier_type :: Lens.Lens' MonitoredResourceIdentifier (Prelude.Maybe Prelude.Text)
monitoredResourceIdentifier_type = Lens.lens (\MonitoredResourceIdentifier' {type'} -> type') (\s@MonitoredResourceIdentifier' {} a -> s {type' = a} :: MonitoredResourceIdentifier)

-- | The permission status of a resource.
monitoredResourceIdentifier_resourcePermission :: Lens.Lens' MonitoredResourceIdentifier (Prelude.Maybe ResourcePermission)
monitoredResourceIdentifier_resourcePermission = Lens.lens (\MonitoredResourceIdentifier' {resourcePermission} -> resourcePermission) (\s@MonitoredResourceIdentifier' {} a -> s {resourcePermission = a} :: MonitoredResourceIdentifier)

-- | The name of the resource being monitored.
monitoredResourceIdentifier_monitoredResourceName :: Lens.Lens' MonitoredResourceIdentifier (Prelude.Maybe Prelude.Text)
monitoredResourceIdentifier_monitoredResourceName = Lens.lens (\MonitoredResourceIdentifier' {monitoredResourceName} -> monitoredResourceName) (\s@MonitoredResourceIdentifier' {} a -> s {monitoredResourceName = a} :: MonitoredResourceIdentifier)

instance Core.FromJSON MonitoredResourceIdentifier where
  parseJSON =
    Core.withObject
      "MonitoredResourceIdentifier"
      ( \x ->
          MonitoredResourceIdentifier'
            Prelude.<$> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "ResourcePermission")
            Prelude.<*> (x Core..:? "MonitoredResourceName")
      )

instance Prelude.Hashable MonitoredResourceIdentifier where
  hashWithSalt _salt MonitoredResourceIdentifier' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` resourcePermission
      `Prelude.hashWithSalt` monitoredResourceName

instance Prelude.NFData MonitoredResourceIdentifier where
  rnf MonitoredResourceIdentifier' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf resourcePermission
      `Prelude.seq` Prelude.rnf monitoredResourceName
