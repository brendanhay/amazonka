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
-- Module      : Amazonka.EC2.Types.ActiveInstance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ActiveInstance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceHealthStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes a running instance in a Spot Fleet.
--
-- /See:/ 'newActiveInstance' smart constructor.
data ActiveInstance = ActiveInstance'
  { -- | The ID of the Spot Instance request.
    spotInstanceRequestId :: Prelude.Maybe Prelude.Text,
    -- | The health status of the instance. If the status of either the instance
    -- status check or the system status check is @impaired@, the health status
    -- of the instance is @unhealthy@. Otherwise, the health status is
    -- @healthy@.
    instanceHealth :: Prelude.Maybe InstanceHealthStatus,
    -- | The instance type.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActiveInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'spotInstanceRequestId', 'activeInstance_spotInstanceRequestId' - The ID of the Spot Instance request.
--
-- 'instanceHealth', 'activeInstance_instanceHealth' - The health status of the instance. If the status of either the instance
-- status check or the system status check is @impaired@, the health status
-- of the instance is @unhealthy@. Otherwise, the health status is
-- @healthy@.
--
-- 'instanceType', 'activeInstance_instanceType' - The instance type.
--
-- 'instanceId', 'activeInstance_instanceId' - The ID of the instance.
newActiveInstance ::
  ActiveInstance
newActiveInstance =
  ActiveInstance'
    { spotInstanceRequestId =
        Prelude.Nothing,
      instanceHealth = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      instanceId = Prelude.Nothing
    }

-- | The ID of the Spot Instance request.
activeInstance_spotInstanceRequestId :: Lens.Lens' ActiveInstance (Prelude.Maybe Prelude.Text)
activeInstance_spotInstanceRequestId = Lens.lens (\ActiveInstance' {spotInstanceRequestId} -> spotInstanceRequestId) (\s@ActiveInstance' {} a -> s {spotInstanceRequestId = a} :: ActiveInstance)

-- | The health status of the instance. If the status of either the instance
-- status check or the system status check is @impaired@, the health status
-- of the instance is @unhealthy@. Otherwise, the health status is
-- @healthy@.
activeInstance_instanceHealth :: Lens.Lens' ActiveInstance (Prelude.Maybe InstanceHealthStatus)
activeInstance_instanceHealth = Lens.lens (\ActiveInstance' {instanceHealth} -> instanceHealth) (\s@ActiveInstance' {} a -> s {instanceHealth = a} :: ActiveInstance)

-- | The instance type.
activeInstance_instanceType :: Lens.Lens' ActiveInstance (Prelude.Maybe Prelude.Text)
activeInstance_instanceType = Lens.lens (\ActiveInstance' {instanceType} -> instanceType) (\s@ActiveInstance' {} a -> s {instanceType = a} :: ActiveInstance)

-- | The ID of the instance.
activeInstance_instanceId :: Lens.Lens' ActiveInstance (Prelude.Maybe Prelude.Text)
activeInstance_instanceId = Lens.lens (\ActiveInstance' {instanceId} -> instanceId) (\s@ActiveInstance' {} a -> s {instanceId = a} :: ActiveInstance)

instance Core.FromXML ActiveInstance where
  parseXML x =
    ActiveInstance'
      Prelude.<$> (x Core..@? "spotInstanceRequestId")
      Prelude.<*> (x Core..@? "instanceHealth")
      Prelude.<*> (x Core..@? "instanceType")
      Prelude.<*> (x Core..@? "instanceId")

instance Prelude.Hashable ActiveInstance where
  hashWithSalt _salt ActiveInstance' {..} =
    _salt `Prelude.hashWithSalt` spotInstanceRequestId
      `Prelude.hashWithSalt` instanceHealth
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData ActiveInstance where
  rnf ActiveInstance' {..} =
    Prelude.rnf spotInstanceRequestId
      `Prelude.seq` Prelude.rnf instanceHealth
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf instanceId
