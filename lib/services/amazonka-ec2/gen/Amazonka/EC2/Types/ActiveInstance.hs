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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ActiveInstance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceHealthStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes a running instance in a Spot Fleet.
--
-- /See:/ 'newActiveInstance' smart constructor.
data ActiveInstance = ActiveInstance'
  { -- | The health status of the instance. If the status of either the instance
    -- status check or the system status check is @impaired@, the health status
    -- of the instance is @unhealthy@. Otherwise, the health status is
    -- @healthy@.
    instanceHealth :: Prelude.Maybe InstanceHealthStatus,
    -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The instance type.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Spot Instance request.
    spotInstanceRequestId :: Prelude.Maybe Prelude.Text
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
-- 'instanceHealth', 'activeInstance_instanceHealth' - The health status of the instance. If the status of either the instance
-- status check or the system status check is @impaired@, the health status
-- of the instance is @unhealthy@. Otherwise, the health status is
-- @healthy@.
--
-- 'instanceId', 'activeInstance_instanceId' - The ID of the instance.
--
-- 'instanceType', 'activeInstance_instanceType' - The instance type.
--
-- 'spotInstanceRequestId', 'activeInstance_spotInstanceRequestId' - The ID of the Spot Instance request.
newActiveInstance ::
  ActiveInstance
newActiveInstance =
  ActiveInstance'
    { instanceHealth = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      spotInstanceRequestId = Prelude.Nothing
    }

-- | The health status of the instance. If the status of either the instance
-- status check or the system status check is @impaired@, the health status
-- of the instance is @unhealthy@. Otherwise, the health status is
-- @healthy@.
activeInstance_instanceHealth :: Lens.Lens' ActiveInstance (Prelude.Maybe InstanceHealthStatus)
activeInstance_instanceHealth = Lens.lens (\ActiveInstance' {instanceHealth} -> instanceHealth) (\s@ActiveInstance' {} a -> s {instanceHealth = a} :: ActiveInstance)

-- | The ID of the instance.
activeInstance_instanceId :: Lens.Lens' ActiveInstance (Prelude.Maybe Prelude.Text)
activeInstance_instanceId = Lens.lens (\ActiveInstance' {instanceId} -> instanceId) (\s@ActiveInstance' {} a -> s {instanceId = a} :: ActiveInstance)

-- | The instance type.
activeInstance_instanceType :: Lens.Lens' ActiveInstance (Prelude.Maybe Prelude.Text)
activeInstance_instanceType = Lens.lens (\ActiveInstance' {instanceType} -> instanceType) (\s@ActiveInstance' {} a -> s {instanceType = a} :: ActiveInstance)

-- | The ID of the Spot Instance request.
activeInstance_spotInstanceRequestId :: Lens.Lens' ActiveInstance (Prelude.Maybe Prelude.Text)
activeInstance_spotInstanceRequestId = Lens.lens (\ActiveInstance' {spotInstanceRequestId} -> spotInstanceRequestId) (\s@ActiveInstance' {} a -> s {spotInstanceRequestId = a} :: ActiveInstance)

instance Data.FromXML ActiveInstance where
  parseXML x =
    ActiveInstance'
      Prelude.<$> (x Data..@? "instanceHealth")
      Prelude.<*> (x Data..@? "instanceId")
      Prelude.<*> (x Data..@? "instanceType")
      Prelude.<*> (x Data..@? "spotInstanceRequestId")

instance Prelude.Hashable ActiveInstance where
  hashWithSalt _salt ActiveInstance' {..} =
    _salt
      `Prelude.hashWithSalt` instanceHealth
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` spotInstanceRequestId

instance Prelude.NFData ActiveInstance where
  rnf ActiveInstance' {..} =
    Prelude.rnf instanceHealth `Prelude.seq`
      Prelude.rnf instanceId `Prelude.seq`
        Prelude.rnf instanceType `Prelude.seq`
          Prelude.rnf spotInstanceRequestId
