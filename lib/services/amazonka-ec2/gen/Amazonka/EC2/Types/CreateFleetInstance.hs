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
-- Module      : Amazonka.EC2.Types.CreateFleetInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CreateFleetInstance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceLifecycle
import Amazonka.EC2.Types.InstanceType
import Amazonka.EC2.Types.LaunchTemplateAndOverridesResponse
import Amazonka.EC2.Types.PlatformValues
import qualified Amazonka.Prelude as Prelude

-- | Describes the instances that were launched by the fleet.
--
-- /See:/ 'newCreateFleetInstance' smart constructor.
data CreateFleetInstance = CreateFleetInstance'
  { -- | The IDs of the instances.
    instanceIds :: Prelude.Maybe [Prelude.Text],
    -- | The instance type.
    instanceType :: Prelude.Maybe InstanceType,
    -- | The launch templates and overrides that were used for launching the
    -- instances. The values that you specify in the Overrides replace the
    -- values in the launch template.
    launchTemplateAndOverrides :: Prelude.Maybe LaunchTemplateAndOverridesResponse,
    -- | Indicates if the instance that was launched is a Spot Instance or
    -- On-Demand Instance.
    lifecycle :: Prelude.Maybe InstanceLifecycle,
    -- | The value is @Windows@ for Windows instances. Otherwise, the value is
    -- blank.
    platform :: Prelude.Maybe PlatformValues
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFleetInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceIds', 'createFleetInstance_instanceIds' - The IDs of the instances.
--
-- 'instanceType', 'createFleetInstance_instanceType' - The instance type.
--
-- 'launchTemplateAndOverrides', 'createFleetInstance_launchTemplateAndOverrides' - The launch templates and overrides that were used for launching the
-- instances. The values that you specify in the Overrides replace the
-- values in the launch template.
--
-- 'lifecycle', 'createFleetInstance_lifecycle' - Indicates if the instance that was launched is a Spot Instance or
-- On-Demand Instance.
--
-- 'platform', 'createFleetInstance_platform' - The value is @Windows@ for Windows instances. Otherwise, the value is
-- blank.
newCreateFleetInstance ::
  CreateFleetInstance
newCreateFleetInstance =
  CreateFleetInstance'
    { instanceIds = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      launchTemplateAndOverrides = Prelude.Nothing,
      lifecycle = Prelude.Nothing,
      platform = Prelude.Nothing
    }

-- | The IDs of the instances.
createFleetInstance_instanceIds :: Lens.Lens' CreateFleetInstance (Prelude.Maybe [Prelude.Text])
createFleetInstance_instanceIds = Lens.lens (\CreateFleetInstance' {instanceIds} -> instanceIds) (\s@CreateFleetInstance' {} a -> s {instanceIds = a} :: CreateFleetInstance) Prelude.. Lens.mapping Lens.coerced

-- | The instance type.
createFleetInstance_instanceType :: Lens.Lens' CreateFleetInstance (Prelude.Maybe InstanceType)
createFleetInstance_instanceType = Lens.lens (\CreateFleetInstance' {instanceType} -> instanceType) (\s@CreateFleetInstance' {} a -> s {instanceType = a} :: CreateFleetInstance)

-- | The launch templates and overrides that were used for launching the
-- instances. The values that you specify in the Overrides replace the
-- values in the launch template.
createFleetInstance_launchTemplateAndOverrides :: Lens.Lens' CreateFleetInstance (Prelude.Maybe LaunchTemplateAndOverridesResponse)
createFleetInstance_launchTemplateAndOverrides = Lens.lens (\CreateFleetInstance' {launchTemplateAndOverrides} -> launchTemplateAndOverrides) (\s@CreateFleetInstance' {} a -> s {launchTemplateAndOverrides = a} :: CreateFleetInstance)

-- | Indicates if the instance that was launched is a Spot Instance or
-- On-Demand Instance.
createFleetInstance_lifecycle :: Lens.Lens' CreateFleetInstance (Prelude.Maybe InstanceLifecycle)
createFleetInstance_lifecycle = Lens.lens (\CreateFleetInstance' {lifecycle} -> lifecycle) (\s@CreateFleetInstance' {} a -> s {lifecycle = a} :: CreateFleetInstance)

-- | The value is @Windows@ for Windows instances. Otherwise, the value is
-- blank.
createFleetInstance_platform :: Lens.Lens' CreateFleetInstance (Prelude.Maybe PlatformValues)
createFleetInstance_platform = Lens.lens (\CreateFleetInstance' {platform} -> platform) (\s@CreateFleetInstance' {} a -> s {platform = a} :: CreateFleetInstance)

instance Data.FromXML CreateFleetInstance where
  parseXML x =
    CreateFleetInstance'
      Prelude.<$> ( x
                      Data..@? "instanceIds"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "instanceType")
      Prelude.<*> (x Data..@? "launchTemplateAndOverrides")
      Prelude.<*> (x Data..@? "lifecycle")
      Prelude.<*> (x Data..@? "platform")

instance Prelude.Hashable CreateFleetInstance where
  hashWithSalt _salt CreateFleetInstance' {..} =
    _salt
      `Prelude.hashWithSalt` instanceIds
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` launchTemplateAndOverrides
      `Prelude.hashWithSalt` lifecycle
      `Prelude.hashWithSalt` platform

instance Prelude.NFData CreateFleetInstance where
  rnf CreateFleetInstance' {..} =
    Prelude.rnf instanceIds
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf launchTemplateAndOverrides
      `Prelude.seq` Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf platform
