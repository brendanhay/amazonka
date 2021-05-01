{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.DescribeFleetsInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DescribeFleetsInstances where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceLifecycle
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.LaunchTemplateAndOverridesResponse
import Network.AWS.EC2.Types.PlatformValues
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the instances that were launched by the fleet.
--
-- /See:/ 'newDescribeFleetsInstances' smart constructor.
data DescribeFleetsInstances = DescribeFleetsInstances'
  { -- | The IDs of the instances.
    instanceIds :: Prelude.Maybe [Prelude.Text],
    -- | The value is @Windows@ for Windows instances. Otherwise, the value is
    -- blank.
    platform :: Prelude.Maybe PlatformValues,
    -- | The instance type.
    instanceType :: Prelude.Maybe InstanceType,
    -- | The launch templates and overrides that were used for launching the
    -- instances. The values that you specify in the Overrides replace the
    -- values in the launch template.
    launchTemplateAndOverrides :: Prelude.Maybe LaunchTemplateAndOverridesResponse,
    -- | Indicates if the instance that was launched is a Spot Instance or
    -- On-Demand Instance.
    lifecycle :: Prelude.Maybe InstanceLifecycle
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetsInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceIds', 'describeFleetsInstances_instanceIds' - The IDs of the instances.
--
-- 'platform', 'describeFleetsInstances_platform' - The value is @Windows@ for Windows instances. Otherwise, the value is
-- blank.
--
-- 'instanceType', 'describeFleetsInstances_instanceType' - The instance type.
--
-- 'launchTemplateAndOverrides', 'describeFleetsInstances_launchTemplateAndOverrides' - The launch templates and overrides that were used for launching the
-- instances. The values that you specify in the Overrides replace the
-- values in the launch template.
--
-- 'lifecycle', 'describeFleetsInstances_lifecycle' - Indicates if the instance that was launched is a Spot Instance or
-- On-Demand Instance.
newDescribeFleetsInstances ::
  DescribeFleetsInstances
newDescribeFleetsInstances =
  DescribeFleetsInstances'
    { instanceIds =
        Prelude.Nothing,
      platform = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      launchTemplateAndOverrides = Prelude.Nothing,
      lifecycle = Prelude.Nothing
    }

-- | The IDs of the instances.
describeFleetsInstances_instanceIds :: Lens.Lens' DescribeFleetsInstances (Prelude.Maybe [Prelude.Text])
describeFleetsInstances_instanceIds = Lens.lens (\DescribeFleetsInstances' {instanceIds} -> instanceIds) (\s@DescribeFleetsInstances' {} a -> s {instanceIds = a} :: DescribeFleetsInstances) Prelude.. Lens.mapping Prelude._Coerce

-- | The value is @Windows@ for Windows instances. Otherwise, the value is
-- blank.
describeFleetsInstances_platform :: Lens.Lens' DescribeFleetsInstances (Prelude.Maybe PlatformValues)
describeFleetsInstances_platform = Lens.lens (\DescribeFleetsInstances' {platform} -> platform) (\s@DescribeFleetsInstances' {} a -> s {platform = a} :: DescribeFleetsInstances)

-- | The instance type.
describeFleetsInstances_instanceType :: Lens.Lens' DescribeFleetsInstances (Prelude.Maybe InstanceType)
describeFleetsInstances_instanceType = Lens.lens (\DescribeFleetsInstances' {instanceType} -> instanceType) (\s@DescribeFleetsInstances' {} a -> s {instanceType = a} :: DescribeFleetsInstances)

-- | The launch templates and overrides that were used for launching the
-- instances. The values that you specify in the Overrides replace the
-- values in the launch template.
describeFleetsInstances_launchTemplateAndOverrides :: Lens.Lens' DescribeFleetsInstances (Prelude.Maybe LaunchTemplateAndOverridesResponse)
describeFleetsInstances_launchTemplateAndOverrides = Lens.lens (\DescribeFleetsInstances' {launchTemplateAndOverrides} -> launchTemplateAndOverrides) (\s@DescribeFleetsInstances' {} a -> s {launchTemplateAndOverrides = a} :: DescribeFleetsInstances)

-- | Indicates if the instance that was launched is a Spot Instance or
-- On-Demand Instance.
describeFleetsInstances_lifecycle :: Lens.Lens' DescribeFleetsInstances (Prelude.Maybe InstanceLifecycle)
describeFleetsInstances_lifecycle = Lens.lens (\DescribeFleetsInstances' {lifecycle} -> lifecycle) (\s@DescribeFleetsInstances' {} a -> s {lifecycle = a} :: DescribeFleetsInstances)

instance Prelude.FromXML DescribeFleetsInstances where
  parseXML x =
    DescribeFleetsInstances'
      Prelude.<$> ( x Prelude..@? "instanceIds"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "platform")
      Prelude.<*> (x Prelude..@? "instanceType")
      Prelude.<*> (x Prelude..@? "launchTemplateAndOverrides")
      Prelude.<*> (x Prelude..@? "lifecycle")

instance Prelude.Hashable DescribeFleetsInstances

instance Prelude.NFData DescribeFleetsInstances
