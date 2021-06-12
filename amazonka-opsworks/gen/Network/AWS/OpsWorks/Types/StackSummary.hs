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
-- Module      : Network.AWS.OpsWorks.Types.StackSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.StackSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.InstancesCount

-- | Summarizes the number of layers, instances, and apps in a stack.
--
-- /See:/ 'newStackSummary' smart constructor.
data StackSummary = StackSummary'
  { -- | The stack ID.
    stackId :: Core.Maybe Core.Text,
    -- | The number of layers.
    layersCount :: Core.Maybe Core.Int,
    -- | The stack\'s ARN.
    arn :: Core.Maybe Core.Text,
    -- | The stack name.
    name :: Core.Maybe Core.Text,
    -- | An @InstancesCount@ object with the number of instances in each status.
    instancesCount :: Core.Maybe InstancesCount,
    -- | The number of apps.
    appsCount :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StackSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackId', 'stackSummary_stackId' - The stack ID.
--
-- 'layersCount', 'stackSummary_layersCount' - The number of layers.
--
-- 'arn', 'stackSummary_arn' - The stack\'s ARN.
--
-- 'name', 'stackSummary_name' - The stack name.
--
-- 'instancesCount', 'stackSummary_instancesCount' - An @InstancesCount@ object with the number of instances in each status.
--
-- 'appsCount', 'stackSummary_appsCount' - The number of apps.
newStackSummary ::
  StackSummary
newStackSummary =
  StackSummary'
    { stackId = Core.Nothing,
      layersCount = Core.Nothing,
      arn = Core.Nothing,
      name = Core.Nothing,
      instancesCount = Core.Nothing,
      appsCount = Core.Nothing
    }

-- | The stack ID.
stackSummary_stackId :: Lens.Lens' StackSummary (Core.Maybe Core.Text)
stackSummary_stackId = Lens.lens (\StackSummary' {stackId} -> stackId) (\s@StackSummary' {} a -> s {stackId = a} :: StackSummary)

-- | The number of layers.
stackSummary_layersCount :: Lens.Lens' StackSummary (Core.Maybe Core.Int)
stackSummary_layersCount = Lens.lens (\StackSummary' {layersCount} -> layersCount) (\s@StackSummary' {} a -> s {layersCount = a} :: StackSummary)

-- | The stack\'s ARN.
stackSummary_arn :: Lens.Lens' StackSummary (Core.Maybe Core.Text)
stackSummary_arn = Lens.lens (\StackSummary' {arn} -> arn) (\s@StackSummary' {} a -> s {arn = a} :: StackSummary)

-- | The stack name.
stackSummary_name :: Lens.Lens' StackSummary (Core.Maybe Core.Text)
stackSummary_name = Lens.lens (\StackSummary' {name} -> name) (\s@StackSummary' {} a -> s {name = a} :: StackSummary)

-- | An @InstancesCount@ object with the number of instances in each status.
stackSummary_instancesCount :: Lens.Lens' StackSummary (Core.Maybe InstancesCount)
stackSummary_instancesCount = Lens.lens (\StackSummary' {instancesCount} -> instancesCount) (\s@StackSummary' {} a -> s {instancesCount = a} :: StackSummary)

-- | The number of apps.
stackSummary_appsCount :: Lens.Lens' StackSummary (Core.Maybe Core.Int)
stackSummary_appsCount = Lens.lens (\StackSummary' {appsCount} -> appsCount) (\s@StackSummary' {} a -> s {appsCount = a} :: StackSummary)

instance Core.FromJSON StackSummary where
  parseJSON =
    Core.withObject
      "StackSummary"
      ( \x ->
          StackSummary'
            Core.<$> (x Core..:? "StackId")
            Core.<*> (x Core..:? "LayersCount")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "InstancesCount")
            Core.<*> (x Core..:? "AppsCount")
      )

instance Core.Hashable StackSummary

instance Core.NFData StackSummary
