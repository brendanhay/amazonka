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
-- Module      : Amazonka.AutoScaling.Types.WarmPoolConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.WarmPoolConfiguration where

import Amazonka.AutoScaling.Types.InstanceReusePolicy
import Amazonka.AutoScaling.Types.WarmPoolState
import Amazonka.AutoScaling.Types.WarmPoolStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a warm pool configuration.
--
-- /See:/ 'newWarmPoolConfiguration' smart constructor.
data WarmPoolConfiguration = WarmPoolConfiguration'
  { -- | The instance state to transition to after the lifecycle actions are
    -- complete.
    poolState :: Prelude.Maybe WarmPoolState,
    -- | The status of a warm pool that is marked for deletion.
    status :: Prelude.Maybe WarmPoolStatus,
    -- | The minimum number of instances to maintain in the warm pool.
    minSize :: Prelude.Maybe Prelude.Natural,
    -- | The instance reuse policy.
    instanceReusePolicy :: Prelude.Maybe InstanceReusePolicy,
    -- | The maximum number of instances that are allowed to be in the warm pool
    -- or in any state except @Terminated@ for the Auto Scaling group.
    maxGroupPreparedCapacity :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WarmPoolConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'poolState', 'warmPoolConfiguration_poolState' - The instance state to transition to after the lifecycle actions are
-- complete.
--
-- 'status', 'warmPoolConfiguration_status' - The status of a warm pool that is marked for deletion.
--
-- 'minSize', 'warmPoolConfiguration_minSize' - The minimum number of instances to maintain in the warm pool.
--
-- 'instanceReusePolicy', 'warmPoolConfiguration_instanceReusePolicy' - The instance reuse policy.
--
-- 'maxGroupPreparedCapacity', 'warmPoolConfiguration_maxGroupPreparedCapacity' - The maximum number of instances that are allowed to be in the warm pool
-- or in any state except @Terminated@ for the Auto Scaling group.
newWarmPoolConfiguration ::
  WarmPoolConfiguration
newWarmPoolConfiguration =
  WarmPoolConfiguration'
    { poolState = Prelude.Nothing,
      status = Prelude.Nothing,
      minSize = Prelude.Nothing,
      instanceReusePolicy = Prelude.Nothing,
      maxGroupPreparedCapacity = Prelude.Nothing
    }

-- | The instance state to transition to after the lifecycle actions are
-- complete.
warmPoolConfiguration_poolState :: Lens.Lens' WarmPoolConfiguration (Prelude.Maybe WarmPoolState)
warmPoolConfiguration_poolState = Lens.lens (\WarmPoolConfiguration' {poolState} -> poolState) (\s@WarmPoolConfiguration' {} a -> s {poolState = a} :: WarmPoolConfiguration)

-- | The status of a warm pool that is marked for deletion.
warmPoolConfiguration_status :: Lens.Lens' WarmPoolConfiguration (Prelude.Maybe WarmPoolStatus)
warmPoolConfiguration_status = Lens.lens (\WarmPoolConfiguration' {status} -> status) (\s@WarmPoolConfiguration' {} a -> s {status = a} :: WarmPoolConfiguration)

-- | The minimum number of instances to maintain in the warm pool.
warmPoolConfiguration_minSize :: Lens.Lens' WarmPoolConfiguration (Prelude.Maybe Prelude.Natural)
warmPoolConfiguration_minSize = Lens.lens (\WarmPoolConfiguration' {minSize} -> minSize) (\s@WarmPoolConfiguration' {} a -> s {minSize = a} :: WarmPoolConfiguration)

-- | The instance reuse policy.
warmPoolConfiguration_instanceReusePolicy :: Lens.Lens' WarmPoolConfiguration (Prelude.Maybe InstanceReusePolicy)
warmPoolConfiguration_instanceReusePolicy = Lens.lens (\WarmPoolConfiguration' {instanceReusePolicy} -> instanceReusePolicy) (\s@WarmPoolConfiguration' {} a -> s {instanceReusePolicy = a} :: WarmPoolConfiguration)

-- | The maximum number of instances that are allowed to be in the warm pool
-- or in any state except @Terminated@ for the Auto Scaling group.
warmPoolConfiguration_maxGroupPreparedCapacity :: Lens.Lens' WarmPoolConfiguration (Prelude.Maybe Prelude.Int)
warmPoolConfiguration_maxGroupPreparedCapacity = Lens.lens (\WarmPoolConfiguration' {maxGroupPreparedCapacity} -> maxGroupPreparedCapacity) (\s@WarmPoolConfiguration' {} a -> s {maxGroupPreparedCapacity = a} :: WarmPoolConfiguration)

instance Data.FromXML WarmPoolConfiguration where
  parseXML x =
    WarmPoolConfiguration'
      Prelude.<$> (x Data..@? "PoolState")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "MinSize")
      Prelude.<*> (x Data..@? "InstanceReusePolicy")
      Prelude.<*> (x Data..@? "MaxGroupPreparedCapacity")

instance Prelude.Hashable WarmPoolConfiguration where
  hashWithSalt _salt WarmPoolConfiguration' {..} =
    _salt `Prelude.hashWithSalt` poolState
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` minSize
      `Prelude.hashWithSalt` instanceReusePolicy
      `Prelude.hashWithSalt` maxGroupPreparedCapacity

instance Prelude.NFData WarmPoolConfiguration where
  rnf WarmPoolConfiguration' {..} =
    Prelude.rnf poolState
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf minSize
      `Prelude.seq` Prelude.rnf instanceReusePolicy
      `Prelude.seq` Prelude.rnf maxGroupPreparedCapacity
