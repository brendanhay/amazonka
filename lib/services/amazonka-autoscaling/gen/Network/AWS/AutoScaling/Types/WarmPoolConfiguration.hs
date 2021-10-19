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
-- Module      : Network.AWS.AutoScaling.Types.WarmPoolConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.WarmPoolConfiguration where

import Network.AWS.AutoScaling.Types.WarmPoolState
import Network.AWS.AutoScaling.Types.WarmPoolStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a warm pool configuration.
--
-- /See:/ 'newWarmPoolConfiguration' smart constructor.
data WarmPoolConfiguration = WarmPoolConfiguration'
  { -- | The status of a warm pool that is marked for deletion.
    status :: Prelude.Maybe WarmPoolStatus,
    -- | The minimum number of instances to maintain in the warm pool.
    minSize :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of instances that are allowed to be in the warm pool
    -- or in any state except @Terminated@ for the Auto Scaling group.
    maxGroupPreparedCapacity :: Prelude.Maybe Prelude.Int,
    -- | The instance state to transition to after the lifecycle actions are
    -- complete.
    poolState :: Prelude.Maybe WarmPoolState
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
-- 'status', 'warmPoolConfiguration_status' - The status of a warm pool that is marked for deletion.
--
-- 'minSize', 'warmPoolConfiguration_minSize' - The minimum number of instances to maintain in the warm pool.
--
-- 'maxGroupPreparedCapacity', 'warmPoolConfiguration_maxGroupPreparedCapacity' - The maximum number of instances that are allowed to be in the warm pool
-- or in any state except @Terminated@ for the Auto Scaling group.
--
-- 'poolState', 'warmPoolConfiguration_poolState' - The instance state to transition to after the lifecycle actions are
-- complete.
newWarmPoolConfiguration ::
  WarmPoolConfiguration
newWarmPoolConfiguration =
  WarmPoolConfiguration'
    { status = Prelude.Nothing,
      minSize = Prelude.Nothing,
      maxGroupPreparedCapacity = Prelude.Nothing,
      poolState = Prelude.Nothing
    }

-- | The status of a warm pool that is marked for deletion.
warmPoolConfiguration_status :: Lens.Lens' WarmPoolConfiguration (Prelude.Maybe WarmPoolStatus)
warmPoolConfiguration_status = Lens.lens (\WarmPoolConfiguration' {status} -> status) (\s@WarmPoolConfiguration' {} a -> s {status = a} :: WarmPoolConfiguration)

-- | The minimum number of instances to maintain in the warm pool.
warmPoolConfiguration_minSize :: Lens.Lens' WarmPoolConfiguration (Prelude.Maybe Prelude.Natural)
warmPoolConfiguration_minSize = Lens.lens (\WarmPoolConfiguration' {minSize} -> minSize) (\s@WarmPoolConfiguration' {} a -> s {minSize = a} :: WarmPoolConfiguration)

-- | The maximum number of instances that are allowed to be in the warm pool
-- or in any state except @Terminated@ for the Auto Scaling group.
warmPoolConfiguration_maxGroupPreparedCapacity :: Lens.Lens' WarmPoolConfiguration (Prelude.Maybe Prelude.Int)
warmPoolConfiguration_maxGroupPreparedCapacity = Lens.lens (\WarmPoolConfiguration' {maxGroupPreparedCapacity} -> maxGroupPreparedCapacity) (\s@WarmPoolConfiguration' {} a -> s {maxGroupPreparedCapacity = a} :: WarmPoolConfiguration)

-- | The instance state to transition to after the lifecycle actions are
-- complete.
warmPoolConfiguration_poolState :: Lens.Lens' WarmPoolConfiguration (Prelude.Maybe WarmPoolState)
warmPoolConfiguration_poolState = Lens.lens (\WarmPoolConfiguration' {poolState} -> poolState) (\s@WarmPoolConfiguration' {} a -> s {poolState = a} :: WarmPoolConfiguration)

instance Core.FromXML WarmPoolConfiguration where
  parseXML x =
    WarmPoolConfiguration'
      Prelude.<$> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "MinSize")
      Prelude.<*> (x Core..@? "MaxGroupPreparedCapacity")
      Prelude.<*> (x Core..@? "PoolState")

instance Prelude.Hashable WarmPoolConfiguration

instance Prelude.NFData WarmPoolConfiguration
