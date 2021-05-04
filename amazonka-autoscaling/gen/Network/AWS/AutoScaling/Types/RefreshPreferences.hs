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
-- Module      : Network.AWS.AutoScaling.Types.RefreshPreferences
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.RefreshPreferences where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes information used to start an instance refresh.
--
-- /See:/ 'newRefreshPreferences' smart constructor.
data RefreshPreferences = RefreshPreferences'
  { -- | The amount of capacity in the Auto Scaling group that must remain
    -- healthy during an instance refresh to allow the operation to continue,
    -- as a percentage of the desired capacity of the Auto Scaling group
    -- (rounded up to the nearest integer). The default is @90@.
    minHealthyPercentage :: Prelude.Maybe Prelude.Natural,
    -- | The number of seconds until a newly launched instance is configured and
    -- ready to use. During this time, Amazon EC2 Auto Scaling does not
    -- immediately move on to the next replacement. The default is to use the
    -- value for the health check grace period defined for the group.
    instanceWarmup :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RefreshPreferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minHealthyPercentage', 'refreshPreferences_minHealthyPercentage' - The amount of capacity in the Auto Scaling group that must remain
-- healthy during an instance refresh to allow the operation to continue,
-- as a percentage of the desired capacity of the Auto Scaling group
-- (rounded up to the nearest integer). The default is @90@.
--
-- 'instanceWarmup', 'refreshPreferences_instanceWarmup' - The number of seconds until a newly launched instance is configured and
-- ready to use. During this time, Amazon EC2 Auto Scaling does not
-- immediately move on to the next replacement. The default is to use the
-- value for the health check grace period defined for the group.
newRefreshPreferences ::
  RefreshPreferences
newRefreshPreferences =
  RefreshPreferences'
    { minHealthyPercentage =
        Prelude.Nothing,
      instanceWarmup = Prelude.Nothing
    }

-- | The amount of capacity in the Auto Scaling group that must remain
-- healthy during an instance refresh to allow the operation to continue,
-- as a percentage of the desired capacity of the Auto Scaling group
-- (rounded up to the nearest integer). The default is @90@.
refreshPreferences_minHealthyPercentage :: Lens.Lens' RefreshPreferences (Prelude.Maybe Prelude.Natural)
refreshPreferences_minHealthyPercentage = Lens.lens (\RefreshPreferences' {minHealthyPercentage} -> minHealthyPercentage) (\s@RefreshPreferences' {} a -> s {minHealthyPercentage = a} :: RefreshPreferences)

-- | The number of seconds until a newly launched instance is configured and
-- ready to use. During this time, Amazon EC2 Auto Scaling does not
-- immediately move on to the next replacement. The default is to use the
-- value for the health check grace period defined for the group.
refreshPreferences_instanceWarmup :: Lens.Lens' RefreshPreferences (Prelude.Maybe Prelude.Natural)
refreshPreferences_instanceWarmup = Lens.lens (\RefreshPreferences' {instanceWarmup} -> instanceWarmup) (\s@RefreshPreferences' {} a -> s {instanceWarmup = a} :: RefreshPreferences)

instance Prelude.Hashable RefreshPreferences

instance Prelude.NFData RefreshPreferences

instance Prelude.ToQuery RefreshPreferences where
  toQuery RefreshPreferences' {..} =
    Prelude.mconcat
      [ "MinHealthyPercentage"
          Prelude.=: minHealthyPercentage,
        "InstanceWarmup" Prelude.=: instanceWarmup
      ]
