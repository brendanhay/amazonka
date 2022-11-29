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
-- Module      : Amazonka.EC2.Types.FastLaunchSnapshotConfigurationRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FastLaunchSnapshotConfigurationRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Configuration settings for creating and managing pre-provisioned
-- snapshots for a fast-launch enabled Windows AMI.
--
-- /See:/ 'newFastLaunchSnapshotConfigurationRequest' smart constructor.
data FastLaunchSnapshotConfigurationRequest = FastLaunchSnapshotConfigurationRequest'
  { -- | The number of pre-provisioned snapshots to keep on hand for a
    -- fast-launch enabled Windows AMI.
    targetResourceCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FastLaunchSnapshotConfigurationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetResourceCount', 'fastLaunchSnapshotConfigurationRequest_targetResourceCount' - The number of pre-provisioned snapshots to keep on hand for a
-- fast-launch enabled Windows AMI.
newFastLaunchSnapshotConfigurationRequest ::
  FastLaunchSnapshotConfigurationRequest
newFastLaunchSnapshotConfigurationRequest =
  FastLaunchSnapshotConfigurationRequest'
    { targetResourceCount =
        Prelude.Nothing
    }

-- | The number of pre-provisioned snapshots to keep on hand for a
-- fast-launch enabled Windows AMI.
fastLaunchSnapshotConfigurationRequest_targetResourceCount :: Lens.Lens' FastLaunchSnapshotConfigurationRequest (Prelude.Maybe Prelude.Int)
fastLaunchSnapshotConfigurationRequest_targetResourceCount = Lens.lens (\FastLaunchSnapshotConfigurationRequest' {targetResourceCount} -> targetResourceCount) (\s@FastLaunchSnapshotConfigurationRequest' {} a -> s {targetResourceCount = a} :: FastLaunchSnapshotConfigurationRequest)

instance
  Prelude.Hashable
    FastLaunchSnapshotConfigurationRequest
  where
  hashWithSalt
    _salt
    FastLaunchSnapshotConfigurationRequest' {..} =
      _salt `Prelude.hashWithSalt` targetResourceCount

instance
  Prelude.NFData
    FastLaunchSnapshotConfigurationRequest
  where
  rnf FastLaunchSnapshotConfigurationRequest' {..} =
    Prelude.rnf targetResourceCount

instance
  Core.ToQuery
    FastLaunchSnapshotConfigurationRequest
  where
  toQuery FastLaunchSnapshotConfigurationRequest' {..} =
    Prelude.mconcat
      ["TargetResourceCount" Core.=: targetResourceCount]
