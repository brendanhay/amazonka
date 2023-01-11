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
-- Module      : Amazonka.EC2.Types.FastLaunchSnapshotConfigurationResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FastLaunchSnapshotConfigurationResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Configuration settings for creating and managing pre-provisioned
-- snapshots for a fast-launch enabled Windows AMI.
--
-- /See:/ 'newFastLaunchSnapshotConfigurationResponse' smart constructor.
data FastLaunchSnapshotConfigurationResponse = FastLaunchSnapshotConfigurationResponse'
  { -- | The number of pre-provisioned snapshots requested to keep on hand for a
    -- fast-launch enabled Windows AMI.
    targetResourceCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FastLaunchSnapshotConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetResourceCount', 'fastLaunchSnapshotConfigurationResponse_targetResourceCount' - The number of pre-provisioned snapshots requested to keep on hand for a
-- fast-launch enabled Windows AMI.
newFastLaunchSnapshotConfigurationResponse ::
  FastLaunchSnapshotConfigurationResponse
newFastLaunchSnapshotConfigurationResponse =
  FastLaunchSnapshotConfigurationResponse'
    { targetResourceCount =
        Prelude.Nothing
    }

-- | The number of pre-provisioned snapshots requested to keep on hand for a
-- fast-launch enabled Windows AMI.
fastLaunchSnapshotConfigurationResponse_targetResourceCount :: Lens.Lens' FastLaunchSnapshotConfigurationResponse (Prelude.Maybe Prelude.Int)
fastLaunchSnapshotConfigurationResponse_targetResourceCount = Lens.lens (\FastLaunchSnapshotConfigurationResponse' {targetResourceCount} -> targetResourceCount) (\s@FastLaunchSnapshotConfigurationResponse' {} a -> s {targetResourceCount = a} :: FastLaunchSnapshotConfigurationResponse)

instance
  Data.FromXML
    FastLaunchSnapshotConfigurationResponse
  where
  parseXML x =
    FastLaunchSnapshotConfigurationResponse'
      Prelude.<$> (x Data..@? "targetResourceCount")

instance
  Prelude.Hashable
    FastLaunchSnapshotConfigurationResponse
  where
  hashWithSalt
    _salt
    FastLaunchSnapshotConfigurationResponse' {..} =
      _salt `Prelude.hashWithSalt` targetResourceCount

instance
  Prelude.NFData
    FastLaunchSnapshotConfigurationResponse
  where
  rnf FastLaunchSnapshotConfigurationResponse' {..} =
    Prelude.rnf targetResourceCount
