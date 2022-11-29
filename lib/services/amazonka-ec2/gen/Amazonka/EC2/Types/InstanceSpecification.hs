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
-- Module      : Amazonka.EC2.Types.InstanceSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The instance details to specify which volumes should be snapshotted.
--
-- /See:/ 'newInstanceSpecification' smart constructor.
data InstanceSpecification = InstanceSpecification'
  { -- | The IDs of the data (non-root) volumes to exclude from the multi-volume
    -- snapshot set. If you specify the ID of the root volume, the request
    -- fails. To exclude the root volume, use __ExcludeBootVolume__.
    --
    -- You can specify up to 40 volume IDs per request.
    excludeDataVolumeIds :: Prelude.Maybe [Prelude.Text],
    -- | Excludes the root volume from being snapshotted.
    excludeBootVolume :: Prelude.Maybe Prelude.Bool,
    -- | The instance to specify which volumes should be snapshotted.
    instanceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excludeDataVolumeIds', 'instanceSpecification_excludeDataVolumeIds' - The IDs of the data (non-root) volumes to exclude from the multi-volume
-- snapshot set. If you specify the ID of the root volume, the request
-- fails. To exclude the root volume, use __ExcludeBootVolume__.
--
-- You can specify up to 40 volume IDs per request.
--
-- 'excludeBootVolume', 'instanceSpecification_excludeBootVolume' - Excludes the root volume from being snapshotted.
--
-- 'instanceId', 'instanceSpecification_instanceId' - The instance to specify which volumes should be snapshotted.
newInstanceSpecification ::
  InstanceSpecification
newInstanceSpecification =
  InstanceSpecification'
    { excludeDataVolumeIds =
        Prelude.Nothing,
      excludeBootVolume = Prelude.Nothing,
      instanceId = Prelude.Nothing
    }

-- | The IDs of the data (non-root) volumes to exclude from the multi-volume
-- snapshot set. If you specify the ID of the root volume, the request
-- fails. To exclude the root volume, use __ExcludeBootVolume__.
--
-- You can specify up to 40 volume IDs per request.
instanceSpecification_excludeDataVolumeIds :: Lens.Lens' InstanceSpecification (Prelude.Maybe [Prelude.Text])
instanceSpecification_excludeDataVolumeIds = Lens.lens (\InstanceSpecification' {excludeDataVolumeIds} -> excludeDataVolumeIds) (\s@InstanceSpecification' {} a -> s {excludeDataVolumeIds = a} :: InstanceSpecification) Prelude.. Lens.mapping Lens.coerced

-- | Excludes the root volume from being snapshotted.
instanceSpecification_excludeBootVolume :: Lens.Lens' InstanceSpecification (Prelude.Maybe Prelude.Bool)
instanceSpecification_excludeBootVolume = Lens.lens (\InstanceSpecification' {excludeBootVolume} -> excludeBootVolume) (\s@InstanceSpecification' {} a -> s {excludeBootVolume = a} :: InstanceSpecification)

-- | The instance to specify which volumes should be snapshotted.
instanceSpecification_instanceId :: Lens.Lens' InstanceSpecification (Prelude.Maybe Prelude.Text)
instanceSpecification_instanceId = Lens.lens (\InstanceSpecification' {instanceId} -> instanceId) (\s@InstanceSpecification' {} a -> s {instanceId = a} :: InstanceSpecification)

instance Prelude.Hashable InstanceSpecification where
  hashWithSalt _salt InstanceSpecification' {..} =
    _salt `Prelude.hashWithSalt` excludeDataVolumeIds
      `Prelude.hashWithSalt` excludeBootVolume
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData InstanceSpecification where
  rnf InstanceSpecification' {..} =
    Prelude.rnf excludeDataVolumeIds
      `Prelude.seq` Prelude.rnf excludeBootVolume
      `Prelude.seq` Prelude.rnf instanceId

instance Core.ToQuery InstanceSpecification where
  toQuery InstanceSpecification' {..} =
    Prelude.mconcat
      [ Core.toQuery
          ( Core.toQueryList "ExcludeDataVolumeId"
              Prelude.<$> excludeDataVolumeIds
          ),
        "ExcludeBootVolume" Core.=: excludeBootVolume,
        "InstanceId" Core.=: instanceId
      ]
