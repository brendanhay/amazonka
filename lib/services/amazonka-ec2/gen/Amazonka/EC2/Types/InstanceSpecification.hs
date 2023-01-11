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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The instance details to specify which volumes should be snapshotted.
--
-- /See:/ 'newInstanceSpecification' smart constructor.
data InstanceSpecification = InstanceSpecification'
  { -- | Excludes the root volume from being snapshotted.
    excludeBootVolume :: Prelude.Maybe Prelude.Bool,
    -- | The IDs of the data (non-root) volumes to exclude from the multi-volume
    -- snapshot set. If you specify the ID of the root volume, the request
    -- fails. To exclude the root volume, use __ExcludeBootVolume__.
    --
    -- You can specify up to 40 volume IDs per request.
    excludeDataVolumeIds :: Prelude.Maybe [Prelude.Text],
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
-- 'excludeBootVolume', 'instanceSpecification_excludeBootVolume' - Excludes the root volume from being snapshotted.
--
-- 'excludeDataVolumeIds', 'instanceSpecification_excludeDataVolumeIds' - The IDs of the data (non-root) volumes to exclude from the multi-volume
-- snapshot set. If you specify the ID of the root volume, the request
-- fails. To exclude the root volume, use __ExcludeBootVolume__.
--
-- You can specify up to 40 volume IDs per request.
--
-- 'instanceId', 'instanceSpecification_instanceId' - The instance to specify which volumes should be snapshotted.
newInstanceSpecification ::
  InstanceSpecification
newInstanceSpecification =
  InstanceSpecification'
    { excludeBootVolume =
        Prelude.Nothing,
      excludeDataVolumeIds = Prelude.Nothing,
      instanceId = Prelude.Nothing
    }

-- | Excludes the root volume from being snapshotted.
instanceSpecification_excludeBootVolume :: Lens.Lens' InstanceSpecification (Prelude.Maybe Prelude.Bool)
instanceSpecification_excludeBootVolume = Lens.lens (\InstanceSpecification' {excludeBootVolume} -> excludeBootVolume) (\s@InstanceSpecification' {} a -> s {excludeBootVolume = a} :: InstanceSpecification)

-- | The IDs of the data (non-root) volumes to exclude from the multi-volume
-- snapshot set. If you specify the ID of the root volume, the request
-- fails. To exclude the root volume, use __ExcludeBootVolume__.
--
-- You can specify up to 40 volume IDs per request.
instanceSpecification_excludeDataVolumeIds :: Lens.Lens' InstanceSpecification (Prelude.Maybe [Prelude.Text])
instanceSpecification_excludeDataVolumeIds = Lens.lens (\InstanceSpecification' {excludeDataVolumeIds} -> excludeDataVolumeIds) (\s@InstanceSpecification' {} a -> s {excludeDataVolumeIds = a} :: InstanceSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The instance to specify which volumes should be snapshotted.
instanceSpecification_instanceId :: Lens.Lens' InstanceSpecification (Prelude.Maybe Prelude.Text)
instanceSpecification_instanceId = Lens.lens (\InstanceSpecification' {instanceId} -> instanceId) (\s@InstanceSpecification' {} a -> s {instanceId = a} :: InstanceSpecification)

instance Prelude.Hashable InstanceSpecification where
  hashWithSalt _salt InstanceSpecification' {..} =
    _salt `Prelude.hashWithSalt` excludeBootVolume
      `Prelude.hashWithSalt` excludeDataVolumeIds
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData InstanceSpecification where
  rnf InstanceSpecification' {..} =
    Prelude.rnf excludeBootVolume
      `Prelude.seq` Prelude.rnf excludeDataVolumeIds
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToQuery InstanceSpecification where
  toQuery InstanceSpecification' {..} =
    Prelude.mconcat
      [ "ExcludeBootVolume" Data.=: excludeBootVolume,
        Data.toQuery
          ( Data.toQueryList "ExcludeDataVolumeId"
              Prelude.<$> excludeDataVolumeIds
          ),
        "InstanceId" Data.=: instanceId
      ]
