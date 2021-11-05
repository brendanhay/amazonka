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
-- Module      : Network.AWS.MemoryDb.Types.ServiceUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MemoryDb.Types.ServiceUpdate where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MemoryDb.Types.ServiceUpdateStatus
import Network.AWS.MemoryDb.Types.ServiceUpdateType
import qualified Network.AWS.Prelude as Prelude

-- | An update that you can apply to your MemoryDB clusters.
--
-- /See:/ 'newServiceUpdate' smart constructor.
data ServiceUpdate = ServiceUpdate'
  { -- | The status of the service update
    status :: Prelude.Maybe ServiceUpdateStatus,
    -- | The date at which the service update will be automatically applied
    autoUpdateStartDate :: Prelude.Maybe Core.POSIX,
    -- | The unique ID of the service update
    serviceUpdateName :: Prelude.Maybe Prelude.Text,
    -- | A list of nodes updated by the service update
    nodesUpdated :: Prelude.Maybe Prelude.Text,
    -- | The date when the service update is initially available
    releaseDate :: Prelude.Maybe Core.POSIX,
    -- | The name of the cluster to which the service update applies
    clusterName :: Prelude.Maybe Prelude.Text,
    -- | Reflects the nature of the service update
    type' :: Prelude.Maybe ServiceUpdateType,
    -- | Provides details of the service update
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'serviceUpdate_status' - The status of the service update
--
-- 'autoUpdateStartDate', 'serviceUpdate_autoUpdateStartDate' - The date at which the service update will be automatically applied
--
-- 'serviceUpdateName', 'serviceUpdate_serviceUpdateName' - The unique ID of the service update
--
-- 'nodesUpdated', 'serviceUpdate_nodesUpdated' - A list of nodes updated by the service update
--
-- 'releaseDate', 'serviceUpdate_releaseDate' - The date when the service update is initially available
--
-- 'clusterName', 'serviceUpdate_clusterName' - The name of the cluster to which the service update applies
--
-- 'type'', 'serviceUpdate_type' - Reflects the nature of the service update
--
-- 'description', 'serviceUpdate_description' - Provides details of the service update
newServiceUpdate ::
  ServiceUpdate
newServiceUpdate =
  ServiceUpdate'
    { status = Prelude.Nothing,
      autoUpdateStartDate = Prelude.Nothing,
      serviceUpdateName = Prelude.Nothing,
      nodesUpdated = Prelude.Nothing,
      releaseDate = Prelude.Nothing,
      clusterName = Prelude.Nothing,
      type' = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The status of the service update
serviceUpdate_status :: Lens.Lens' ServiceUpdate (Prelude.Maybe ServiceUpdateStatus)
serviceUpdate_status = Lens.lens (\ServiceUpdate' {status} -> status) (\s@ServiceUpdate' {} a -> s {status = a} :: ServiceUpdate)

-- | The date at which the service update will be automatically applied
serviceUpdate_autoUpdateStartDate :: Lens.Lens' ServiceUpdate (Prelude.Maybe Prelude.UTCTime)
serviceUpdate_autoUpdateStartDate = Lens.lens (\ServiceUpdate' {autoUpdateStartDate} -> autoUpdateStartDate) (\s@ServiceUpdate' {} a -> s {autoUpdateStartDate = a} :: ServiceUpdate) Prelude.. Lens.mapping Core._Time

-- | The unique ID of the service update
serviceUpdate_serviceUpdateName :: Lens.Lens' ServiceUpdate (Prelude.Maybe Prelude.Text)
serviceUpdate_serviceUpdateName = Lens.lens (\ServiceUpdate' {serviceUpdateName} -> serviceUpdateName) (\s@ServiceUpdate' {} a -> s {serviceUpdateName = a} :: ServiceUpdate)

-- | A list of nodes updated by the service update
serviceUpdate_nodesUpdated :: Lens.Lens' ServiceUpdate (Prelude.Maybe Prelude.Text)
serviceUpdate_nodesUpdated = Lens.lens (\ServiceUpdate' {nodesUpdated} -> nodesUpdated) (\s@ServiceUpdate' {} a -> s {nodesUpdated = a} :: ServiceUpdate)

-- | The date when the service update is initially available
serviceUpdate_releaseDate :: Lens.Lens' ServiceUpdate (Prelude.Maybe Prelude.UTCTime)
serviceUpdate_releaseDate = Lens.lens (\ServiceUpdate' {releaseDate} -> releaseDate) (\s@ServiceUpdate' {} a -> s {releaseDate = a} :: ServiceUpdate) Prelude.. Lens.mapping Core._Time

-- | The name of the cluster to which the service update applies
serviceUpdate_clusterName :: Lens.Lens' ServiceUpdate (Prelude.Maybe Prelude.Text)
serviceUpdate_clusterName = Lens.lens (\ServiceUpdate' {clusterName} -> clusterName) (\s@ServiceUpdate' {} a -> s {clusterName = a} :: ServiceUpdate)

-- | Reflects the nature of the service update
serviceUpdate_type :: Lens.Lens' ServiceUpdate (Prelude.Maybe ServiceUpdateType)
serviceUpdate_type = Lens.lens (\ServiceUpdate' {type'} -> type') (\s@ServiceUpdate' {} a -> s {type' = a} :: ServiceUpdate)

-- | Provides details of the service update
serviceUpdate_description :: Lens.Lens' ServiceUpdate (Prelude.Maybe Prelude.Text)
serviceUpdate_description = Lens.lens (\ServiceUpdate' {description} -> description) (\s@ServiceUpdate' {} a -> s {description = a} :: ServiceUpdate)

instance Core.FromJSON ServiceUpdate where
  parseJSON =
    Core.withObject
      "ServiceUpdate"
      ( \x ->
          ServiceUpdate'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "AutoUpdateStartDate")
            Prelude.<*> (x Core..:? "ServiceUpdateName")
            Prelude.<*> (x Core..:? "NodesUpdated")
            Prelude.<*> (x Core..:? "ReleaseDate")
            Prelude.<*> (x Core..:? "ClusterName")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable ServiceUpdate

instance Prelude.NFData ServiceUpdate
