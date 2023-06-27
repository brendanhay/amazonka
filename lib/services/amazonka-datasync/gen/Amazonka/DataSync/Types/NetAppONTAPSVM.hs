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
-- Module      : Amazonka.DataSync.Types.NetAppONTAPSVM
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.NetAppONTAPSVM where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types.MaxP95Performance
import Amazonka.DataSync.Types.Recommendation
import Amazonka.DataSync.Types.RecommendationStatus
import qualified Amazonka.Prelude as Prelude

-- | The information that DataSync Discovery collects about a storage virtual
-- machine (SVM) in your on-premises storage system.
--
-- /See:/ 'newNetAppONTAPSVM' smart constructor.
data NetAppONTAPSVM = NetAppONTAPSVM'
  { -- | The number of CIFS shares in the SVM.
    cifsShareCount :: Prelude.Maybe Prelude.Natural,
    -- | The universally unique identifier (UUID) of the cluster associated with
    -- the SVM.
    clusterUuid :: Prelude.Maybe Prelude.Text,
    -- | The data transfer protocols (such as NFS) configured for the SVM.
    enabledProtocols :: Prelude.Maybe [Prelude.Text],
    -- | The performance data that DataSync Discovery collects about the SVM.
    maxP95Performance :: Prelude.Maybe MaxP95Performance,
    -- | The number of NFS volumes in the SVM.
    nfsExportedVolumes :: Prelude.Maybe Prelude.Natural,
    -- | Indicates whether DataSync Discovery recommendations for the SVM are
    -- ready to view, incomplete, or can\'t be determined.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-job-statuses.html#recommendation-statuses-table Recommendation statuses>.
    recommendationStatus :: Prelude.Maybe RecommendationStatus,
    -- | The Amazon Web Services storage services that DataSync Discovery
    -- recommends for the SVM. For more information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-understand-recommendations.html Recommendations provided by DataSync Discovery>.
    recommendations :: Prelude.Maybe [Recommendation],
    -- | The UUID of the SVM.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The name of the SVM
    svmName :: Prelude.Maybe Prelude.Text,
    -- | The total storage space that\'s available in the SVM.
    totalCapacityProvisioned :: Prelude.Maybe Prelude.Natural,
    -- | The storage space that\'s being used in the SVM.
    totalCapacityUsed :: Prelude.Maybe Prelude.Natural,
    -- | The storage space that\'s being used in the SVM without accounting for
    -- compression or deduplication.
    totalLogicalCapacityUsed :: Prelude.Maybe Prelude.Natural,
    -- | The amount of storage in the SVM that\'s being used for snapshots.
    totalSnapshotCapacityUsed :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetAppONTAPSVM' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cifsShareCount', 'netAppONTAPSVM_cifsShareCount' - The number of CIFS shares in the SVM.
--
-- 'clusterUuid', 'netAppONTAPSVM_clusterUuid' - The universally unique identifier (UUID) of the cluster associated with
-- the SVM.
--
-- 'enabledProtocols', 'netAppONTAPSVM_enabledProtocols' - The data transfer protocols (such as NFS) configured for the SVM.
--
-- 'maxP95Performance', 'netAppONTAPSVM_maxP95Performance' - The performance data that DataSync Discovery collects about the SVM.
--
-- 'nfsExportedVolumes', 'netAppONTAPSVM_nfsExportedVolumes' - The number of NFS volumes in the SVM.
--
-- 'recommendationStatus', 'netAppONTAPSVM_recommendationStatus' - Indicates whether DataSync Discovery recommendations for the SVM are
-- ready to view, incomplete, or can\'t be determined.
--
-- For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-job-statuses.html#recommendation-statuses-table Recommendation statuses>.
--
-- 'recommendations', 'netAppONTAPSVM_recommendations' - The Amazon Web Services storage services that DataSync Discovery
-- recommends for the SVM. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-understand-recommendations.html Recommendations provided by DataSync Discovery>.
--
-- 'resourceId', 'netAppONTAPSVM_resourceId' - The UUID of the SVM.
--
-- 'svmName', 'netAppONTAPSVM_svmName' - The name of the SVM
--
-- 'totalCapacityProvisioned', 'netAppONTAPSVM_totalCapacityProvisioned' - The total storage space that\'s available in the SVM.
--
-- 'totalCapacityUsed', 'netAppONTAPSVM_totalCapacityUsed' - The storage space that\'s being used in the SVM.
--
-- 'totalLogicalCapacityUsed', 'netAppONTAPSVM_totalLogicalCapacityUsed' - The storage space that\'s being used in the SVM without accounting for
-- compression or deduplication.
--
-- 'totalSnapshotCapacityUsed', 'netAppONTAPSVM_totalSnapshotCapacityUsed' - The amount of storage in the SVM that\'s being used for snapshots.
newNetAppONTAPSVM ::
  NetAppONTAPSVM
newNetAppONTAPSVM =
  NetAppONTAPSVM'
    { cifsShareCount = Prelude.Nothing,
      clusterUuid = Prelude.Nothing,
      enabledProtocols = Prelude.Nothing,
      maxP95Performance = Prelude.Nothing,
      nfsExportedVolumes = Prelude.Nothing,
      recommendationStatus = Prelude.Nothing,
      recommendations = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      svmName = Prelude.Nothing,
      totalCapacityProvisioned = Prelude.Nothing,
      totalCapacityUsed = Prelude.Nothing,
      totalLogicalCapacityUsed = Prelude.Nothing,
      totalSnapshotCapacityUsed = Prelude.Nothing
    }

-- | The number of CIFS shares in the SVM.
netAppONTAPSVM_cifsShareCount :: Lens.Lens' NetAppONTAPSVM (Prelude.Maybe Prelude.Natural)
netAppONTAPSVM_cifsShareCount = Lens.lens (\NetAppONTAPSVM' {cifsShareCount} -> cifsShareCount) (\s@NetAppONTAPSVM' {} a -> s {cifsShareCount = a} :: NetAppONTAPSVM)

-- | The universally unique identifier (UUID) of the cluster associated with
-- the SVM.
netAppONTAPSVM_clusterUuid :: Lens.Lens' NetAppONTAPSVM (Prelude.Maybe Prelude.Text)
netAppONTAPSVM_clusterUuid = Lens.lens (\NetAppONTAPSVM' {clusterUuid} -> clusterUuid) (\s@NetAppONTAPSVM' {} a -> s {clusterUuid = a} :: NetAppONTAPSVM)

-- | The data transfer protocols (such as NFS) configured for the SVM.
netAppONTAPSVM_enabledProtocols :: Lens.Lens' NetAppONTAPSVM (Prelude.Maybe [Prelude.Text])
netAppONTAPSVM_enabledProtocols = Lens.lens (\NetAppONTAPSVM' {enabledProtocols} -> enabledProtocols) (\s@NetAppONTAPSVM' {} a -> s {enabledProtocols = a} :: NetAppONTAPSVM) Prelude.. Lens.mapping Lens.coerced

-- | The performance data that DataSync Discovery collects about the SVM.
netAppONTAPSVM_maxP95Performance :: Lens.Lens' NetAppONTAPSVM (Prelude.Maybe MaxP95Performance)
netAppONTAPSVM_maxP95Performance = Lens.lens (\NetAppONTAPSVM' {maxP95Performance} -> maxP95Performance) (\s@NetAppONTAPSVM' {} a -> s {maxP95Performance = a} :: NetAppONTAPSVM)

-- | The number of NFS volumes in the SVM.
netAppONTAPSVM_nfsExportedVolumes :: Lens.Lens' NetAppONTAPSVM (Prelude.Maybe Prelude.Natural)
netAppONTAPSVM_nfsExportedVolumes = Lens.lens (\NetAppONTAPSVM' {nfsExportedVolumes} -> nfsExportedVolumes) (\s@NetAppONTAPSVM' {} a -> s {nfsExportedVolumes = a} :: NetAppONTAPSVM)

-- | Indicates whether DataSync Discovery recommendations for the SVM are
-- ready to view, incomplete, or can\'t be determined.
--
-- For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-job-statuses.html#recommendation-statuses-table Recommendation statuses>.
netAppONTAPSVM_recommendationStatus :: Lens.Lens' NetAppONTAPSVM (Prelude.Maybe RecommendationStatus)
netAppONTAPSVM_recommendationStatus = Lens.lens (\NetAppONTAPSVM' {recommendationStatus} -> recommendationStatus) (\s@NetAppONTAPSVM' {} a -> s {recommendationStatus = a} :: NetAppONTAPSVM)

-- | The Amazon Web Services storage services that DataSync Discovery
-- recommends for the SVM. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-understand-recommendations.html Recommendations provided by DataSync Discovery>.
netAppONTAPSVM_recommendations :: Lens.Lens' NetAppONTAPSVM (Prelude.Maybe [Recommendation])
netAppONTAPSVM_recommendations = Lens.lens (\NetAppONTAPSVM' {recommendations} -> recommendations) (\s@NetAppONTAPSVM' {} a -> s {recommendations = a} :: NetAppONTAPSVM) Prelude.. Lens.mapping Lens.coerced

-- | The UUID of the SVM.
netAppONTAPSVM_resourceId :: Lens.Lens' NetAppONTAPSVM (Prelude.Maybe Prelude.Text)
netAppONTAPSVM_resourceId = Lens.lens (\NetAppONTAPSVM' {resourceId} -> resourceId) (\s@NetAppONTAPSVM' {} a -> s {resourceId = a} :: NetAppONTAPSVM)

-- | The name of the SVM
netAppONTAPSVM_svmName :: Lens.Lens' NetAppONTAPSVM (Prelude.Maybe Prelude.Text)
netAppONTAPSVM_svmName = Lens.lens (\NetAppONTAPSVM' {svmName} -> svmName) (\s@NetAppONTAPSVM' {} a -> s {svmName = a} :: NetAppONTAPSVM)

-- | The total storage space that\'s available in the SVM.
netAppONTAPSVM_totalCapacityProvisioned :: Lens.Lens' NetAppONTAPSVM (Prelude.Maybe Prelude.Natural)
netAppONTAPSVM_totalCapacityProvisioned = Lens.lens (\NetAppONTAPSVM' {totalCapacityProvisioned} -> totalCapacityProvisioned) (\s@NetAppONTAPSVM' {} a -> s {totalCapacityProvisioned = a} :: NetAppONTAPSVM)

-- | The storage space that\'s being used in the SVM.
netAppONTAPSVM_totalCapacityUsed :: Lens.Lens' NetAppONTAPSVM (Prelude.Maybe Prelude.Natural)
netAppONTAPSVM_totalCapacityUsed = Lens.lens (\NetAppONTAPSVM' {totalCapacityUsed} -> totalCapacityUsed) (\s@NetAppONTAPSVM' {} a -> s {totalCapacityUsed = a} :: NetAppONTAPSVM)

-- | The storage space that\'s being used in the SVM without accounting for
-- compression or deduplication.
netAppONTAPSVM_totalLogicalCapacityUsed :: Lens.Lens' NetAppONTAPSVM (Prelude.Maybe Prelude.Natural)
netAppONTAPSVM_totalLogicalCapacityUsed = Lens.lens (\NetAppONTAPSVM' {totalLogicalCapacityUsed} -> totalLogicalCapacityUsed) (\s@NetAppONTAPSVM' {} a -> s {totalLogicalCapacityUsed = a} :: NetAppONTAPSVM)

-- | The amount of storage in the SVM that\'s being used for snapshots.
netAppONTAPSVM_totalSnapshotCapacityUsed :: Lens.Lens' NetAppONTAPSVM (Prelude.Maybe Prelude.Natural)
netAppONTAPSVM_totalSnapshotCapacityUsed = Lens.lens (\NetAppONTAPSVM' {totalSnapshotCapacityUsed} -> totalSnapshotCapacityUsed) (\s@NetAppONTAPSVM' {} a -> s {totalSnapshotCapacityUsed = a} :: NetAppONTAPSVM)

instance Data.FromJSON NetAppONTAPSVM where
  parseJSON =
    Data.withObject
      "NetAppONTAPSVM"
      ( \x ->
          NetAppONTAPSVM'
            Prelude.<$> (x Data..:? "CifsShareCount")
            Prelude.<*> (x Data..:? "ClusterUuid")
            Prelude.<*> ( x
                            Data..:? "EnabledProtocols"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "MaxP95Performance")
            Prelude.<*> (x Data..:? "NfsExportedVolumes")
            Prelude.<*> (x Data..:? "RecommendationStatus")
            Prelude.<*> ( x
                            Data..:? "Recommendations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ResourceId")
            Prelude.<*> (x Data..:? "SvmName")
            Prelude.<*> (x Data..:? "TotalCapacityProvisioned")
            Prelude.<*> (x Data..:? "TotalCapacityUsed")
            Prelude.<*> (x Data..:? "TotalLogicalCapacityUsed")
            Prelude.<*> (x Data..:? "TotalSnapshotCapacityUsed")
      )

instance Prelude.Hashable NetAppONTAPSVM where
  hashWithSalt _salt NetAppONTAPSVM' {..} =
    _salt
      `Prelude.hashWithSalt` cifsShareCount
      `Prelude.hashWithSalt` clusterUuid
      `Prelude.hashWithSalt` enabledProtocols
      `Prelude.hashWithSalt` maxP95Performance
      `Prelude.hashWithSalt` nfsExportedVolumes
      `Prelude.hashWithSalt` recommendationStatus
      `Prelude.hashWithSalt` recommendations
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` svmName
      `Prelude.hashWithSalt` totalCapacityProvisioned
      `Prelude.hashWithSalt` totalCapacityUsed
      `Prelude.hashWithSalt` totalLogicalCapacityUsed
      `Prelude.hashWithSalt` totalSnapshotCapacityUsed

instance Prelude.NFData NetAppONTAPSVM where
  rnf NetAppONTAPSVM' {..} =
    Prelude.rnf cifsShareCount
      `Prelude.seq` Prelude.rnf clusterUuid
      `Prelude.seq` Prelude.rnf enabledProtocols
      `Prelude.seq` Prelude.rnf maxP95Performance
      `Prelude.seq` Prelude.rnf nfsExportedVolumes
      `Prelude.seq` Prelude.rnf recommendationStatus
      `Prelude.seq` Prelude.rnf recommendations
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf svmName
      `Prelude.seq` Prelude.rnf totalCapacityProvisioned
      `Prelude.seq` Prelude.rnf totalCapacityUsed
      `Prelude.seq` Prelude.rnf totalLogicalCapacityUsed
      `Prelude.seq` Prelude.rnf totalSnapshotCapacityUsed
