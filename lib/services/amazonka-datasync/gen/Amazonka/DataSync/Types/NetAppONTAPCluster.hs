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
-- Module      : Amazonka.DataSync.Types.NetAppONTAPCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.NetAppONTAPCluster where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types.MaxP95Performance
import Amazonka.DataSync.Types.Recommendation
import Amazonka.DataSync.Types.RecommendationStatus
import qualified Amazonka.Prelude as Prelude

-- | The information that DataSync Discovery collects about an on-premises
-- storage system cluster.
--
-- /See:/ 'newNetAppONTAPCluster' smart constructor.
data NetAppONTAPCluster = NetAppONTAPCluster'
  { -- | The number of CIFS shares in the cluster.
    cifsShareCount :: Prelude.Maybe Prelude.Natural,
    -- | The storage space that\'s being used in the cluster without accounting
    -- for compression or deduplication.
    clusterBlockStorageLogicalUsed :: Prelude.Maybe Prelude.Natural,
    -- | The total storage space that\'s available in the cluster.
    clusterBlockStorageSize :: Prelude.Maybe Prelude.Natural,
    -- | The storage space that\'s being used in a cluster.
    clusterBlockStorageUsed :: Prelude.Maybe Prelude.Natural,
    -- | The name of the cluster.
    clusterName :: Prelude.Maybe Prelude.Text,
    -- | The performance data that DataSync Discovery collects about the cluster.
    maxP95Performance :: Prelude.Maybe MaxP95Performance,
    -- | The number of NFS volumes in the cluster.
    nfsExportedVolumes :: Prelude.Maybe Prelude.Natural,
    -- | Indicates whether DataSync Discovery recommendations for the cluster are
    -- ready to view, incomplete, or can\'t be determined.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-job-statuses.html#recommendation-statuses-table Recommendation statuses>.
    recommendationStatus :: Prelude.Maybe RecommendationStatus,
    -- | The Amazon Web Services storage services that DataSync Discovery
    -- recommends for the cluster. For more information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-understand-recommendations.html Recommendations provided by DataSync Discovery>.
    recommendations :: Prelude.Maybe [Recommendation],
    -- | The universally unique identifier (UUID) of the cluster.
    resourceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetAppONTAPCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cifsShareCount', 'netAppONTAPCluster_cifsShareCount' - The number of CIFS shares in the cluster.
--
-- 'clusterBlockStorageLogicalUsed', 'netAppONTAPCluster_clusterBlockStorageLogicalUsed' - The storage space that\'s being used in the cluster without accounting
-- for compression or deduplication.
--
-- 'clusterBlockStorageSize', 'netAppONTAPCluster_clusterBlockStorageSize' - The total storage space that\'s available in the cluster.
--
-- 'clusterBlockStorageUsed', 'netAppONTAPCluster_clusterBlockStorageUsed' - The storage space that\'s being used in a cluster.
--
-- 'clusterName', 'netAppONTAPCluster_clusterName' - The name of the cluster.
--
-- 'maxP95Performance', 'netAppONTAPCluster_maxP95Performance' - The performance data that DataSync Discovery collects about the cluster.
--
-- 'nfsExportedVolumes', 'netAppONTAPCluster_nfsExportedVolumes' - The number of NFS volumes in the cluster.
--
-- 'recommendationStatus', 'netAppONTAPCluster_recommendationStatus' - Indicates whether DataSync Discovery recommendations for the cluster are
-- ready to view, incomplete, or can\'t be determined.
--
-- For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-job-statuses.html#recommendation-statuses-table Recommendation statuses>.
--
-- 'recommendations', 'netAppONTAPCluster_recommendations' - The Amazon Web Services storage services that DataSync Discovery
-- recommends for the cluster. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-understand-recommendations.html Recommendations provided by DataSync Discovery>.
--
-- 'resourceId', 'netAppONTAPCluster_resourceId' - The universally unique identifier (UUID) of the cluster.
newNetAppONTAPCluster ::
  NetAppONTAPCluster
newNetAppONTAPCluster =
  NetAppONTAPCluster'
    { cifsShareCount =
        Prelude.Nothing,
      clusterBlockStorageLogicalUsed = Prelude.Nothing,
      clusterBlockStorageSize = Prelude.Nothing,
      clusterBlockStorageUsed = Prelude.Nothing,
      clusterName = Prelude.Nothing,
      maxP95Performance = Prelude.Nothing,
      nfsExportedVolumes = Prelude.Nothing,
      recommendationStatus = Prelude.Nothing,
      recommendations = Prelude.Nothing,
      resourceId = Prelude.Nothing
    }

-- | The number of CIFS shares in the cluster.
netAppONTAPCluster_cifsShareCount :: Lens.Lens' NetAppONTAPCluster (Prelude.Maybe Prelude.Natural)
netAppONTAPCluster_cifsShareCount = Lens.lens (\NetAppONTAPCluster' {cifsShareCount} -> cifsShareCount) (\s@NetAppONTAPCluster' {} a -> s {cifsShareCount = a} :: NetAppONTAPCluster)

-- | The storage space that\'s being used in the cluster without accounting
-- for compression or deduplication.
netAppONTAPCluster_clusterBlockStorageLogicalUsed :: Lens.Lens' NetAppONTAPCluster (Prelude.Maybe Prelude.Natural)
netAppONTAPCluster_clusterBlockStorageLogicalUsed = Lens.lens (\NetAppONTAPCluster' {clusterBlockStorageLogicalUsed} -> clusterBlockStorageLogicalUsed) (\s@NetAppONTAPCluster' {} a -> s {clusterBlockStorageLogicalUsed = a} :: NetAppONTAPCluster)

-- | The total storage space that\'s available in the cluster.
netAppONTAPCluster_clusterBlockStorageSize :: Lens.Lens' NetAppONTAPCluster (Prelude.Maybe Prelude.Natural)
netAppONTAPCluster_clusterBlockStorageSize = Lens.lens (\NetAppONTAPCluster' {clusterBlockStorageSize} -> clusterBlockStorageSize) (\s@NetAppONTAPCluster' {} a -> s {clusterBlockStorageSize = a} :: NetAppONTAPCluster)

-- | The storage space that\'s being used in a cluster.
netAppONTAPCluster_clusterBlockStorageUsed :: Lens.Lens' NetAppONTAPCluster (Prelude.Maybe Prelude.Natural)
netAppONTAPCluster_clusterBlockStorageUsed = Lens.lens (\NetAppONTAPCluster' {clusterBlockStorageUsed} -> clusterBlockStorageUsed) (\s@NetAppONTAPCluster' {} a -> s {clusterBlockStorageUsed = a} :: NetAppONTAPCluster)

-- | The name of the cluster.
netAppONTAPCluster_clusterName :: Lens.Lens' NetAppONTAPCluster (Prelude.Maybe Prelude.Text)
netAppONTAPCluster_clusterName = Lens.lens (\NetAppONTAPCluster' {clusterName} -> clusterName) (\s@NetAppONTAPCluster' {} a -> s {clusterName = a} :: NetAppONTAPCluster)

-- | The performance data that DataSync Discovery collects about the cluster.
netAppONTAPCluster_maxP95Performance :: Lens.Lens' NetAppONTAPCluster (Prelude.Maybe MaxP95Performance)
netAppONTAPCluster_maxP95Performance = Lens.lens (\NetAppONTAPCluster' {maxP95Performance} -> maxP95Performance) (\s@NetAppONTAPCluster' {} a -> s {maxP95Performance = a} :: NetAppONTAPCluster)

-- | The number of NFS volumes in the cluster.
netAppONTAPCluster_nfsExportedVolumes :: Lens.Lens' NetAppONTAPCluster (Prelude.Maybe Prelude.Natural)
netAppONTAPCluster_nfsExportedVolumes = Lens.lens (\NetAppONTAPCluster' {nfsExportedVolumes} -> nfsExportedVolumes) (\s@NetAppONTAPCluster' {} a -> s {nfsExportedVolumes = a} :: NetAppONTAPCluster)

-- | Indicates whether DataSync Discovery recommendations for the cluster are
-- ready to view, incomplete, or can\'t be determined.
--
-- For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-job-statuses.html#recommendation-statuses-table Recommendation statuses>.
netAppONTAPCluster_recommendationStatus :: Lens.Lens' NetAppONTAPCluster (Prelude.Maybe RecommendationStatus)
netAppONTAPCluster_recommendationStatus = Lens.lens (\NetAppONTAPCluster' {recommendationStatus} -> recommendationStatus) (\s@NetAppONTAPCluster' {} a -> s {recommendationStatus = a} :: NetAppONTAPCluster)

-- | The Amazon Web Services storage services that DataSync Discovery
-- recommends for the cluster. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-understand-recommendations.html Recommendations provided by DataSync Discovery>.
netAppONTAPCluster_recommendations :: Lens.Lens' NetAppONTAPCluster (Prelude.Maybe [Recommendation])
netAppONTAPCluster_recommendations = Lens.lens (\NetAppONTAPCluster' {recommendations} -> recommendations) (\s@NetAppONTAPCluster' {} a -> s {recommendations = a} :: NetAppONTAPCluster) Prelude.. Lens.mapping Lens.coerced

-- | The universally unique identifier (UUID) of the cluster.
netAppONTAPCluster_resourceId :: Lens.Lens' NetAppONTAPCluster (Prelude.Maybe Prelude.Text)
netAppONTAPCluster_resourceId = Lens.lens (\NetAppONTAPCluster' {resourceId} -> resourceId) (\s@NetAppONTAPCluster' {} a -> s {resourceId = a} :: NetAppONTAPCluster)

instance Data.FromJSON NetAppONTAPCluster where
  parseJSON =
    Data.withObject
      "NetAppONTAPCluster"
      ( \x ->
          NetAppONTAPCluster'
            Prelude.<$> (x Data..:? "CifsShareCount")
            Prelude.<*> (x Data..:? "ClusterBlockStorageLogicalUsed")
            Prelude.<*> (x Data..:? "ClusterBlockStorageSize")
            Prelude.<*> (x Data..:? "ClusterBlockStorageUsed")
            Prelude.<*> (x Data..:? "ClusterName")
            Prelude.<*> (x Data..:? "MaxP95Performance")
            Prelude.<*> (x Data..:? "NfsExportedVolumes")
            Prelude.<*> (x Data..:? "RecommendationStatus")
            Prelude.<*> ( x
                            Data..:? "Recommendations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ResourceId")
      )

instance Prelude.Hashable NetAppONTAPCluster where
  hashWithSalt _salt NetAppONTAPCluster' {..} =
    _salt
      `Prelude.hashWithSalt` cifsShareCount
      `Prelude.hashWithSalt` clusterBlockStorageLogicalUsed
      `Prelude.hashWithSalt` clusterBlockStorageSize
      `Prelude.hashWithSalt` clusterBlockStorageUsed
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` maxP95Performance
      `Prelude.hashWithSalt` nfsExportedVolumes
      `Prelude.hashWithSalt` recommendationStatus
      `Prelude.hashWithSalt` recommendations
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData NetAppONTAPCluster where
  rnf NetAppONTAPCluster' {..} =
    Prelude.rnf cifsShareCount
      `Prelude.seq` Prelude.rnf clusterBlockStorageLogicalUsed
      `Prelude.seq` Prelude.rnf clusterBlockStorageSize
      `Prelude.seq` Prelude.rnf clusterBlockStorageUsed
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf maxP95Performance
      `Prelude.seq` Prelude.rnf nfsExportedVolumes
      `Prelude.seq` Prelude.rnf recommendationStatus
      `Prelude.seq` Prelude.rnf recommendations
      `Prelude.seq` Prelude.rnf resourceId
