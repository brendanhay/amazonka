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
-- Module      : Amazonka.DataSync.Types.NetAppONTAPVolume
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.NetAppONTAPVolume where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types.MaxP95Performance
import Amazonka.DataSync.Types.Recommendation
import Amazonka.DataSync.Types.RecommendationStatus
import qualified Amazonka.Prelude as Prelude

-- | The information that DataSync Discovery collects about a volume in your
-- on-premises storage system.
--
-- /See:/ 'newNetAppONTAPVolume' smart constructor.
data NetAppONTAPVolume = NetAppONTAPVolume'
  { -- | The total storage space that\'s available in the volume.
    capacityProvisioned :: Prelude.Maybe Prelude.Natural,
    -- | The storage space that\'s being used in the volume.
    capacityUsed :: Prelude.Maybe Prelude.Natural,
    -- | The number of CIFS shares in the volume.
    cifsShareCount :: Prelude.Maybe Prelude.Natural,
    -- | The storage space that\'s being used in the volume without accounting
    -- for compression or deduplication.
    logicalCapacityUsed :: Prelude.Maybe Prelude.Natural,
    -- | The performance data that DataSync Discovery collects about the volume.
    maxP95Performance :: Prelude.Maybe MaxP95Performance,
    -- | The number of NFS volumes in the volume.
    nfsExported :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether DataSync Discovery recommendations for the volume are
    -- ready to view, incomplete, or can\'t be determined.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-job-statuses.html#recommendation-statuses-table Recommendation statuses>.
    recommendationStatus :: Prelude.Maybe RecommendationStatus,
    -- | The Amazon Web Services storage services that DataSync Discovery
    -- recommends for the volume. For more information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-understand-recommendations.html Recommendations provided by DataSync Discovery>.
    recommendations :: Prelude.Maybe [Recommendation],
    -- | The universally unique identifier (UUID) of the volume.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The volume\'s security style (such as Unix or NTFS).
    securityStyle :: Prelude.Maybe Prelude.Text,
    -- | The amount of storage in the volume that\'s being used for snapshots.
    snapshotCapacityUsed :: Prelude.Maybe Prelude.Natural,
    -- | The name of the SVM associated with the volume.
    svmName :: Prelude.Maybe Prelude.Text,
    -- | The UUID of the storage virtual machine (SVM) associated with the
    -- volume.
    svmUuid :: Prelude.Maybe Prelude.Text,
    -- | The name of the volume.
    volumeName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetAppONTAPVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityProvisioned', 'netAppONTAPVolume_capacityProvisioned' - The total storage space that\'s available in the volume.
--
-- 'capacityUsed', 'netAppONTAPVolume_capacityUsed' - The storage space that\'s being used in the volume.
--
-- 'cifsShareCount', 'netAppONTAPVolume_cifsShareCount' - The number of CIFS shares in the volume.
--
-- 'logicalCapacityUsed', 'netAppONTAPVolume_logicalCapacityUsed' - The storage space that\'s being used in the volume without accounting
-- for compression or deduplication.
--
-- 'maxP95Performance', 'netAppONTAPVolume_maxP95Performance' - The performance data that DataSync Discovery collects about the volume.
--
-- 'nfsExported', 'netAppONTAPVolume_nfsExported' - The number of NFS volumes in the volume.
--
-- 'recommendationStatus', 'netAppONTAPVolume_recommendationStatus' - Indicates whether DataSync Discovery recommendations for the volume are
-- ready to view, incomplete, or can\'t be determined.
--
-- For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-job-statuses.html#recommendation-statuses-table Recommendation statuses>.
--
-- 'recommendations', 'netAppONTAPVolume_recommendations' - The Amazon Web Services storage services that DataSync Discovery
-- recommends for the volume. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-understand-recommendations.html Recommendations provided by DataSync Discovery>.
--
-- 'resourceId', 'netAppONTAPVolume_resourceId' - The universally unique identifier (UUID) of the volume.
--
-- 'securityStyle', 'netAppONTAPVolume_securityStyle' - The volume\'s security style (such as Unix or NTFS).
--
-- 'snapshotCapacityUsed', 'netAppONTAPVolume_snapshotCapacityUsed' - The amount of storage in the volume that\'s being used for snapshots.
--
-- 'svmName', 'netAppONTAPVolume_svmName' - The name of the SVM associated with the volume.
--
-- 'svmUuid', 'netAppONTAPVolume_svmUuid' - The UUID of the storage virtual machine (SVM) associated with the
-- volume.
--
-- 'volumeName', 'netAppONTAPVolume_volumeName' - The name of the volume.
newNetAppONTAPVolume ::
  NetAppONTAPVolume
newNetAppONTAPVolume =
  NetAppONTAPVolume'
    { capacityProvisioned =
        Prelude.Nothing,
      capacityUsed = Prelude.Nothing,
      cifsShareCount = Prelude.Nothing,
      logicalCapacityUsed = Prelude.Nothing,
      maxP95Performance = Prelude.Nothing,
      nfsExported = Prelude.Nothing,
      recommendationStatus = Prelude.Nothing,
      recommendations = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      securityStyle = Prelude.Nothing,
      snapshotCapacityUsed = Prelude.Nothing,
      svmName = Prelude.Nothing,
      svmUuid = Prelude.Nothing,
      volumeName = Prelude.Nothing
    }

-- | The total storage space that\'s available in the volume.
netAppONTAPVolume_capacityProvisioned :: Lens.Lens' NetAppONTAPVolume (Prelude.Maybe Prelude.Natural)
netAppONTAPVolume_capacityProvisioned = Lens.lens (\NetAppONTAPVolume' {capacityProvisioned} -> capacityProvisioned) (\s@NetAppONTAPVolume' {} a -> s {capacityProvisioned = a} :: NetAppONTAPVolume)

-- | The storage space that\'s being used in the volume.
netAppONTAPVolume_capacityUsed :: Lens.Lens' NetAppONTAPVolume (Prelude.Maybe Prelude.Natural)
netAppONTAPVolume_capacityUsed = Lens.lens (\NetAppONTAPVolume' {capacityUsed} -> capacityUsed) (\s@NetAppONTAPVolume' {} a -> s {capacityUsed = a} :: NetAppONTAPVolume)

-- | The number of CIFS shares in the volume.
netAppONTAPVolume_cifsShareCount :: Lens.Lens' NetAppONTAPVolume (Prelude.Maybe Prelude.Natural)
netAppONTAPVolume_cifsShareCount = Lens.lens (\NetAppONTAPVolume' {cifsShareCount} -> cifsShareCount) (\s@NetAppONTAPVolume' {} a -> s {cifsShareCount = a} :: NetAppONTAPVolume)

-- | The storage space that\'s being used in the volume without accounting
-- for compression or deduplication.
netAppONTAPVolume_logicalCapacityUsed :: Lens.Lens' NetAppONTAPVolume (Prelude.Maybe Prelude.Natural)
netAppONTAPVolume_logicalCapacityUsed = Lens.lens (\NetAppONTAPVolume' {logicalCapacityUsed} -> logicalCapacityUsed) (\s@NetAppONTAPVolume' {} a -> s {logicalCapacityUsed = a} :: NetAppONTAPVolume)

-- | The performance data that DataSync Discovery collects about the volume.
netAppONTAPVolume_maxP95Performance :: Lens.Lens' NetAppONTAPVolume (Prelude.Maybe MaxP95Performance)
netAppONTAPVolume_maxP95Performance = Lens.lens (\NetAppONTAPVolume' {maxP95Performance} -> maxP95Performance) (\s@NetAppONTAPVolume' {} a -> s {maxP95Performance = a} :: NetAppONTAPVolume)

-- | The number of NFS volumes in the volume.
netAppONTAPVolume_nfsExported :: Lens.Lens' NetAppONTAPVolume (Prelude.Maybe Prelude.Bool)
netAppONTAPVolume_nfsExported = Lens.lens (\NetAppONTAPVolume' {nfsExported} -> nfsExported) (\s@NetAppONTAPVolume' {} a -> s {nfsExported = a} :: NetAppONTAPVolume)

-- | Indicates whether DataSync Discovery recommendations for the volume are
-- ready to view, incomplete, or can\'t be determined.
--
-- For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-job-statuses.html#recommendation-statuses-table Recommendation statuses>.
netAppONTAPVolume_recommendationStatus :: Lens.Lens' NetAppONTAPVolume (Prelude.Maybe RecommendationStatus)
netAppONTAPVolume_recommendationStatus = Lens.lens (\NetAppONTAPVolume' {recommendationStatus} -> recommendationStatus) (\s@NetAppONTAPVolume' {} a -> s {recommendationStatus = a} :: NetAppONTAPVolume)

-- | The Amazon Web Services storage services that DataSync Discovery
-- recommends for the volume. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-understand-recommendations.html Recommendations provided by DataSync Discovery>.
netAppONTAPVolume_recommendations :: Lens.Lens' NetAppONTAPVolume (Prelude.Maybe [Recommendation])
netAppONTAPVolume_recommendations = Lens.lens (\NetAppONTAPVolume' {recommendations} -> recommendations) (\s@NetAppONTAPVolume' {} a -> s {recommendations = a} :: NetAppONTAPVolume) Prelude.. Lens.mapping Lens.coerced

-- | The universally unique identifier (UUID) of the volume.
netAppONTAPVolume_resourceId :: Lens.Lens' NetAppONTAPVolume (Prelude.Maybe Prelude.Text)
netAppONTAPVolume_resourceId = Lens.lens (\NetAppONTAPVolume' {resourceId} -> resourceId) (\s@NetAppONTAPVolume' {} a -> s {resourceId = a} :: NetAppONTAPVolume)

-- | The volume\'s security style (such as Unix or NTFS).
netAppONTAPVolume_securityStyle :: Lens.Lens' NetAppONTAPVolume (Prelude.Maybe Prelude.Text)
netAppONTAPVolume_securityStyle = Lens.lens (\NetAppONTAPVolume' {securityStyle} -> securityStyle) (\s@NetAppONTAPVolume' {} a -> s {securityStyle = a} :: NetAppONTAPVolume)

-- | The amount of storage in the volume that\'s being used for snapshots.
netAppONTAPVolume_snapshotCapacityUsed :: Lens.Lens' NetAppONTAPVolume (Prelude.Maybe Prelude.Natural)
netAppONTAPVolume_snapshotCapacityUsed = Lens.lens (\NetAppONTAPVolume' {snapshotCapacityUsed} -> snapshotCapacityUsed) (\s@NetAppONTAPVolume' {} a -> s {snapshotCapacityUsed = a} :: NetAppONTAPVolume)

-- | The name of the SVM associated with the volume.
netAppONTAPVolume_svmName :: Lens.Lens' NetAppONTAPVolume (Prelude.Maybe Prelude.Text)
netAppONTAPVolume_svmName = Lens.lens (\NetAppONTAPVolume' {svmName} -> svmName) (\s@NetAppONTAPVolume' {} a -> s {svmName = a} :: NetAppONTAPVolume)

-- | The UUID of the storage virtual machine (SVM) associated with the
-- volume.
netAppONTAPVolume_svmUuid :: Lens.Lens' NetAppONTAPVolume (Prelude.Maybe Prelude.Text)
netAppONTAPVolume_svmUuid = Lens.lens (\NetAppONTAPVolume' {svmUuid} -> svmUuid) (\s@NetAppONTAPVolume' {} a -> s {svmUuid = a} :: NetAppONTAPVolume)

-- | The name of the volume.
netAppONTAPVolume_volumeName :: Lens.Lens' NetAppONTAPVolume (Prelude.Maybe Prelude.Text)
netAppONTAPVolume_volumeName = Lens.lens (\NetAppONTAPVolume' {volumeName} -> volumeName) (\s@NetAppONTAPVolume' {} a -> s {volumeName = a} :: NetAppONTAPVolume)

instance Data.FromJSON NetAppONTAPVolume where
  parseJSON =
    Data.withObject
      "NetAppONTAPVolume"
      ( \x ->
          NetAppONTAPVolume'
            Prelude.<$> (x Data..:? "CapacityProvisioned")
            Prelude.<*> (x Data..:? "CapacityUsed")
            Prelude.<*> (x Data..:? "CifsShareCount")
            Prelude.<*> (x Data..:? "LogicalCapacityUsed")
            Prelude.<*> (x Data..:? "MaxP95Performance")
            Prelude.<*> (x Data..:? "NfsExported")
            Prelude.<*> (x Data..:? "RecommendationStatus")
            Prelude.<*> ( x
                            Data..:? "Recommendations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ResourceId")
            Prelude.<*> (x Data..:? "SecurityStyle")
            Prelude.<*> (x Data..:? "SnapshotCapacityUsed")
            Prelude.<*> (x Data..:? "SvmName")
            Prelude.<*> (x Data..:? "SvmUuid")
            Prelude.<*> (x Data..:? "VolumeName")
      )

instance Prelude.Hashable NetAppONTAPVolume where
  hashWithSalt _salt NetAppONTAPVolume' {..} =
    _salt
      `Prelude.hashWithSalt` capacityProvisioned
      `Prelude.hashWithSalt` capacityUsed
      `Prelude.hashWithSalt` cifsShareCount
      `Prelude.hashWithSalt` logicalCapacityUsed
      `Prelude.hashWithSalt` maxP95Performance
      `Prelude.hashWithSalt` nfsExported
      `Prelude.hashWithSalt` recommendationStatus
      `Prelude.hashWithSalt` recommendations
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` securityStyle
      `Prelude.hashWithSalt` snapshotCapacityUsed
      `Prelude.hashWithSalt` svmName
      `Prelude.hashWithSalt` svmUuid
      `Prelude.hashWithSalt` volumeName

instance Prelude.NFData NetAppONTAPVolume where
  rnf NetAppONTAPVolume' {..} =
    Prelude.rnf capacityProvisioned
      `Prelude.seq` Prelude.rnf capacityUsed
      `Prelude.seq` Prelude.rnf cifsShareCount
      `Prelude.seq` Prelude.rnf logicalCapacityUsed
      `Prelude.seq` Prelude.rnf maxP95Performance
      `Prelude.seq` Prelude.rnf nfsExported
      `Prelude.seq` Prelude.rnf recommendationStatus
      `Prelude.seq` Prelude.rnf recommendations
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf securityStyle
      `Prelude.seq` Prelude.rnf snapshotCapacityUsed
      `Prelude.seq` Prelude.rnf svmName
      `Prelude.seq` Prelude.rnf svmUuid
      `Prelude.seq` Prelude.rnf volumeName
