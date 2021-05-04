{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.EnableSnapshotCopy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the automatic copy of snapshots from one region to another
-- region for a specified cluster.
module Network.AWS.Redshift.EnableSnapshotCopy
  ( -- * Creating a Request
    EnableSnapshotCopy (..),
    newEnableSnapshotCopy,

    -- * Request Lenses
    enableSnapshotCopy_snapshotCopyGrantName,
    enableSnapshotCopy_manualSnapshotRetentionPeriod,
    enableSnapshotCopy_retentionPeriod,
    enableSnapshotCopy_clusterIdentifier,
    enableSnapshotCopy_destinationRegion,

    -- * Destructuring the Response
    EnableSnapshotCopyResponse (..),
    newEnableSnapshotCopyResponse,

    -- * Response Lenses
    enableSnapshotCopyResponse_cluster,
    enableSnapshotCopyResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newEnableSnapshotCopy' smart constructor.
data EnableSnapshotCopy = EnableSnapshotCopy'
  { -- | The name of the snapshot copy grant to use when snapshots of an AWS
    -- KMS-encrypted cluster are copied to the destination region.
    snapshotCopyGrantName :: Prelude.Maybe Prelude.Text,
    -- | The number of days to retain newly copied snapshots in the destination
    -- AWS Region after they are copied from the source AWS Region. If the
    -- value is -1, the manual snapshot is retained indefinitely.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    manualSnapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The number of days to retain automated snapshots in the destination
    -- region after they are copied from the source region.
    --
    -- Default: 7.
    --
    -- Constraints: Must be at least 1 and no more than 35.
    retentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The unique identifier of the source cluster to copy snapshots from.
    --
    -- Constraints: Must be the valid name of an existing cluster that does not
    -- already have cross-region snapshot copy enabled.
    clusterIdentifier :: Prelude.Text,
    -- | The destination AWS Region that you want to copy snapshots to.
    --
    -- Constraints: Must be the name of a valid AWS Region. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html#redshift_region Regions and Endpoints>
    -- in the Amazon Web Services General Reference.
    destinationRegion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnableSnapshotCopy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotCopyGrantName', 'enableSnapshotCopy_snapshotCopyGrantName' - The name of the snapshot copy grant to use when snapshots of an AWS
-- KMS-encrypted cluster are copied to the destination region.
--
-- 'manualSnapshotRetentionPeriod', 'enableSnapshotCopy_manualSnapshotRetentionPeriod' - The number of days to retain newly copied snapshots in the destination
-- AWS Region after they are copied from the source AWS Region. If the
-- value is -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- 'retentionPeriod', 'enableSnapshotCopy_retentionPeriod' - The number of days to retain automated snapshots in the destination
-- region after they are copied from the source region.
--
-- Default: 7.
--
-- Constraints: Must be at least 1 and no more than 35.
--
-- 'clusterIdentifier', 'enableSnapshotCopy_clusterIdentifier' - The unique identifier of the source cluster to copy snapshots from.
--
-- Constraints: Must be the valid name of an existing cluster that does not
-- already have cross-region snapshot copy enabled.
--
-- 'destinationRegion', 'enableSnapshotCopy_destinationRegion' - The destination AWS Region that you want to copy snapshots to.
--
-- Constraints: Must be the name of a valid AWS Region. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html#redshift_region Regions and Endpoints>
-- in the Amazon Web Services General Reference.
newEnableSnapshotCopy ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  -- | 'destinationRegion'
  Prelude.Text ->
  EnableSnapshotCopy
newEnableSnapshotCopy
  pClusterIdentifier_
  pDestinationRegion_ =
    EnableSnapshotCopy'
      { snapshotCopyGrantName =
          Prelude.Nothing,
        manualSnapshotRetentionPeriod = Prelude.Nothing,
        retentionPeriod = Prelude.Nothing,
        clusterIdentifier = pClusterIdentifier_,
        destinationRegion = pDestinationRegion_
      }

-- | The name of the snapshot copy grant to use when snapshots of an AWS
-- KMS-encrypted cluster are copied to the destination region.
enableSnapshotCopy_snapshotCopyGrantName :: Lens.Lens' EnableSnapshotCopy (Prelude.Maybe Prelude.Text)
enableSnapshotCopy_snapshotCopyGrantName = Lens.lens (\EnableSnapshotCopy' {snapshotCopyGrantName} -> snapshotCopyGrantName) (\s@EnableSnapshotCopy' {} a -> s {snapshotCopyGrantName = a} :: EnableSnapshotCopy)

-- | The number of days to retain newly copied snapshots in the destination
-- AWS Region after they are copied from the source AWS Region. If the
-- value is -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
enableSnapshotCopy_manualSnapshotRetentionPeriod :: Lens.Lens' EnableSnapshotCopy (Prelude.Maybe Prelude.Int)
enableSnapshotCopy_manualSnapshotRetentionPeriod = Lens.lens (\EnableSnapshotCopy' {manualSnapshotRetentionPeriod} -> manualSnapshotRetentionPeriod) (\s@EnableSnapshotCopy' {} a -> s {manualSnapshotRetentionPeriod = a} :: EnableSnapshotCopy)

-- | The number of days to retain automated snapshots in the destination
-- region after they are copied from the source region.
--
-- Default: 7.
--
-- Constraints: Must be at least 1 and no more than 35.
enableSnapshotCopy_retentionPeriod :: Lens.Lens' EnableSnapshotCopy (Prelude.Maybe Prelude.Int)
enableSnapshotCopy_retentionPeriod = Lens.lens (\EnableSnapshotCopy' {retentionPeriod} -> retentionPeriod) (\s@EnableSnapshotCopy' {} a -> s {retentionPeriod = a} :: EnableSnapshotCopy)

-- | The unique identifier of the source cluster to copy snapshots from.
--
-- Constraints: Must be the valid name of an existing cluster that does not
-- already have cross-region snapshot copy enabled.
enableSnapshotCopy_clusterIdentifier :: Lens.Lens' EnableSnapshotCopy Prelude.Text
enableSnapshotCopy_clusterIdentifier = Lens.lens (\EnableSnapshotCopy' {clusterIdentifier} -> clusterIdentifier) (\s@EnableSnapshotCopy' {} a -> s {clusterIdentifier = a} :: EnableSnapshotCopy)

-- | The destination AWS Region that you want to copy snapshots to.
--
-- Constraints: Must be the name of a valid AWS Region. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html#redshift_region Regions and Endpoints>
-- in the Amazon Web Services General Reference.
enableSnapshotCopy_destinationRegion :: Lens.Lens' EnableSnapshotCopy Prelude.Text
enableSnapshotCopy_destinationRegion = Lens.lens (\EnableSnapshotCopy' {destinationRegion} -> destinationRegion) (\s@EnableSnapshotCopy' {} a -> s {destinationRegion = a} :: EnableSnapshotCopy)

instance Prelude.AWSRequest EnableSnapshotCopy where
  type
    Rs EnableSnapshotCopy =
      EnableSnapshotCopyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "EnableSnapshotCopyResult"
      ( \s h x ->
          EnableSnapshotCopyResponse'
            Prelude.<$> (x Prelude..@? "Cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EnableSnapshotCopy

instance Prelude.NFData EnableSnapshotCopy

instance Prelude.ToHeaders EnableSnapshotCopy where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath EnableSnapshotCopy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery EnableSnapshotCopy where
  toQuery EnableSnapshotCopy' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("EnableSnapshotCopy" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2012-12-01" :: Prelude.ByteString),
        "SnapshotCopyGrantName"
          Prelude.=: snapshotCopyGrantName,
        "ManualSnapshotRetentionPeriod"
          Prelude.=: manualSnapshotRetentionPeriod,
        "RetentionPeriod" Prelude.=: retentionPeriod,
        "ClusterIdentifier" Prelude.=: clusterIdentifier,
        "DestinationRegion" Prelude.=: destinationRegion
      ]

-- | /See:/ 'newEnableSnapshotCopyResponse' smart constructor.
data EnableSnapshotCopyResponse = EnableSnapshotCopyResponse'
  { cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnableSnapshotCopyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'enableSnapshotCopyResponse_cluster' - Undocumented member.
--
-- 'httpStatus', 'enableSnapshotCopyResponse_httpStatus' - The response's http status code.
newEnableSnapshotCopyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableSnapshotCopyResponse
newEnableSnapshotCopyResponse pHttpStatus_ =
  EnableSnapshotCopyResponse'
    { cluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
enableSnapshotCopyResponse_cluster :: Lens.Lens' EnableSnapshotCopyResponse (Prelude.Maybe Cluster)
enableSnapshotCopyResponse_cluster = Lens.lens (\EnableSnapshotCopyResponse' {cluster} -> cluster) (\s@EnableSnapshotCopyResponse' {} a -> s {cluster = a} :: EnableSnapshotCopyResponse)

-- | The response's http status code.
enableSnapshotCopyResponse_httpStatus :: Lens.Lens' EnableSnapshotCopyResponse Prelude.Int
enableSnapshotCopyResponse_httpStatus = Lens.lens (\EnableSnapshotCopyResponse' {httpStatus} -> httpStatus) (\s@EnableSnapshotCopyResponse' {} a -> s {httpStatus = a} :: EnableSnapshotCopyResponse)

instance Prelude.NFData EnableSnapshotCopyResponse
