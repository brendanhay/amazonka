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
-- Module      : Network.AWS.EC2.CreateSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of an EBS volume and stores it in Amazon S3. You can
-- use snapshots for backups, to make copies of EBS volumes, and to save
-- data before shutting down an instance.
--
-- You can create snapshots of volumes in a Region and volumes on an
-- Outpost. If you create a snapshot of a volume in a Region, the snapshot
-- must be stored in the same Region as the volume. If you create a
-- snapshot of a volume on an Outpost, the snapshot can be stored on the
-- same Outpost as the volume, or in the Region for that Outpost.
--
-- When a snapshot is created, any AWS Marketplace product codes that are
-- associated with the source volume are propagated to the snapshot.
--
-- You can take a snapshot of an attached volume that is in use. However,
-- snapshots only capture data that has been written to your EBS volume at
-- the time the snapshot command is issued; this might exclude any data
-- that has been cached by any applications or the operating system. If you
-- can pause any file systems on the volume long enough to take a snapshot,
-- your snapshot should be complete. However, if you cannot pause all file
-- writes to the volume, you should unmount the volume from within the
-- instance, issue the snapshot command, and then remount the volume to
-- ensure a consistent and complete snapshot. You may remount and use your
-- volume while the snapshot status is @pending@.
--
-- To create a snapshot for EBS volumes that serve as root devices, you
-- should stop the instance before taking the snapshot.
--
-- Snapshots that are taken from encrypted volumes are automatically
-- encrypted. Volumes that are created from encrypted snapshots are also
-- automatically encrypted. Your encrypted volumes and any associated
-- snapshots always remain protected.
--
-- You can tag your snapshots during creation. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging your Amazon EC2 resources>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AmazonEBS.html Amazon Elastic Block Store>
-- and
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.CreateSnapshot
  ( -- * Creating a Request
    CreateSnapshot (..),
    newCreateSnapshot,

    -- * Request Lenses
    createSnapshot_tagSpecifications,
    createSnapshot_dryRun,
    createSnapshot_outpostArn,
    createSnapshot_description,
    createSnapshot_volumeId,

    -- * Destructuring the Response
    Snapshot (..),
    newSnapshot,

    -- * Response Lenses
    snapshot_ownerAlias,
    snapshot_stateMessage,
    snapshot_outpostArn,
    snapshot_dataEncryptionKeyId,
    snapshot_kmsKeyId,
    snapshot_tags,
    snapshot_snapshotId,
    snapshot_ownerId,
    snapshot_volumeId,
    snapshot_volumeSize,
    snapshot_description,
    snapshot_startTime,
    snapshot_progress,
    snapshot_state,
    snapshot_encrypted,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSnapshot' smart constructor.
data CreateSnapshot = CreateSnapshot'
  { -- | The tags to apply to the snapshot during creation.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the AWS Outpost on which to create a
    -- local snapshot.
    --
    -- -   To create a snapshot of a volume in a Region, omit this parameter.
    --     The snapshot is created in the same Region as the volume.
    --
    -- -   To create a snapshot of a volume on an Outpost and store the
    --     snapshot in the Region, omit this parameter. The snapshot is created
    --     in the Region for the Outpost.
    --
    -- -   To create a snapshot of a volume on an Outpost and store the
    --     snapshot on an Outpost, specify the ARN of the destination Outpost.
    --     The snapshot must be created on the same Outpost as the volume.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html#create-snapshot Creating local snapshots from volumes on an Outpost>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | A description for the snapshot.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the EBS volume.
    volumeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'createSnapshot_tagSpecifications' - The tags to apply to the snapshot during creation.
--
-- 'dryRun', 'createSnapshot_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'outpostArn', 'createSnapshot_outpostArn' - The Amazon Resource Name (ARN) of the AWS Outpost on which to create a
-- local snapshot.
--
-- -   To create a snapshot of a volume in a Region, omit this parameter.
--     The snapshot is created in the same Region as the volume.
--
-- -   To create a snapshot of a volume on an Outpost and store the
--     snapshot in the Region, omit this parameter. The snapshot is created
--     in the Region for the Outpost.
--
-- -   To create a snapshot of a volume on an Outpost and store the
--     snapshot on an Outpost, specify the ARN of the destination Outpost.
--     The snapshot must be created on the same Outpost as the volume.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html#create-snapshot Creating local snapshots from volumes on an Outpost>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'description', 'createSnapshot_description' - A description for the snapshot.
--
-- 'volumeId', 'createSnapshot_volumeId' - The ID of the EBS volume.
newCreateSnapshot ::
  -- | 'volumeId'
  Prelude.Text ->
  CreateSnapshot
newCreateSnapshot pVolumeId_ =
  CreateSnapshot'
    { tagSpecifications =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      description = Prelude.Nothing,
      volumeId = pVolumeId_
    }

-- | The tags to apply to the snapshot during creation.
createSnapshot_tagSpecifications :: Lens.Lens' CreateSnapshot (Prelude.Maybe [TagSpecification])
createSnapshot_tagSpecifications = Lens.lens (\CreateSnapshot' {tagSpecifications} -> tagSpecifications) (\s@CreateSnapshot' {} a -> s {tagSpecifications = a} :: CreateSnapshot) Prelude.. Lens.mapping Prelude._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createSnapshot_dryRun :: Lens.Lens' CreateSnapshot (Prelude.Maybe Prelude.Bool)
createSnapshot_dryRun = Lens.lens (\CreateSnapshot' {dryRun} -> dryRun) (\s@CreateSnapshot' {} a -> s {dryRun = a} :: CreateSnapshot)

-- | The Amazon Resource Name (ARN) of the AWS Outpost on which to create a
-- local snapshot.
--
-- -   To create a snapshot of a volume in a Region, omit this parameter.
--     The snapshot is created in the same Region as the volume.
--
-- -   To create a snapshot of a volume on an Outpost and store the
--     snapshot in the Region, omit this parameter. The snapshot is created
--     in the Region for the Outpost.
--
-- -   To create a snapshot of a volume on an Outpost and store the
--     snapshot on an Outpost, specify the ARN of the destination Outpost.
--     The snapshot must be created on the same Outpost as the volume.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html#create-snapshot Creating local snapshots from volumes on an Outpost>
-- in the /Amazon Elastic Compute Cloud User Guide/.
createSnapshot_outpostArn :: Lens.Lens' CreateSnapshot (Prelude.Maybe Prelude.Text)
createSnapshot_outpostArn = Lens.lens (\CreateSnapshot' {outpostArn} -> outpostArn) (\s@CreateSnapshot' {} a -> s {outpostArn = a} :: CreateSnapshot)

-- | A description for the snapshot.
createSnapshot_description :: Lens.Lens' CreateSnapshot (Prelude.Maybe Prelude.Text)
createSnapshot_description = Lens.lens (\CreateSnapshot' {description} -> description) (\s@CreateSnapshot' {} a -> s {description = a} :: CreateSnapshot)

-- | The ID of the EBS volume.
createSnapshot_volumeId :: Lens.Lens' CreateSnapshot Prelude.Text
createSnapshot_volumeId = Lens.lens (\CreateSnapshot' {volumeId} -> volumeId) (\s@CreateSnapshot' {} a -> s {volumeId = a} :: CreateSnapshot)

instance Prelude.AWSRequest CreateSnapshot where
  type Rs CreateSnapshot = Snapshot
  request = Request.postQuery defaultService
  response =
    Response.receiveXML (\s h x -> Prelude.parseXML x)

instance Prelude.Hashable CreateSnapshot

instance Prelude.NFData CreateSnapshot

instance Prelude.ToHeaders CreateSnapshot where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CreateSnapshot where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateSnapshot where
  toQuery CreateSnapshot' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CreateSnapshot" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        Prelude.toQuery
          ( Prelude.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "DryRun" Prelude.=: dryRun,
        "OutpostArn" Prelude.=: outpostArn,
        "Description" Prelude.=: description,
        "VolumeId" Prelude.=: volumeId
      ]
