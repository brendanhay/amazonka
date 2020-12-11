{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of an EBS volume and stores it in Amazon S3. You can use snapshots for backups, to make copies of EBS volumes, and to save data before shutting down an instance.
--
-- When a snapshot is created, any AWS Marketplace product codes that are associated with the source volume are propagated to the snapshot.
-- You can take a snapshot of an attached volume that is in use. However, snapshots only capture data that has been written to your EBS volume at the time the snapshot command is issued; this may exclude any data that has been cached by any applications or the operating system. If you can pause any file systems on the volume long enough to take a snapshot, your snapshot should be complete. However, if you cannot pause all file writes to the volume, you should unmount the volume from within the instance, issue the snapshot command, and then remount the volume to ensure a consistent and complete snapshot. You may remount and use your volume while the snapshot status is @pending@ .
-- To create a snapshot for EBS volumes that serve as root devices, you should stop the instance before taking the snapshot.
-- Snapshots that are taken from encrypted volumes are automatically encrypted. Volumes that are created from encrypted snapshots are also automatically encrypted. Your encrypted volumes and any associated snapshots always remain protected.
-- You can tag your snapshots during creation. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging your Amazon EC2 resources> in the /Amazon Elastic Compute Cloud User Guide/ .
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AmazonEBS.html Amazon Elastic Block Store> and <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.CreateSnapshot
  ( -- * Creating a request
    CreateSnapshot (..),
    mkCreateSnapshot,

    -- ** Request lenses
    ccTagSpecifications,
    ccDescription,
    ccDryRun,
    ccVolumeId,

    -- * Destructuring the response
    Snapshot (..),
    mkSnapshot,

    -- ** Response lenses
    sStateMessage,
    sOwnerAlias,
    sDataEncryptionKeyId,
    sKMSKeyId,
    sTags,
    sSnapshotId,
    sOwnerId,
    sVolumeId,
    sVolumeSize,
    sDescription,
    sStartTime,
    sProgress,
    sState,
    sEncrypted,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateSnapshot' smart constructor.
data CreateSnapshot = CreateSnapshot'
  { tagSpecifications ::
      Lude.Maybe [TagSpecification],
    description :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    volumeId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSnapshot' with the minimum fields required to make a request.
--
-- * 'description' - A description for the snapshot.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'tagSpecifications' - The tags to apply to the snapshot during creation.
-- * 'volumeId' - The ID of the EBS volume.
mkCreateSnapshot ::
  -- | 'volumeId'
  Lude.Text ->
  CreateSnapshot
mkCreateSnapshot pVolumeId_ =
  CreateSnapshot'
    { tagSpecifications = Lude.Nothing,
      description = Lude.Nothing,
      dryRun = Lude.Nothing,
      volumeId = pVolumeId_
    }

-- | The tags to apply to the snapshot during creation.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTagSpecifications :: Lens.Lens' CreateSnapshot (Lude.Maybe [TagSpecification])
ccTagSpecifications = Lens.lens (tagSpecifications :: CreateSnapshot -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateSnapshot)
{-# DEPRECATED ccTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | A description for the snapshot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDescription :: Lens.Lens' CreateSnapshot (Lude.Maybe Lude.Text)
ccDescription = Lens.lens (description :: CreateSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateSnapshot)
{-# DEPRECATED ccDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDryRun :: Lens.Lens' CreateSnapshot (Lude.Maybe Lude.Bool)
ccDryRun = Lens.lens (dryRun :: CreateSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateSnapshot)
{-# DEPRECATED ccDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the EBS volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccVolumeId :: Lens.Lens' CreateSnapshot Lude.Text
ccVolumeId = Lens.lens (volumeId :: CreateSnapshot -> Lude.Text) (\s a -> s {volumeId = a} :: CreateSnapshot)
{-# DEPRECATED ccVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

instance Lude.AWSRequest CreateSnapshot where
  type Rs CreateSnapshot = Snapshot
  request = Req.postQuery ec2Service
  response = Res.receiveXML (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders CreateSnapshot where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateSnapshot where
  toQuery CreateSnapshot' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateSnapshot" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun,
        "VolumeId" Lude.=: volumeId
      ]
