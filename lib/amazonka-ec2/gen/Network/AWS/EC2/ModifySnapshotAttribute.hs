{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifySnapshotAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or removes permission settings for the specified snapshot. You may add or remove specified AWS account IDs from a snapshot's list of create volume permissions, but you cannot do both in a single operation. If you need to both add and remove account IDs for a snapshot, you must use multiple operations. You can make up to 500 modifications to a snapshot in a single operation.
--
-- Encrypted snapshots and snapshots with AWS Marketplace product codes cannot be made public. Snapshots encrypted with your default CMK cannot be shared with other accounts.
-- For more information about modifying snapshot permissions, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-modifying-snapshot-permissions.html Sharing snapshots> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.ModifySnapshotAttribute
  ( -- * Creating a request
    ModifySnapshotAttribute (..),
    mkModifySnapshotAttribute,

    -- ** Request lenses
    msaAttribute,
    msaCreateVolumePermission,
    msaUserIds,
    msaGroupNames,
    msaOperationType,
    msaDryRun,
    msaSnapshotId,

    -- * Destructuring the response
    ModifySnapshotAttributeResponse (..),
    mkModifySnapshotAttributeResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifySnapshotAttribute' smart constructor.
data ModifySnapshotAttribute = ModifySnapshotAttribute'
  { -- | The snapshot attribute to modify. Only volume creation permissions can be modified.
    attribute :: Lude.Maybe SnapshotAttributeName,
    -- | A JSON representation of the snapshot attribute modification.
    createVolumePermission :: Lude.Maybe CreateVolumePermissionModifications,
    -- | The account ID to modify for the snapshot.
    userIds :: Lude.Maybe [Lude.Text],
    -- | The group to modify for the snapshot.
    groupNames :: Lude.Maybe [Lude.Text],
    -- | The type of operation to perform to the attribute.
    operationType :: Lude.Maybe OperationType,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The ID of the snapshot.
    snapshotId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifySnapshotAttribute' with the minimum fields required to make a request.
--
-- * 'attribute' - The snapshot attribute to modify. Only volume creation permissions can be modified.
-- * 'createVolumePermission' - A JSON representation of the snapshot attribute modification.
-- * 'userIds' - The account ID to modify for the snapshot.
-- * 'groupNames' - The group to modify for the snapshot.
-- * 'operationType' - The type of operation to perform to the attribute.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'snapshotId' - The ID of the snapshot.
mkModifySnapshotAttribute ::
  -- | 'snapshotId'
  Lude.Text ->
  ModifySnapshotAttribute
mkModifySnapshotAttribute pSnapshotId_ =
  ModifySnapshotAttribute'
    { attribute = Lude.Nothing,
      createVolumePermission = Lude.Nothing,
      userIds = Lude.Nothing,
      groupNames = Lude.Nothing,
      operationType = Lude.Nothing,
      dryRun = Lude.Nothing,
      snapshotId = pSnapshotId_
    }

-- | The snapshot attribute to modify. Only volume creation permissions can be modified.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaAttribute :: Lens.Lens' ModifySnapshotAttribute (Lude.Maybe SnapshotAttributeName)
msaAttribute = Lens.lens (attribute :: ModifySnapshotAttribute -> Lude.Maybe SnapshotAttributeName) (\s a -> s {attribute = a} :: ModifySnapshotAttribute)
{-# DEPRECATED msaAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | A JSON representation of the snapshot attribute modification.
--
-- /Note:/ Consider using 'createVolumePermission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaCreateVolumePermission :: Lens.Lens' ModifySnapshotAttribute (Lude.Maybe CreateVolumePermissionModifications)
msaCreateVolumePermission = Lens.lens (createVolumePermission :: ModifySnapshotAttribute -> Lude.Maybe CreateVolumePermissionModifications) (\s a -> s {createVolumePermission = a} :: ModifySnapshotAttribute)
{-# DEPRECATED msaCreateVolumePermission "Use generic-lens or generic-optics with 'createVolumePermission' instead." #-}

-- | The account ID to modify for the snapshot.
--
-- /Note:/ Consider using 'userIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaUserIds :: Lens.Lens' ModifySnapshotAttribute (Lude.Maybe [Lude.Text])
msaUserIds = Lens.lens (userIds :: ModifySnapshotAttribute -> Lude.Maybe [Lude.Text]) (\s a -> s {userIds = a} :: ModifySnapshotAttribute)
{-# DEPRECATED msaUserIds "Use generic-lens or generic-optics with 'userIds' instead." #-}

-- | The group to modify for the snapshot.
--
-- /Note:/ Consider using 'groupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaGroupNames :: Lens.Lens' ModifySnapshotAttribute (Lude.Maybe [Lude.Text])
msaGroupNames = Lens.lens (groupNames :: ModifySnapshotAttribute -> Lude.Maybe [Lude.Text]) (\s a -> s {groupNames = a} :: ModifySnapshotAttribute)
{-# DEPRECATED msaGroupNames "Use generic-lens or generic-optics with 'groupNames' instead." #-}

-- | The type of operation to perform to the attribute.
--
-- /Note:/ Consider using 'operationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaOperationType :: Lens.Lens' ModifySnapshotAttribute (Lude.Maybe OperationType)
msaOperationType = Lens.lens (operationType :: ModifySnapshotAttribute -> Lude.Maybe OperationType) (\s a -> s {operationType = a} :: ModifySnapshotAttribute)
{-# DEPRECATED msaOperationType "Use generic-lens or generic-optics with 'operationType' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaDryRun :: Lens.Lens' ModifySnapshotAttribute (Lude.Maybe Lude.Bool)
msaDryRun = Lens.lens (dryRun :: ModifySnapshotAttribute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifySnapshotAttribute)
{-# DEPRECATED msaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaSnapshotId :: Lens.Lens' ModifySnapshotAttribute Lude.Text
msaSnapshotId = Lens.lens (snapshotId :: ModifySnapshotAttribute -> Lude.Text) (\s a -> s {snapshotId = a} :: ModifySnapshotAttribute)
{-# DEPRECATED msaSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

instance Lude.AWSRequest ModifySnapshotAttribute where
  type Rs ModifySnapshotAttribute = ModifySnapshotAttributeResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull ModifySnapshotAttributeResponse'

instance Lude.ToHeaders ModifySnapshotAttribute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifySnapshotAttribute where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifySnapshotAttribute where
  toQuery ModifySnapshotAttribute' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifySnapshotAttribute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Attribute" Lude.=: attribute,
        "CreateVolumePermission" Lude.=: createVolumePermission,
        Lude.toQuery (Lude.toQueryList "UserId" Lude.<$> userIds),
        Lude.toQuery (Lude.toQueryList "UserGroup" Lude.<$> groupNames),
        "OperationType" Lude.=: operationType,
        "DryRun" Lude.=: dryRun,
        "SnapshotId" Lude.=: snapshotId
      ]

-- | /See:/ 'mkModifySnapshotAttributeResponse' smart constructor.
data ModifySnapshotAttributeResponse = ModifySnapshotAttributeResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifySnapshotAttributeResponse' with the minimum fields required to make a request.
mkModifySnapshotAttributeResponse ::
  ModifySnapshotAttributeResponse
mkModifySnapshotAttributeResponse =
  ModifySnapshotAttributeResponse'
