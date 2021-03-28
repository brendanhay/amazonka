{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifySnapshotAttribute (..)
    , mkModifySnapshotAttribute
    -- ** Request lenses
    , msaSnapshotId
    , msaAttribute
    , msaCreateVolumePermission
    , msaDryRun
    , msaGroupNames
    , msaOperationType
    , msaUserIds

    -- * Destructuring the response
    , ModifySnapshotAttributeResponse (..)
    , mkModifySnapshotAttributeResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifySnapshotAttribute' smart constructor.
data ModifySnapshotAttribute = ModifySnapshotAttribute'
  { snapshotId :: Types.SnapshotId
    -- ^ The ID of the snapshot.
  , attribute :: Core.Maybe Types.SnapshotAttributeName
    -- ^ The snapshot attribute to modify. Only volume creation permissions can be modified.
  , createVolumePermission :: Core.Maybe Types.CreateVolumePermissionModifications
    -- ^ A JSON representation of the snapshot attribute modification.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , groupNames :: Core.Maybe [Types.SecurityGroupName]
    -- ^ The group to modify for the snapshot.
  , operationType :: Core.Maybe Types.OperationType
    -- ^ The type of operation to perform to the attribute.
  , userIds :: Core.Maybe [Core.Text]
    -- ^ The account ID to modify for the snapshot.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifySnapshotAttribute' value with any optional fields omitted.
mkModifySnapshotAttribute
    :: Types.SnapshotId -- ^ 'snapshotId'
    -> ModifySnapshotAttribute
mkModifySnapshotAttribute snapshotId
  = ModifySnapshotAttribute'{snapshotId, attribute = Core.Nothing,
                             createVolumePermission = Core.Nothing, dryRun = Core.Nothing,
                             groupNames = Core.Nothing, operationType = Core.Nothing,
                             userIds = Core.Nothing}

-- | The ID of the snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaSnapshotId :: Lens.Lens' ModifySnapshotAttribute Types.SnapshotId
msaSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE msaSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

-- | The snapshot attribute to modify. Only volume creation permissions can be modified.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaAttribute :: Lens.Lens' ModifySnapshotAttribute (Core.Maybe Types.SnapshotAttributeName)
msaAttribute = Lens.field @"attribute"
{-# INLINEABLE msaAttribute #-}
{-# DEPRECATED attribute "Use generic-lens or generic-optics with 'attribute' instead"  #-}

-- | A JSON representation of the snapshot attribute modification.
--
-- /Note:/ Consider using 'createVolumePermission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaCreateVolumePermission :: Lens.Lens' ModifySnapshotAttribute (Core.Maybe Types.CreateVolumePermissionModifications)
msaCreateVolumePermission = Lens.field @"createVolumePermission"
{-# INLINEABLE msaCreateVolumePermission #-}
{-# DEPRECATED createVolumePermission "Use generic-lens or generic-optics with 'createVolumePermission' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaDryRun :: Lens.Lens' ModifySnapshotAttribute (Core.Maybe Core.Bool)
msaDryRun = Lens.field @"dryRun"
{-# INLINEABLE msaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The group to modify for the snapshot.
--
-- /Note:/ Consider using 'groupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaGroupNames :: Lens.Lens' ModifySnapshotAttribute (Core.Maybe [Types.SecurityGroupName])
msaGroupNames = Lens.field @"groupNames"
{-# INLINEABLE msaGroupNames #-}
{-# DEPRECATED groupNames "Use generic-lens or generic-optics with 'groupNames' instead"  #-}

-- | The type of operation to perform to the attribute.
--
-- /Note:/ Consider using 'operationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaOperationType :: Lens.Lens' ModifySnapshotAttribute (Core.Maybe Types.OperationType)
msaOperationType = Lens.field @"operationType"
{-# INLINEABLE msaOperationType #-}
{-# DEPRECATED operationType "Use generic-lens or generic-optics with 'operationType' instead"  #-}

-- | The account ID to modify for the snapshot.
--
-- /Note:/ Consider using 'userIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaUserIds :: Lens.Lens' ModifySnapshotAttribute (Core.Maybe [Core.Text])
msaUserIds = Lens.field @"userIds"
{-# INLINEABLE msaUserIds #-}
{-# DEPRECATED userIds "Use generic-lens or generic-optics with 'userIds' instead"  #-}

instance Core.ToQuery ModifySnapshotAttribute where
        toQuery ModifySnapshotAttribute{..}
          = Core.toQueryPair "Action"
              ("ModifySnapshotAttribute" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "SnapshotId" snapshotId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Attribute") attribute
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CreateVolumePermission")
                createVolumePermission
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "UserGroup") groupNames
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "OperationType")
                operationType
              Core.<> Core.maybe Core.mempty (Core.toQueryList "UserId") userIds

instance Core.ToHeaders ModifySnapshotAttribute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifySnapshotAttribute where
        type Rs ModifySnapshotAttribute = ModifySnapshotAttributeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull ModifySnapshotAttributeResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifySnapshotAttributeResponse' smart constructor.
data ModifySnapshotAttributeResponse = ModifySnapshotAttributeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifySnapshotAttributeResponse' value with any optional fields omitted.
mkModifySnapshotAttributeResponse
    :: ModifySnapshotAttributeResponse
mkModifySnapshotAttributeResponse
  = ModifySnapshotAttributeResponse'
