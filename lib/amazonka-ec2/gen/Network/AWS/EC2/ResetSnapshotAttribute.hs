{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ResetSnapshotAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets permission settings for the specified snapshot.
--
-- For more information about modifying snapshot permissions, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-modifying-snapshot-permissions.html Sharing snapshots> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.ResetSnapshotAttribute
    (
    -- * Creating a request
      ResetSnapshotAttribute (..)
    , mkResetSnapshotAttribute
    -- ** Request lenses
    , rsaAttribute
    , rsaSnapshotId
    , rsaDryRun

    -- * Destructuring the response
    , ResetSnapshotAttributeResponse (..)
    , mkResetSnapshotAttributeResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkResetSnapshotAttribute' smart constructor.
data ResetSnapshotAttribute = ResetSnapshotAttribute'
  { attribute :: Types.SnapshotAttributeName
    -- ^ The attribute to reset. Currently, only the attribute for permission to create volumes can be reset.
  , snapshotId :: Types.SnapshotId
    -- ^ The ID of the snapshot.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetSnapshotAttribute' value with any optional fields omitted.
mkResetSnapshotAttribute
    :: Types.SnapshotAttributeName -- ^ 'attribute'
    -> Types.SnapshotId -- ^ 'snapshotId'
    -> ResetSnapshotAttribute
mkResetSnapshotAttribute attribute snapshotId
  = ResetSnapshotAttribute'{attribute, snapshotId,
                            dryRun = Core.Nothing}

-- | The attribute to reset. Currently, only the attribute for permission to create volumes can be reset.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsaAttribute :: Lens.Lens' ResetSnapshotAttribute Types.SnapshotAttributeName
rsaAttribute = Lens.field @"attribute"
{-# INLINEABLE rsaAttribute #-}
{-# DEPRECATED attribute "Use generic-lens or generic-optics with 'attribute' instead"  #-}

-- | The ID of the snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsaSnapshotId :: Lens.Lens' ResetSnapshotAttribute Types.SnapshotId
rsaSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE rsaSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsaDryRun :: Lens.Lens' ResetSnapshotAttribute (Core.Maybe Core.Bool)
rsaDryRun = Lens.field @"dryRun"
{-# INLINEABLE rsaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery ResetSnapshotAttribute where
        toQuery ResetSnapshotAttribute{..}
          = Core.toQueryPair "Action" ("ResetSnapshotAttribute" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "Attribute" attribute
              Core.<> Core.toQueryPair "SnapshotId" snapshotId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders ResetSnapshotAttribute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ResetSnapshotAttribute where
        type Rs ResetSnapshotAttribute = ResetSnapshotAttributeResponse
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
          = Response.receiveNull ResetSnapshotAttributeResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkResetSnapshotAttributeResponse' smart constructor.
data ResetSnapshotAttributeResponse = ResetSnapshotAttributeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetSnapshotAttributeResponse' value with any optional fields omitted.
mkResetSnapshotAttributeResponse
    :: ResetSnapshotAttributeResponse
mkResetSnapshotAttributeResponse = ResetSnapshotAttributeResponse'
