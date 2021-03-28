{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.DeleteClusterSnapshotMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.DeleteClusterSnapshotMessage
  ( DeleteClusterSnapshotMessage (..)
  -- * Smart constructor
  , mkDeleteClusterSnapshotMessage
  -- * Lenses
  , dcsmSnapshotIdentifier
  , dcsmSnapshotClusterIdentifier
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types

-- | 
--
-- /See:/ 'mkDeleteClusterSnapshotMessage' smart constructor.
data DeleteClusterSnapshotMessage = DeleteClusterSnapshotMessage'
  { snapshotIdentifier :: Core.Text
    -- ^ The unique identifier of the manual snapshot to be deleted.
--
-- Constraints: Must be the name of an existing snapshot that is in the @available@ , @failed@ , or @cancelled@ state.
  , snapshotClusterIdentifier :: Core.Maybe Core.Text
    -- ^ The unique identifier of the cluster the snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
--
-- Constraints: Must be the name of valid cluster.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteClusterSnapshotMessage' value with any optional fields omitted.
mkDeleteClusterSnapshotMessage
    :: Core.Text -- ^ 'snapshotIdentifier'
    -> DeleteClusterSnapshotMessage
mkDeleteClusterSnapshotMessage snapshotIdentifier
  = DeleteClusterSnapshotMessage'{snapshotIdentifier,
                                  snapshotClusterIdentifier = Core.Nothing}

-- | The unique identifier of the manual snapshot to be deleted.
--
-- Constraints: Must be the name of an existing snapshot that is in the @available@ , @failed@ , or @cancelled@ state.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsmSnapshotIdentifier :: Lens.Lens' DeleteClusterSnapshotMessage Core.Text
dcsmSnapshotIdentifier = Lens.field @"snapshotIdentifier"
{-# INLINEABLE dcsmSnapshotIdentifier #-}
{-# DEPRECATED snapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead"  #-}

-- | The unique identifier of the cluster the snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
--
-- Constraints: Must be the name of valid cluster.
--
-- /Note:/ Consider using 'snapshotClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsmSnapshotClusterIdentifier :: Lens.Lens' DeleteClusterSnapshotMessage (Core.Maybe Core.Text)
dcsmSnapshotClusterIdentifier = Lens.field @"snapshotClusterIdentifier"
{-# INLINEABLE dcsmSnapshotClusterIdentifier #-}
{-# DEPRECATED snapshotClusterIdentifier "Use generic-lens or generic-optics with 'snapshotClusterIdentifier' instead"  #-}

instance Core.ToQuery DeleteClusterSnapshotMessage where
        toQuery DeleteClusterSnapshotMessage{..}
          = Core.toQueryPair "SnapshotIdentifier" snapshotIdentifier Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "SnapshotClusterIdentifier")
                snapshotClusterIdentifier
