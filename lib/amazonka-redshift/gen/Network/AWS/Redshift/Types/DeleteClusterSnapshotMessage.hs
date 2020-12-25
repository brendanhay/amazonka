{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.DeleteClusterSnapshotMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.DeleteClusterSnapshotMessage
  ( DeleteClusterSnapshotMessage (..),

    -- * Smart constructor
    mkDeleteClusterSnapshotMessage,

    -- * Lenses
    dcsmSnapshotIdentifier,
    dcsmSnapshotClusterIdentifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.SnapshotClusterIdentifier as Types
import qualified Network.AWS.Redshift.Types.SnapshotIdentifier as Types

-- |
--
-- /See:/ 'mkDeleteClusterSnapshotMessage' smart constructor.
data DeleteClusterSnapshotMessage = DeleteClusterSnapshotMessage'
  { -- | The unique identifier of the manual snapshot to be deleted.
    --
    -- Constraints: Must be the name of an existing snapshot that is in the @available@ , @failed@ , or @cancelled@ state.
    snapshotIdentifier :: Types.SnapshotIdentifier,
    -- | The unique identifier of the cluster the snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
    --
    -- Constraints: Must be the name of valid cluster.
    snapshotClusterIdentifier :: Core.Maybe Types.SnapshotClusterIdentifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteClusterSnapshotMessage' value with any optional fields omitted.
mkDeleteClusterSnapshotMessage ::
  -- | 'snapshotIdentifier'
  Types.SnapshotIdentifier ->
  DeleteClusterSnapshotMessage
mkDeleteClusterSnapshotMessage snapshotIdentifier =
  DeleteClusterSnapshotMessage'
    { snapshotIdentifier,
      snapshotClusterIdentifier = Core.Nothing
    }

-- | The unique identifier of the manual snapshot to be deleted.
--
-- Constraints: Must be the name of an existing snapshot that is in the @available@ , @failed@ , or @cancelled@ state.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsmSnapshotIdentifier :: Lens.Lens' DeleteClusterSnapshotMessage Types.SnapshotIdentifier
dcsmSnapshotIdentifier = Lens.field @"snapshotIdentifier"
{-# DEPRECATED dcsmSnapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead." #-}

-- | The unique identifier of the cluster the snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
--
-- Constraints: Must be the name of valid cluster.
--
-- /Note:/ Consider using 'snapshotClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsmSnapshotClusterIdentifier :: Lens.Lens' DeleteClusterSnapshotMessage (Core.Maybe Types.SnapshotClusterIdentifier)
dcsmSnapshotClusterIdentifier = Lens.field @"snapshotClusterIdentifier"
{-# DEPRECATED dcsmSnapshotClusterIdentifier "Use generic-lens or generic-optics with 'snapshotClusterIdentifier' instead." #-}
