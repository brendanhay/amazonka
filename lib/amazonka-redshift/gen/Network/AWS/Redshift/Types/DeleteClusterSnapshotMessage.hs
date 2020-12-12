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
    dcsmSnapshotClusterIdentifier,
    dcsmSnapshotIdentifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- |
--
-- /See:/ 'mkDeleteClusterSnapshotMessage' smart constructor.
data DeleteClusterSnapshotMessage = DeleteClusterSnapshotMessage'
  { snapshotClusterIdentifier ::
      Lude.Maybe Lude.Text,
    snapshotIdentifier :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteClusterSnapshotMessage' with the minimum fields required to make a request.
--
-- * 'snapshotClusterIdentifier' - The unique identifier of the cluster the snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
--
-- Constraints: Must be the name of valid cluster.
-- * 'snapshotIdentifier' - The unique identifier of the manual snapshot to be deleted.
--
-- Constraints: Must be the name of an existing snapshot that is in the @available@ , @failed@ , or @cancelled@ state.
mkDeleteClusterSnapshotMessage ::
  -- | 'snapshotIdentifier'
  Lude.Text ->
  DeleteClusterSnapshotMessage
mkDeleteClusterSnapshotMessage pSnapshotIdentifier_ =
  DeleteClusterSnapshotMessage'
    { snapshotClusterIdentifier =
        Lude.Nothing,
      snapshotIdentifier = pSnapshotIdentifier_
    }

-- | The unique identifier of the cluster the snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
--
-- Constraints: Must be the name of valid cluster.
--
-- /Note:/ Consider using 'snapshotClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsmSnapshotClusterIdentifier :: Lens.Lens' DeleteClusterSnapshotMessage (Lude.Maybe Lude.Text)
dcsmSnapshotClusterIdentifier = Lens.lens (snapshotClusterIdentifier :: DeleteClusterSnapshotMessage -> Lude.Maybe Lude.Text) (\s a -> s {snapshotClusterIdentifier = a} :: DeleteClusterSnapshotMessage)
{-# DEPRECATED dcsmSnapshotClusterIdentifier "Use generic-lens or generic-optics with 'snapshotClusterIdentifier' instead." #-}

-- | The unique identifier of the manual snapshot to be deleted.
--
-- Constraints: Must be the name of an existing snapshot that is in the @available@ , @failed@ , or @cancelled@ state.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsmSnapshotIdentifier :: Lens.Lens' DeleteClusterSnapshotMessage Lude.Text
dcsmSnapshotIdentifier = Lens.lens (snapshotIdentifier :: DeleteClusterSnapshotMessage -> Lude.Text) (\s a -> s {snapshotIdentifier = a} :: DeleteClusterSnapshotMessage)
{-# DEPRECATED dcsmSnapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead." #-}

instance Lude.ToQuery DeleteClusterSnapshotMessage where
  toQuery DeleteClusterSnapshotMessage' {..} =
    Lude.mconcat
      [ "SnapshotClusterIdentifier" Lude.=: snapshotClusterIdentifier,
        "SnapshotIdentifier" Lude.=: snapshotIdentifier
      ]
