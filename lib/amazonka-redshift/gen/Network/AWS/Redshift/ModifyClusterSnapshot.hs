{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifyClusterSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for a snapshot.
--
-- This exanmple modifies the manual retention period setting for a cluster snapshot.
module Network.AWS.Redshift.ModifyClusterSnapshot
  ( -- * Creating a request
    ModifyClusterSnapshot (..),
    mkModifyClusterSnapshot,

    -- ** Request lenses
    mcsManualSnapshotRetentionPeriod,
    mcsForce,
    mcsSnapshotIdentifier,

    -- * Destructuring the response
    ModifyClusterSnapshotResponse (..),
    mkModifyClusterSnapshotResponse,

    -- ** Response lenses
    mcsrsSnapshot,
    mcsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyClusterSnapshot' smart constructor.
data ModifyClusterSnapshot = ModifyClusterSnapshot'
  { manualSnapshotRetentionPeriod ::
      Lude.Maybe Lude.Int,
    force :: Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'ModifyClusterSnapshot' with the minimum fields required to make a request.
--
-- * 'force' - A Boolean option to override an exception if the retention period has already passed.
-- * 'manualSnapshotRetentionPeriod' - The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.
--
-- If the manual snapshot falls outside of the new retention period, you can specify the force option to immediately delete the snapshot.
-- The value must be either -1 or an integer between 1 and 3,653.
-- * 'snapshotIdentifier' - The identifier of the snapshot whose setting you want to modify.
mkModifyClusterSnapshot ::
  -- | 'snapshotIdentifier'
  Lude.Text ->
  ModifyClusterSnapshot
mkModifyClusterSnapshot pSnapshotIdentifier_ =
  ModifyClusterSnapshot'
    { manualSnapshotRetentionPeriod =
        Lude.Nothing,
      force = Lude.Nothing,
      snapshotIdentifier = pSnapshotIdentifier_
    }

-- | The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.
--
-- If the manual snapshot falls outside of the new retention period, you can specify the force option to immediately delete the snapshot.
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsManualSnapshotRetentionPeriod :: Lens.Lens' ModifyClusterSnapshot (Lude.Maybe Lude.Int)
mcsManualSnapshotRetentionPeriod = Lens.lens (manualSnapshotRetentionPeriod :: ModifyClusterSnapshot -> Lude.Maybe Lude.Int) (\s a -> s {manualSnapshotRetentionPeriod = a} :: ModifyClusterSnapshot)
{-# DEPRECATED mcsManualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead." #-}

-- | A Boolean option to override an exception if the retention period has already passed.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsForce :: Lens.Lens' ModifyClusterSnapshot (Lude.Maybe Lude.Bool)
mcsForce = Lens.lens (force :: ModifyClusterSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {force = a} :: ModifyClusterSnapshot)
{-# DEPRECATED mcsForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | The identifier of the snapshot whose setting you want to modify.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsSnapshotIdentifier :: Lens.Lens' ModifyClusterSnapshot Lude.Text
mcsSnapshotIdentifier = Lens.lens (snapshotIdentifier :: ModifyClusterSnapshot -> Lude.Text) (\s a -> s {snapshotIdentifier = a} :: ModifyClusterSnapshot)
{-# DEPRECATED mcsSnapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead." #-}

instance Lude.AWSRequest ModifyClusterSnapshot where
  type Rs ModifyClusterSnapshot = ModifyClusterSnapshotResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "ModifyClusterSnapshotResult"
      ( \s h x ->
          ModifyClusterSnapshotResponse'
            Lude.<$> (x Lude..@? "Snapshot") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyClusterSnapshot where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyClusterSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyClusterSnapshot where
  toQuery ModifyClusterSnapshot' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyClusterSnapshot" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ManualSnapshotRetentionPeriod"
          Lude.=: manualSnapshotRetentionPeriod,
        "Force" Lude.=: force,
        "SnapshotIdentifier" Lude.=: snapshotIdentifier
      ]

-- | /See:/ 'mkModifyClusterSnapshotResponse' smart constructor.
data ModifyClusterSnapshotResponse = ModifyClusterSnapshotResponse'
  { snapshot ::
      Lude.Maybe Snapshot,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyClusterSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'snapshot' - Undocumented field.
mkModifyClusterSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyClusterSnapshotResponse
mkModifyClusterSnapshotResponse pResponseStatus_ =
  ModifyClusterSnapshotResponse'
    { snapshot = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'snapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsrsSnapshot :: Lens.Lens' ModifyClusterSnapshotResponse (Lude.Maybe Snapshot)
mcsrsSnapshot = Lens.lens (snapshot :: ModifyClusterSnapshotResponse -> Lude.Maybe Snapshot) (\s a -> s {snapshot = a} :: ModifyClusterSnapshotResponse)
{-# DEPRECATED mcsrsSnapshot "Use generic-lens or generic-optics with 'snapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsrsResponseStatus :: Lens.Lens' ModifyClusterSnapshotResponse Lude.Int
mcsrsResponseStatus = Lens.lens (responseStatus :: ModifyClusterSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyClusterSnapshotResponse)
{-# DEPRECATED mcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
