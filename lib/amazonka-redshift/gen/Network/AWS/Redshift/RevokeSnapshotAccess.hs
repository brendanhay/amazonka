{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.RevokeSnapshotAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the ability of the specified AWS customer account to restore the specified snapshot. If the account is currently restoring the snapshot, the restore will run to completion.
--
-- For more information about working with snapshots, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html Amazon Redshift Snapshots> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.RevokeSnapshotAccess
  ( -- * Creating a request
    RevokeSnapshotAccess (..),
    mkRevokeSnapshotAccess,

    -- ** Request lenses
    rsaSnapshotClusterIdentifier,
    rsaSnapshotIdentifier,
    rsaAccountWithRestoreAccess,

    -- * Destructuring the response
    RevokeSnapshotAccessResponse (..),
    mkRevokeSnapshotAccessResponse,

    -- ** Response lenses
    rsarsSnapshot,
    rsarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkRevokeSnapshotAccess' smart constructor.
data RevokeSnapshotAccess = RevokeSnapshotAccess'
  { snapshotClusterIdentifier ::
      Lude.Maybe Lude.Text,
    snapshotIdentifier :: Lude.Text,
    accountWithRestoreAccess :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RevokeSnapshotAccess' with the minimum fields required to make a request.
--
-- * 'accountWithRestoreAccess' - The identifier of the AWS customer account that can no longer restore the specified snapshot.
-- * 'snapshotClusterIdentifier' - The identifier of the cluster the snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
-- * 'snapshotIdentifier' - The identifier of the snapshot that the account can no longer access.
mkRevokeSnapshotAccess ::
  -- | 'snapshotIdentifier'
  Lude.Text ->
  -- | 'accountWithRestoreAccess'
  Lude.Text ->
  RevokeSnapshotAccess
mkRevokeSnapshotAccess
  pSnapshotIdentifier_
  pAccountWithRestoreAccess_ =
    RevokeSnapshotAccess'
      { snapshotClusterIdentifier = Lude.Nothing,
        snapshotIdentifier = pSnapshotIdentifier_,
        accountWithRestoreAccess = pAccountWithRestoreAccess_
      }

-- | The identifier of the cluster the snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
--
-- /Note:/ Consider using 'snapshotClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsaSnapshotClusterIdentifier :: Lens.Lens' RevokeSnapshotAccess (Lude.Maybe Lude.Text)
rsaSnapshotClusterIdentifier = Lens.lens (snapshotClusterIdentifier :: RevokeSnapshotAccess -> Lude.Maybe Lude.Text) (\s a -> s {snapshotClusterIdentifier = a} :: RevokeSnapshotAccess)
{-# DEPRECATED rsaSnapshotClusterIdentifier "Use generic-lens or generic-optics with 'snapshotClusterIdentifier' instead." #-}

-- | The identifier of the snapshot that the account can no longer access.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsaSnapshotIdentifier :: Lens.Lens' RevokeSnapshotAccess Lude.Text
rsaSnapshotIdentifier = Lens.lens (snapshotIdentifier :: RevokeSnapshotAccess -> Lude.Text) (\s a -> s {snapshotIdentifier = a} :: RevokeSnapshotAccess)
{-# DEPRECATED rsaSnapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead." #-}

-- | The identifier of the AWS customer account that can no longer restore the specified snapshot.
--
-- /Note:/ Consider using 'accountWithRestoreAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsaAccountWithRestoreAccess :: Lens.Lens' RevokeSnapshotAccess Lude.Text
rsaAccountWithRestoreAccess = Lens.lens (accountWithRestoreAccess :: RevokeSnapshotAccess -> Lude.Text) (\s a -> s {accountWithRestoreAccess = a} :: RevokeSnapshotAccess)
{-# DEPRECATED rsaAccountWithRestoreAccess "Use generic-lens or generic-optics with 'accountWithRestoreAccess' instead." #-}

instance Lude.AWSRequest RevokeSnapshotAccess where
  type Rs RevokeSnapshotAccess = RevokeSnapshotAccessResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "RevokeSnapshotAccessResult"
      ( \s h x ->
          RevokeSnapshotAccessResponse'
            Lude.<$> (x Lude..@? "Snapshot") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RevokeSnapshotAccess where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RevokeSnapshotAccess where
  toPath = Lude.const "/"

instance Lude.ToQuery RevokeSnapshotAccess where
  toQuery RevokeSnapshotAccess' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RevokeSnapshotAccess" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "SnapshotClusterIdentifier" Lude.=: snapshotClusterIdentifier,
        "SnapshotIdentifier" Lude.=: snapshotIdentifier,
        "AccountWithRestoreAccess" Lude.=: accountWithRestoreAccess
      ]

-- | /See:/ 'mkRevokeSnapshotAccessResponse' smart constructor.
data RevokeSnapshotAccessResponse = RevokeSnapshotAccessResponse'
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

-- | Creates a value of 'RevokeSnapshotAccessResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'snapshot' - Undocumented field.
mkRevokeSnapshotAccessResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RevokeSnapshotAccessResponse
mkRevokeSnapshotAccessResponse pResponseStatus_ =
  RevokeSnapshotAccessResponse'
    { snapshot = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'snapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsarsSnapshot :: Lens.Lens' RevokeSnapshotAccessResponse (Lude.Maybe Snapshot)
rsarsSnapshot = Lens.lens (snapshot :: RevokeSnapshotAccessResponse -> Lude.Maybe Snapshot) (\s a -> s {snapshot = a} :: RevokeSnapshotAccessResponse)
{-# DEPRECATED rsarsSnapshot "Use generic-lens or generic-optics with 'snapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsarsResponseStatus :: Lens.Lens' RevokeSnapshotAccessResponse Lude.Int
rsarsResponseStatus = Lens.lens (responseStatus :: RevokeSnapshotAccessResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RevokeSnapshotAccessResponse)
{-# DEPRECATED rsarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
