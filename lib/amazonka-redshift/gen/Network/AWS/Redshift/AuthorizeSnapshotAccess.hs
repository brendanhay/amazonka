{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.AuthorizeSnapshotAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Authorizes the specified AWS customer account to restore the specified snapshot.
--
-- For more information about working with snapshots, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html Amazon Redshift Snapshots> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.AuthorizeSnapshotAccess
  ( -- * Creating a request
    AuthorizeSnapshotAccess (..),
    mkAuthorizeSnapshotAccess,

    -- ** Request lenses
    asaSnapshotIdentifier,
    asaSnapshotClusterIdentifier,
    asaAccountWithRestoreAccess,

    -- * Destructuring the response
    AuthorizeSnapshotAccessResponse (..),
    mkAuthorizeSnapshotAccessResponse,

    -- ** Response lenses
    asarsSnapshot,
    asarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkAuthorizeSnapshotAccess' smart constructor.
data AuthorizeSnapshotAccess = AuthorizeSnapshotAccess'
  { -- | The identifier of the snapshot the account is authorized to restore.
    snapshotIdentifier :: Lude.Text,
    -- | The identifier of the cluster the snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
    snapshotClusterIdentifier :: Lude.Maybe Lude.Text,
    -- | The identifier of the AWS customer account authorized to restore the specified snapshot.
    --
    -- To share a snapshot with AWS support, specify amazon-redshift-support.
    accountWithRestoreAccess :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuthorizeSnapshotAccess' with the minimum fields required to make a request.
--
-- * 'snapshotIdentifier' - The identifier of the snapshot the account is authorized to restore.
-- * 'snapshotClusterIdentifier' - The identifier of the cluster the snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
-- * 'accountWithRestoreAccess' - The identifier of the AWS customer account authorized to restore the specified snapshot.
--
-- To share a snapshot with AWS support, specify amazon-redshift-support.
mkAuthorizeSnapshotAccess ::
  -- | 'snapshotIdentifier'
  Lude.Text ->
  -- | 'accountWithRestoreAccess'
  Lude.Text ->
  AuthorizeSnapshotAccess
mkAuthorizeSnapshotAccess
  pSnapshotIdentifier_
  pAccountWithRestoreAccess_ =
    AuthorizeSnapshotAccess'
      { snapshotIdentifier =
          pSnapshotIdentifier_,
        snapshotClusterIdentifier = Lude.Nothing,
        accountWithRestoreAccess = pAccountWithRestoreAccess_
      }

-- | The identifier of the snapshot the account is authorized to restore.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asaSnapshotIdentifier :: Lens.Lens' AuthorizeSnapshotAccess Lude.Text
asaSnapshotIdentifier = Lens.lens (snapshotIdentifier :: AuthorizeSnapshotAccess -> Lude.Text) (\s a -> s {snapshotIdentifier = a} :: AuthorizeSnapshotAccess)
{-# DEPRECATED asaSnapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead." #-}

-- | The identifier of the cluster the snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
--
-- /Note:/ Consider using 'snapshotClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asaSnapshotClusterIdentifier :: Lens.Lens' AuthorizeSnapshotAccess (Lude.Maybe Lude.Text)
asaSnapshotClusterIdentifier = Lens.lens (snapshotClusterIdentifier :: AuthorizeSnapshotAccess -> Lude.Maybe Lude.Text) (\s a -> s {snapshotClusterIdentifier = a} :: AuthorizeSnapshotAccess)
{-# DEPRECATED asaSnapshotClusterIdentifier "Use generic-lens or generic-optics with 'snapshotClusterIdentifier' instead." #-}

-- | The identifier of the AWS customer account authorized to restore the specified snapshot.
--
-- To share a snapshot with AWS support, specify amazon-redshift-support.
--
-- /Note:/ Consider using 'accountWithRestoreAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asaAccountWithRestoreAccess :: Lens.Lens' AuthorizeSnapshotAccess Lude.Text
asaAccountWithRestoreAccess = Lens.lens (accountWithRestoreAccess :: AuthorizeSnapshotAccess -> Lude.Text) (\s a -> s {accountWithRestoreAccess = a} :: AuthorizeSnapshotAccess)
{-# DEPRECATED asaAccountWithRestoreAccess "Use generic-lens or generic-optics with 'accountWithRestoreAccess' instead." #-}

instance Lude.AWSRequest AuthorizeSnapshotAccess where
  type Rs AuthorizeSnapshotAccess = AuthorizeSnapshotAccessResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "AuthorizeSnapshotAccessResult"
      ( \s h x ->
          AuthorizeSnapshotAccessResponse'
            Lude.<$> (x Lude..@? "Snapshot") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AuthorizeSnapshotAccess where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AuthorizeSnapshotAccess where
  toPath = Lude.const "/"

instance Lude.ToQuery AuthorizeSnapshotAccess where
  toQuery AuthorizeSnapshotAccess' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AuthorizeSnapshotAccess" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "SnapshotIdentifier" Lude.=: snapshotIdentifier,
        "SnapshotClusterIdentifier" Lude.=: snapshotClusterIdentifier,
        "AccountWithRestoreAccess" Lude.=: accountWithRestoreAccess
      ]

-- | /See:/ 'mkAuthorizeSnapshotAccessResponse' smart constructor.
data AuthorizeSnapshotAccessResponse = AuthorizeSnapshotAccessResponse'
  { snapshot :: Lude.Maybe Snapshot,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuthorizeSnapshotAccessResponse' with the minimum fields required to make a request.
--
-- * 'snapshot' -
-- * 'responseStatus' - The response status code.
mkAuthorizeSnapshotAccessResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AuthorizeSnapshotAccessResponse
mkAuthorizeSnapshotAccessResponse pResponseStatus_ =
  AuthorizeSnapshotAccessResponse'
    { snapshot = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'snapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asarsSnapshot :: Lens.Lens' AuthorizeSnapshotAccessResponse (Lude.Maybe Snapshot)
asarsSnapshot = Lens.lens (snapshot :: AuthorizeSnapshotAccessResponse -> Lude.Maybe Snapshot) (\s a -> s {snapshot = a} :: AuthorizeSnapshotAccessResponse)
{-# DEPRECATED asarsSnapshot "Use generic-lens or generic-optics with 'snapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asarsResponseStatus :: Lens.Lens' AuthorizeSnapshotAccessResponse Lude.Int
asarsResponseStatus = Lens.lens (responseStatus :: AuthorizeSnapshotAccessResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AuthorizeSnapshotAccessResponse)
{-# DEPRECATED asarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
