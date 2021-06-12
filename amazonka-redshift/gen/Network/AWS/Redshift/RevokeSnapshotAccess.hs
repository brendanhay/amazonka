{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.RevokeSnapshotAccess
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the ability of the specified AWS customer account to restore the
-- specified snapshot. If the account is currently restoring the snapshot,
-- the restore will run to completion.
--
-- For more information about working with snapshots, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html Amazon Redshift Snapshots>
-- in the /Amazon Redshift Cluster Management Guide/.
module Network.AWS.Redshift.RevokeSnapshotAccess
  ( -- * Creating a Request
    RevokeSnapshotAccess (..),
    newRevokeSnapshotAccess,

    -- * Request Lenses
    revokeSnapshotAccess_snapshotClusterIdentifier,
    revokeSnapshotAccess_snapshotIdentifier,
    revokeSnapshotAccess_accountWithRestoreAccess,

    -- * Destructuring the Response
    RevokeSnapshotAccessResponse (..),
    newRevokeSnapshotAccessResponse,

    -- * Response Lenses
    revokeSnapshotAccessResponse_snapshot,
    revokeSnapshotAccessResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newRevokeSnapshotAccess' smart constructor.
data RevokeSnapshotAccess = RevokeSnapshotAccess'
  { -- | The identifier of the cluster the snapshot was created from. This
    -- parameter is required if your IAM user has a policy containing a
    -- snapshot resource element that specifies anything other than * for the
    -- cluster name.
    snapshotClusterIdentifier :: Core.Maybe Core.Text,
    -- | The identifier of the snapshot that the account can no longer access.
    snapshotIdentifier :: Core.Text,
    -- | The identifier of the AWS customer account that can no longer restore
    -- the specified snapshot.
    accountWithRestoreAccess :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RevokeSnapshotAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotClusterIdentifier', 'revokeSnapshotAccess_snapshotClusterIdentifier' - The identifier of the cluster the snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a
-- snapshot resource element that specifies anything other than * for the
-- cluster name.
--
-- 'snapshotIdentifier', 'revokeSnapshotAccess_snapshotIdentifier' - The identifier of the snapshot that the account can no longer access.
--
-- 'accountWithRestoreAccess', 'revokeSnapshotAccess_accountWithRestoreAccess' - The identifier of the AWS customer account that can no longer restore
-- the specified snapshot.
newRevokeSnapshotAccess ::
  -- | 'snapshotIdentifier'
  Core.Text ->
  -- | 'accountWithRestoreAccess'
  Core.Text ->
  RevokeSnapshotAccess
newRevokeSnapshotAccess
  pSnapshotIdentifier_
  pAccountWithRestoreAccess_ =
    RevokeSnapshotAccess'
      { snapshotClusterIdentifier =
          Core.Nothing,
        snapshotIdentifier = pSnapshotIdentifier_,
        accountWithRestoreAccess =
          pAccountWithRestoreAccess_
      }

-- | The identifier of the cluster the snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a
-- snapshot resource element that specifies anything other than * for the
-- cluster name.
revokeSnapshotAccess_snapshotClusterIdentifier :: Lens.Lens' RevokeSnapshotAccess (Core.Maybe Core.Text)
revokeSnapshotAccess_snapshotClusterIdentifier = Lens.lens (\RevokeSnapshotAccess' {snapshotClusterIdentifier} -> snapshotClusterIdentifier) (\s@RevokeSnapshotAccess' {} a -> s {snapshotClusterIdentifier = a} :: RevokeSnapshotAccess)

-- | The identifier of the snapshot that the account can no longer access.
revokeSnapshotAccess_snapshotIdentifier :: Lens.Lens' RevokeSnapshotAccess Core.Text
revokeSnapshotAccess_snapshotIdentifier = Lens.lens (\RevokeSnapshotAccess' {snapshotIdentifier} -> snapshotIdentifier) (\s@RevokeSnapshotAccess' {} a -> s {snapshotIdentifier = a} :: RevokeSnapshotAccess)

-- | The identifier of the AWS customer account that can no longer restore
-- the specified snapshot.
revokeSnapshotAccess_accountWithRestoreAccess :: Lens.Lens' RevokeSnapshotAccess Core.Text
revokeSnapshotAccess_accountWithRestoreAccess = Lens.lens (\RevokeSnapshotAccess' {accountWithRestoreAccess} -> accountWithRestoreAccess) (\s@RevokeSnapshotAccess' {} a -> s {accountWithRestoreAccess = a} :: RevokeSnapshotAccess)

instance Core.AWSRequest RevokeSnapshotAccess where
  type
    AWSResponse RevokeSnapshotAccess =
      RevokeSnapshotAccessResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "RevokeSnapshotAccessResult"
      ( \s h x ->
          RevokeSnapshotAccessResponse'
            Core.<$> (x Core..@? "Snapshot")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RevokeSnapshotAccess

instance Core.NFData RevokeSnapshotAccess

instance Core.ToHeaders RevokeSnapshotAccess where
  toHeaders = Core.const Core.mempty

instance Core.ToPath RevokeSnapshotAccess where
  toPath = Core.const "/"

instance Core.ToQuery RevokeSnapshotAccess where
  toQuery RevokeSnapshotAccess' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("RevokeSnapshotAccess" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "SnapshotClusterIdentifier"
          Core.=: snapshotClusterIdentifier,
        "SnapshotIdentifier" Core.=: snapshotIdentifier,
        "AccountWithRestoreAccess"
          Core.=: accountWithRestoreAccess
      ]

-- | /See:/ 'newRevokeSnapshotAccessResponse' smart constructor.
data RevokeSnapshotAccessResponse = RevokeSnapshotAccessResponse'
  { snapshot :: Core.Maybe Snapshot,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RevokeSnapshotAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshot', 'revokeSnapshotAccessResponse_snapshot' - Undocumented member.
--
-- 'httpStatus', 'revokeSnapshotAccessResponse_httpStatus' - The response's http status code.
newRevokeSnapshotAccessResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RevokeSnapshotAccessResponse
newRevokeSnapshotAccessResponse pHttpStatus_ =
  RevokeSnapshotAccessResponse'
    { snapshot =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
revokeSnapshotAccessResponse_snapshot :: Lens.Lens' RevokeSnapshotAccessResponse (Core.Maybe Snapshot)
revokeSnapshotAccessResponse_snapshot = Lens.lens (\RevokeSnapshotAccessResponse' {snapshot} -> snapshot) (\s@RevokeSnapshotAccessResponse' {} a -> s {snapshot = a} :: RevokeSnapshotAccessResponse)

-- | The response's http status code.
revokeSnapshotAccessResponse_httpStatus :: Lens.Lens' RevokeSnapshotAccessResponse Core.Int
revokeSnapshotAccessResponse_httpStatus = Lens.lens (\RevokeSnapshotAccessResponse' {httpStatus} -> httpStatus) (\s@RevokeSnapshotAccessResponse' {} a -> s {httpStatus = a} :: RevokeSnapshotAccessResponse)

instance Core.NFData RevokeSnapshotAccessResponse
