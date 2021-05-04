{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    snapshotClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the snapshot that the account can no longer access.
    snapshotIdentifier :: Prelude.Text,
    -- | The identifier of the AWS customer account that can no longer restore
    -- the specified snapshot.
    accountWithRestoreAccess :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'accountWithRestoreAccess'
  Prelude.Text ->
  RevokeSnapshotAccess
newRevokeSnapshotAccess
  pSnapshotIdentifier_
  pAccountWithRestoreAccess_ =
    RevokeSnapshotAccess'
      { snapshotClusterIdentifier =
          Prelude.Nothing,
        snapshotIdentifier = pSnapshotIdentifier_,
        accountWithRestoreAccess =
          pAccountWithRestoreAccess_
      }

-- | The identifier of the cluster the snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a
-- snapshot resource element that specifies anything other than * for the
-- cluster name.
revokeSnapshotAccess_snapshotClusterIdentifier :: Lens.Lens' RevokeSnapshotAccess (Prelude.Maybe Prelude.Text)
revokeSnapshotAccess_snapshotClusterIdentifier = Lens.lens (\RevokeSnapshotAccess' {snapshotClusterIdentifier} -> snapshotClusterIdentifier) (\s@RevokeSnapshotAccess' {} a -> s {snapshotClusterIdentifier = a} :: RevokeSnapshotAccess)

-- | The identifier of the snapshot that the account can no longer access.
revokeSnapshotAccess_snapshotIdentifier :: Lens.Lens' RevokeSnapshotAccess Prelude.Text
revokeSnapshotAccess_snapshotIdentifier = Lens.lens (\RevokeSnapshotAccess' {snapshotIdentifier} -> snapshotIdentifier) (\s@RevokeSnapshotAccess' {} a -> s {snapshotIdentifier = a} :: RevokeSnapshotAccess)

-- | The identifier of the AWS customer account that can no longer restore
-- the specified snapshot.
revokeSnapshotAccess_accountWithRestoreAccess :: Lens.Lens' RevokeSnapshotAccess Prelude.Text
revokeSnapshotAccess_accountWithRestoreAccess = Lens.lens (\RevokeSnapshotAccess' {accountWithRestoreAccess} -> accountWithRestoreAccess) (\s@RevokeSnapshotAccess' {} a -> s {accountWithRestoreAccess = a} :: RevokeSnapshotAccess)

instance Prelude.AWSRequest RevokeSnapshotAccess where
  type
    Rs RevokeSnapshotAccess =
      RevokeSnapshotAccessResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "RevokeSnapshotAccessResult"
      ( \s h x ->
          RevokeSnapshotAccessResponse'
            Prelude.<$> (x Prelude..@? "Snapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RevokeSnapshotAccess

instance Prelude.NFData RevokeSnapshotAccess

instance Prelude.ToHeaders RevokeSnapshotAccess where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath RevokeSnapshotAccess where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RevokeSnapshotAccess where
  toQuery RevokeSnapshotAccess' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("RevokeSnapshotAccess" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2012-12-01" :: Prelude.ByteString),
        "SnapshotClusterIdentifier"
          Prelude.=: snapshotClusterIdentifier,
        "SnapshotIdentifier" Prelude.=: snapshotIdentifier,
        "AccountWithRestoreAccess"
          Prelude.=: accountWithRestoreAccess
      ]

-- | /See:/ 'newRevokeSnapshotAccessResponse' smart constructor.
data RevokeSnapshotAccessResponse = RevokeSnapshotAccessResponse'
  { snapshot :: Prelude.Maybe Snapshot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  RevokeSnapshotAccessResponse
newRevokeSnapshotAccessResponse pHttpStatus_ =
  RevokeSnapshotAccessResponse'
    { snapshot =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
revokeSnapshotAccessResponse_snapshot :: Lens.Lens' RevokeSnapshotAccessResponse (Prelude.Maybe Snapshot)
revokeSnapshotAccessResponse_snapshot = Lens.lens (\RevokeSnapshotAccessResponse' {snapshot} -> snapshot) (\s@RevokeSnapshotAccessResponse' {} a -> s {snapshot = a} :: RevokeSnapshotAccessResponse)

-- | The response's http status code.
revokeSnapshotAccessResponse_httpStatus :: Lens.Lens' RevokeSnapshotAccessResponse Prelude.Int
revokeSnapshotAccessResponse_httpStatus = Lens.lens (\RevokeSnapshotAccessResponse' {httpStatus} -> httpStatus) (\s@RevokeSnapshotAccessResponse' {} a -> s {httpStatus = a} :: RevokeSnapshotAccessResponse)

instance Prelude.NFData RevokeSnapshotAccessResponse
