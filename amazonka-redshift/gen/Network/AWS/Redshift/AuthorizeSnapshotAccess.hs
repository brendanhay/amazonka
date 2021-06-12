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
-- Module      : Network.AWS.Redshift.AuthorizeSnapshotAccess
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Authorizes the specified AWS customer account to restore the specified
-- snapshot.
--
-- For more information about working with snapshots, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html Amazon Redshift Snapshots>
-- in the /Amazon Redshift Cluster Management Guide/.
module Network.AWS.Redshift.AuthorizeSnapshotAccess
  ( -- * Creating a Request
    AuthorizeSnapshotAccess (..),
    newAuthorizeSnapshotAccess,

    -- * Request Lenses
    authorizeSnapshotAccess_snapshotClusterIdentifier,
    authorizeSnapshotAccess_snapshotIdentifier,
    authorizeSnapshotAccess_accountWithRestoreAccess,

    -- * Destructuring the Response
    AuthorizeSnapshotAccessResponse (..),
    newAuthorizeSnapshotAccessResponse,

    -- * Response Lenses
    authorizeSnapshotAccessResponse_snapshot,
    authorizeSnapshotAccessResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newAuthorizeSnapshotAccess' smart constructor.
data AuthorizeSnapshotAccess = AuthorizeSnapshotAccess'
  { -- | The identifier of the cluster the snapshot was created from. This
    -- parameter is required if your IAM user has a policy containing a
    -- snapshot resource element that specifies anything other than * for the
    -- cluster name.
    snapshotClusterIdentifier :: Core.Maybe Core.Text,
    -- | The identifier of the snapshot the account is authorized to restore.
    snapshotIdentifier :: Core.Text,
    -- | The identifier of the AWS customer account authorized to restore the
    -- specified snapshot.
    --
    -- To share a snapshot with AWS support, specify amazon-redshift-support.
    accountWithRestoreAccess :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AuthorizeSnapshotAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotClusterIdentifier', 'authorizeSnapshotAccess_snapshotClusterIdentifier' - The identifier of the cluster the snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a
-- snapshot resource element that specifies anything other than * for the
-- cluster name.
--
-- 'snapshotIdentifier', 'authorizeSnapshotAccess_snapshotIdentifier' - The identifier of the snapshot the account is authorized to restore.
--
-- 'accountWithRestoreAccess', 'authorizeSnapshotAccess_accountWithRestoreAccess' - The identifier of the AWS customer account authorized to restore the
-- specified snapshot.
--
-- To share a snapshot with AWS support, specify amazon-redshift-support.
newAuthorizeSnapshotAccess ::
  -- | 'snapshotIdentifier'
  Core.Text ->
  -- | 'accountWithRestoreAccess'
  Core.Text ->
  AuthorizeSnapshotAccess
newAuthorizeSnapshotAccess
  pSnapshotIdentifier_
  pAccountWithRestoreAccess_ =
    AuthorizeSnapshotAccess'
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
authorizeSnapshotAccess_snapshotClusterIdentifier :: Lens.Lens' AuthorizeSnapshotAccess (Core.Maybe Core.Text)
authorizeSnapshotAccess_snapshotClusterIdentifier = Lens.lens (\AuthorizeSnapshotAccess' {snapshotClusterIdentifier} -> snapshotClusterIdentifier) (\s@AuthorizeSnapshotAccess' {} a -> s {snapshotClusterIdentifier = a} :: AuthorizeSnapshotAccess)

-- | The identifier of the snapshot the account is authorized to restore.
authorizeSnapshotAccess_snapshotIdentifier :: Lens.Lens' AuthorizeSnapshotAccess Core.Text
authorizeSnapshotAccess_snapshotIdentifier = Lens.lens (\AuthorizeSnapshotAccess' {snapshotIdentifier} -> snapshotIdentifier) (\s@AuthorizeSnapshotAccess' {} a -> s {snapshotIdentifier = a} :: AuthorizeSnapshotAccess)

-- | The identifier of the AWS customer account authorized to restore the
-- specified snapshot.
--
-- To share a snapshot with AWS support, specify amazon-redshift-support.
authorizeSnapshotAccess_accountWithRestoreAccess :: Lens.Lens' AuthorizeSnapshotAccess Core.Text
authorizeSnapshotAccess_accountWithRestoreAccess = Lens.lens (\AuthorizeSnapshotAccess' {accountWithRestoreAccess} -> accountWithRestoreAccess) (\s@AuthorizeSnapshotAccess' {} a -> s {accountWithRestoreAccess = a} :: AuthorizeSnapshotAccess)

instance Core.AWSRequest AuthorizeSnapshotAccess where
  type
    AWSResponse AuthorizeSnapshotAccess =
      AuthorizeSnapshotAccessResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "AuthorizeSnapshotAccessResult"
      ( \s h x ->
          AuthorizeSnapshotAccessResponse'
            Core.<$> (x Core..@? "Snapshot")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AuthorizeSnapshotAccess

instance Core.NFData AuthorizeSnapshotAccess

instance Core.ToHeaders AuthorizeSnapshotAccess where
  toHeaders = Core.const Core.mempty

instance Core.ToPath AuthorizeSnapshotAccess where
  toPath = Core.const "/"

instance Core.ToQuery AuthorizeSnapshotAccess where
  toQuery AuthorizeSnapshotAccess' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("AuthorizeSnapshotAccess" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "SnapshotClusterIdentifier"
          Core.=: snapshotClusterIdentifier,
        "SnapshotIdentifier" Core.=: snapshotIdentifier,
        "AccountWithRestoreAccess"
          Core.=: accountWithRestoreAccess
      ]

-- | /See:/ 'newAuthorizeSnapshotAccessResponse' smart constructor.
data AuthorizeSnapshotAccessResponse = AuthorizeSnapshotAccessResponse'
  { snapshot :: Core.Maybe Snapshot,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AuthorizeSnapshotAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshot', 'authorizeSnapshotAccessResponse_snapshot' - Undocumented member.
--
-- 'httpStatus', 'authorizeSnapshotAccessResponse_httpStatus' - The response's http status code.
newAuthorizeSnapshotAccessResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AuthorizeSnapshotAccessResponse
newAuthorizeSnapshotAccessResponse pHttpStatus_ =
  AuthorizeSnapshotAccessResponse'
    { snapshot =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
authorizeSnapshotAccessResponse_snapshot :: Lens.Lens' AuthorizeSnapshotAccessResponse (Core.Maybe Snapshot)
authorizeSnapshotAccessResponse_snapshot = Lens.lens (\AuthorizeSnapshotAccessResponse' {snapshot} -> snapshot) (\s@AuthorizeSnapshotAccessResponse' {} a -> s {snapshot = a} :: AuthorizeSnapshotAccessResponse)

-- | The response's http status code.
authorizeSnapshotAccessResponse_httpStatus :: Lens.Lens' AuthorizeSnapshotAccessResponse Core.Int
authorizeSnapshotAccessResponse_httpStatus = Lens.lens (\AuthorizeSnapshotAccessResponse' {httpStatus} -> httpStatus) (\s@AuthorizeSnapshotAccessResponse' {} a -> s {httpStatus = a} :: AuthorizeSnapshotAccessResponse)

instance Core.NFData AuthorizeSnapshotAccessResponse
