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
-- Module      : Amazonka.Redshift.AuthorizeSnapshotAccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Authorizes the specified Amazon Web Services account to restore the
-- specified snapshot.
--
-- For more information about working with snapshots, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html Amazon Redshift Snapshots>
-- in the /Amazon Redshift Cluster Management Guide/.
module Amazonka.Redshift.AuthorizeSnapshotAccess
  ( -- * Creating a Request
    AuthorizeSnapshotAccess (..),
    newAuthorizeSnapshotAccess,

    -- * Request Lenses
    authorizeSnapshotAccess_snapshotArn,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newAuthorizeSnapshotAccess' smart constructor.
data AuthorizeSnapshotAccess = AuthorizeSnapshotAccess'
  { -- | The Amazon Resource Name (ARN) of the snapshot to authorize access to.
    snapshotArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the cluster the snapshot was created from. This
    -- parameter is required if your IAM user has a policy containing a
    -- snapshot resource element that specifies anything other than * for the
    -- cluster name.
    snapshotClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the snapshot the account is authorized to restore.
    snapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Web Services account authorized to restore
    -- the specified snapshot.
    --
    -- To share a snapshot with Amazon Web Services Support, specify
    -- amazon-redshift-support.
    accountWithRestoreAccess :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthorizeSnapshotAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotArn', 'authorizeSnapshotAccess_snapshotArn' - The Amazon Resource Name (ARN) of the snapshot to authorize access to.
--
-- 'snapshotClusterIdentifier', 'authorizeSnapshotAccess_snapshotClusterIdentifier' - The identifier of the cluster the snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a
-- snapshot resource element that specifies anything other than * for the
-- cluster name.
--
-- 'snapshotIdentifier', 'authorizeSnapshotAccess_snapshotIdentifier' - The identifier of the snapshot the account is authorized to restore.
--
-- 'accountWithRestoreAccess', 'authorizeSnapshotAccess_accountWithRestoreAccess' - The identifier of the Amazon Web Services account authorized to restore
-- the specified snapshot.
--
-- To share a snapshot with Amazon Web Services Support, specify
-- amazon-redshift-support.
newAuthorizeSnapshotAccess ::
  -- | 'accountWithRestoreAccess'
  Prelude.Text ->
  AuthorizeSnapshotAccess
newAuthorizeSnapshotAccess pAccountWithRestoreAccess_ =
  AuthorizeSnapshotAccess'
    { snapshotArn =
        Prelude.Nothing,
      snapshotClusterIdentifier = Prelude.Nothing,
      snapshotIdentifier = Prelude.Nothing,
      accountWithRestoreAccess =
        pAccountWithRestoreAccess_
    }

-- | The Amazon Resource Name (ARN) of the snapshot to authorize access to.
authorizeSnapshotAccess_snapshotArn :: Lens.Lens' AuthorizeSnapshotAccess (Prelude.Maybe Prelude.Text)
authorizeSnapshotAccess_snapshotArn = Lens.lens (\AuthorizeSnapshotAccess' {snapshotArn} -> snapshotArn) (\s@AuthorizeSnapshotAccess' {} a -> s {snapshotArn = a} :: AuthorizeSnapshotAccess)

-- | The identifier of the cluster the snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a
-- snapshot resource element that specifies anything other than * for the
-- cluster name.
authorizeSnapshotAccess_snapshotClusterIdentifier :: Lens.Lens' AuthorizeSnapshotAccess (Prelude.Maybe Prelude.Text)
authorizeSnapshotAccess_snapshotClusterIdentifier = Lens.lens (\AuthorizeSnapshotAccess' {snapshotClusterIdentifier} -> snapshotClusterIdentifier) (\s@AuthorizeSnapshotAccess' {} a -> s {snapshotClusterIdentifier = a} :: AuthorizeSnapshotAccess)

-- | The identifier of the snapshot the account is authorized to restore.
authorizeSnapshotAccess_snapshotIdentifier :: Lens.Lens' AuthorizeSnapshotAccess (Prelude.Maybe Prelude.Text)
authorizeSnapshotAccess_snapshotIdentifier = Lens.lens (\AuthorizeSnapshotAccess' {snapshotIdentifier} -> snapshotIdentifier) (\s@AuthorizeSnapshotAccess' {} a -> s {snapshotIdentifier = a} :: AuthorizeSnapshotAccess)

-- | The identifier of the Amazon Web Services account authorized to restore
-- the specified snapshot.
--
-- To share a snapshot with Amazon Web Services Support, specify
-- amazon-redshift-support.
authorizeSnapshotAccess_accountWithRestoreAccess :: Lens.Lens' AuthorizeSnapshotAccess Prelude.Text
authorizeSnapshotAccess_accountWithRestoreAccess = Lens.lens (\AuthorizeSnapshotAccess' {accountWithRestoreAccess} -> accountWithRestoreAccess) (\s@AuthorizeSnapshotAccess' {} a -> s {accountWithRestoreAccess = a} :: AuthorizeSnapshotAccess)

instance Core.AWSRequest AuthorizeSnapshotAccess where
  type
    AWSResponse AuthorizeSnapshotAccess =
      AuthorizeSnapshotAccessResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "AuthorizeSnapshotAccessResult"
      ( \s h x ->
          AuthorizeSnapshotAccessResponse'
            Prelude.<$> (x Data..@? "Snapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AuthorizeSnapshotAccess where
  hashWithSalt _salt AuthorizeSnapshotAccess' {..} =
    _salt
      `Prelude.hashWithSalt` snapshotArn
      `Prelude.hashWithSalt` snapshotClusterIdentifier
      `Prelude.hashWithSalt` snapshotIdentifier
      `Prelude.hashWithSalt` accountWithRestoreAccess

instance Prelude.NFData AuthorizeSnapshotAccess where
  rnf AuthorizeSnapshotAccess' {..} =
    Prelude.rnf snapshotArn `Prelude.seq`
      Prelude.rnf snapshotClusterIdentifier `Prelude.seq`
        Prelude.rnf snapshotIdentifier `Prelude.seq`
          Prelude.rnf accountWithRestoreAccess

instance Data.ToHeaders AuthorizeSnapshotAccess where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AuthorizeSnapshotAccess where
  toPath = Prelude.const "/"

instance Data.ToQuery AuthorizeSnapshotAccess where
  toQuery AuthorizeSnapshotAccess' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AuthorizeSnapshotAccess" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "SnapshotArn" Data.=: snapshotArn,
        "SnapshotClusterIdentifier"
          Data.=: snapshotClusterIdentifier,
        "SnapshotIdentifier" Data.=: snapshotIdentifier,
        "AccountWithRestoreAccess"
          Data.=: accountWithRestoreAccess
      ]

-- | /See:/ 'newAuthorizeSnapshotAccessResponse' smart constructor.
data AuthorizeSnapshotAccessResponse = AuthorizeSnapshotAccessResponse'
  { snapshot :: Prelude.Maybe Snapshot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  AuthorizeSnapshotAccessResponse
newAuthorizeSnapshotAccessResponse pHttpStatus_ =
  AuthorizeSnapshotAccessResponse'
    { snapshot =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
authorizeSnapshotAccessResponse_snapshot :: Lens.Lens' AuthorizeSnapshotAccessResponse (Prelude.Maybe Snapshot)
authorizeSnapshotAccessResponse_snapshot = Lens.lens (\AuthorizeSnapshotAccessResponse' {snapshot} -> snapshot) (\s@AuthorizeSnapshotAccessResponse' {} a -> s {snapshot = a} :: AuthorizeSnapshotAccessResponse)

-- | The response's http status code.
authorizeSnapshotAccessResponse_httpStatus :: Lens.Lens' AuthorizeSnapshotAccessResponse Prelude.Int
authorizeSnapshotAccessResponse_httpStatus = Lens.lens (\AuthorizeSnapshotAccessResponse' {httpStatus} -> httpStatus) (\s@AuthorizeSnapshotAccessResponse' {} a -> s {httpStatus = a} :: AuthorizeSnapshotAccessResponse)

instance
  Prelude.NFData
    AuthorizeSnapshotAccessResponse
  where
  rnf AuthorizeSnapshotAccessResponse' {..} =
    Prelude.rnf snapshot `Prelude.seq`
      Prelude.rnf httpStatus
