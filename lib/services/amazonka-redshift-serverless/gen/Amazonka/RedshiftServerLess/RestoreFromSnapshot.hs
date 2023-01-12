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
-- Module      : Amazonka.RedshiftServerLess.RestoreFromSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a namespace from a snapshot.
module Amazonka.RedshiftServerLess.RestoreFromSnapshot
  ( -- * Creating a Request
    RestoreFromSnapshot (..),
    newRestoreFromSnapshot,

    -- * Request Lenses
    restoreFromSnapshot_ownerAccount,
    restoreFromSnapshot_snapshotArn,
    restoreFromSnapshot_snapshotName,
    restoreFromSnapshot_namespaceName,
    restoreFromSnapshot_workgroupName,

    -- * Destructuring the Response
    RestoreFromSnapshotResponse (..),
    newRestoreFromSnapshotResponse,

    -- * Response Lenses
    restoreFromSnapshotResponse_namespace,
    restoreFromSnapshotResponse_ownerAccount,
    restoreFromSnapshotResponse_snapshotName,
    restoreFromSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRestoreFromSnapshot' smart constructor.
data RestoreFromSnapshot = RestoreFromSnapshot'
  { -- | The Amazon Web Services account that owns the snapshot.
    ownerAccount :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the snapshot to restore from. Required
    -- if restoring from Amazon Redshift Serverless to a provisioned cluster.
    -- Must not be specified at the same time as @snapshotName@.
    --
    -- The format of the ARN is
    -- arn:aws:redshift:\<region>:\<account_id>:snapshot:\<cluster_identifier>\/\<snapshot_identifier>.
    snapshotArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the snapshot to restore from. Must not be specified at the
    -- same time as @snapshotArn@.
    snapshotName :: Prelude.Maybe Prelude.Text,
    -- | The name of the namespace to restore the snapshot to.
    namespaceName :: Prelude.Text,
    -- | The name of the workgroup used to restore the snapshot.
    workgroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreFromSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerAccount', 'restoreFromSnapshot_ownerAccount' - The Amazon Web Services account that owns the snapshot.
--
-- 'snapshotArn', 'restoreFromSnapshot_snapshotArn' - The Amazon Resource Name (ARN) of the snapshot to restore from. Required
-- if restoring from Amazon Redshift Serverless to a provisioned cluster.
-- Must not be specified at the same time as @snapshotName@.
--
-- The format of the ARN is
-- arn:aws:redshift:\<region>:\<account_id>:snapshot:\<cluster_identifier>\/\<snapshot_identifier>.
--
-- 'snapshotName', 'restoreFromSnapshot_snapshotName' - The name of the snapshot to restore from. Must not be specified at the
-- same time as @snapshotArn@.
--
-- 'namespaceName', 'restoreFromSnapshot_namespaceName' - The name of the namespace to restore the snapshot to.
--
-- 'workgroupName', 'restoreFromSnapshot_workgroupName' - The name of the workgroup used to restore the snapshot.
newRestoreFromSnapshot ::
  -- | 'namespaceName'
  Prelude.Text ->
  -- | 'workgroupName'
  Prelude.Text ->
  RestoreFromSnapshot
newRestoreFromSnapshot
  pNamespaceName_
  pWorkgroupName_ =
    RestoreFromSnapshot'
      { ownerAccount =
          Prelude.Nothing,
        snapshotArn = Prelude.Nothing,
        snapshotName = Prelude.Nothing,
        namespaceName = pNamespaceName_,
        workgroupName = pWorkgroupName_
      }

-- | The Amazon Web Services account that owns the snapshot.
restoreFromSnapshot_ownerAccount :: Lens.Lens' RestoreFromSnapshot (Prelude.Maybe Prelude.Text)
restoreFromSnapshot_ownerAccount = Lens.lens (\RestoreFromSnapshot' {ownerAccount} -> ownerAccount) (\s@RestoreFromSnapshot' {} a -> s {ownerAccount = a} :: RestoreFromSnapshot)

-- | The Amazon Resource Name (ARN) of the snapshot to restore from. Required
-- if restoring from Amazon Redshift Serverless to a provisioned cluster.
-- Must not be specified at the same time as @snapshotName@.
--
-- The format of the ARN is
-- arn:aws:redshift:\<region>:\<account_id>:snapshot:\<cluster_identifier>\/\<snapshot_identifier>.
restoreFromSnapshot_snapshotArn :: Lens.Lens' RestoreFromSnapshot (Prelude.Maybe Prelude.Text)
restoreFromSnapshot_snapshotArn = Lens.lens (\RestoreFromSnapshot' {snapshotArn} -> snapshotArn) (\s@RestoreFromSnapshot' {} a -> s {snapshotArn = a} :: RestoreFromSnapshot)

-- | The name of the snapshot to restore from. Must not be specified at the
-- same time as @snapshotArn@.
restoreFromSnapshot_snapshotName :: Lens.Lens' RestoreFromSnapshot (Prelude.Maybe Prelude.Text)
restoreFromSnapshot_snapshotName = Lens.lens (\RestoreFromSnapshot' {snapshotName} -> snapshotName) (\s@RestoreFromSnapshot' {} a -> s {snapshotName = a} :: RestoreFromSnapshot)

-- | The name of the namespace to restore the snapshot to.
restoreFromSnapshot_namespaceName :: Lens.Lens' RestoreFromSnapshot Prelude.Text
restoreFromSnapshot_namespaceName = Lens.lens (\RestoreFromSnapshot' {namespaceName} -> namespaceName) (\s@RestoreFromSnapshot' {} a -> s {namespaceName = a} :: RestoreFromSnapshot)

-- | The name of the workgroup used to restore the snapshot.
restoreFromSnapshot_workgroupName :: Lens.Lens' RestoreFromSnapshot Prelude.Text
restoreFromSnapshot_workgroupName = Lens.lens (\RestoreFromSnapshot' {workgroupName} -> workgroupName) (\s@RestoreFromSnapshot' {} a -> s {workgroupName = a} :: RestoreFromSnapshot)

instance Core.AWSRequest RestoreFromSnapshot where
  type
    AWSResponse RestoreFromSnapshot =
      RestoreFromSnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RestoreFromSnapshotResponse'
            Prelude.<$> (x Data..?> "namespace")
            Prelude.<*> (x Data..?> "ownerAccount")
            Prelude.<*> (x Data..?> "snapshotName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RestoreFromSnapshot where
  hashWithSalt _salt RestoreFromSnapshot' {..} =
    _salt `Prelude.hashWithSalt` ownerAccount
      `Prelude.hashWithSalt` snapshotArn
      `Prelude.hashWithSalt` snapshotName
      `Prelude.hashWithSalt` namespaceName
      `Prelude.hashWithSalt` workgroupName

instance Prelude.NFData RestoreFromSnapshot where
  rnf RestoreFromSnapshot' {..} =
    Prelude.rnf ownerAccount
      `Prelude.seq` Prelude.rnf snapshotArn
      `Prelude.seq` Prelude.rnf snapshotName
      `Prelude.seq` Prelude.rnf namespaceName
      `Prelude.seq` Prelude.rnf workgroupName

instance Data.ToHeaders RestoreFromSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftServerless.RestoreFromSnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RestoreFromSnapshot where
  toJSON RestoreFromSnapshot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ownerAccount" Data..=) Prelude.<$> ownerAccount,
            ("snapshotArn" Data..=) Prelude.<$> snapshotArn,
            ("snapshotName" Data..=) Prelude.<$> snapshotName,
            Prelude.Just ("namespaceName" Data..= namespaceName),
            Prelude.Just
              ("workgroupName" Data..= workgroupName)
          ]
      )

instance Data.ToPath RestoreFromSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery RestoreFromSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRestoreFromSnapshotResponse' smart constructor.
data RestoreFromSnapshotResponse = RestoreFromSnapshotResponse'
  { namespace :: Prelude.Maybe Namespace,
    -- | The owner Amazon Web Services; account of the snapshot that was
    -- restored.
    ownerAccount :: Prelude.Maybe Prelude.Text,
    -- | The name of the snapshot used to restore the namespace.
    snapshotName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreFromSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespace', 'restoreFromSnapshotResponse_namespace' - Undocumented member.
--
-- 'ownerAccount', 'restoreFromSnapshotResponse_ownerAccount' - The owner Amazon Web Services; account of the snapshot that was
-- restored.
--
-- 'snapshotName', 'restoreFromSnapshotResponse_snapshotName' - The name of the snapshot used to restore the namespace.
--
-- 'httpStatus', 'restoreFromSnapshotResponse_httpStatus' - The response's http status code.
newRestoreFromSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreFromSnapshotResponse
newRestoreFromSnapshotResponse pHttpStatus_ =
  RestoreFromSnapshotResponse'
    { namespace =
        Prelude.Nothing,
      ownerAccount = Prelude.Nothing,
      snapshotName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
restoreFromSnapshotResponse_namespace :: Lens.Lens' RestoreFromSnapshotResponse (Prelude.Maybe Namespace)
restoreFromSnapshotResponse_namespace = Lens.lens (\RestoreFromSnapshotResponse' {namespace} -> namespace) (\s@RestoreFromSnapshotResponse' {} a -> s {namespace = a} :: RestoreFromSnapshotResponse)

-- | The owner Amazon Web Services; account of the snapshot that was
-- restored.
restoreFromSnapshotResponse_ownerAccount :: Lens.Lens' RestoreFromSnapshotResponse (Prelude.Maybe Prelude.Text)
restoreFromSnapshotResponse_ownerAccount = Lens.lens (\RestoreFromSnapshotResponse' {ownerAccount} -> ownerAccount) (\s@RestoreFromSnapshotResponse' {} a -> s {ownerAccount = a} :: RestoreFromSnapshotResponse)

-- | The name of the snapshot used to restore the namespace.
restoreFromSnapshotResponse_snapshotName :: Lens.Lens' RestoreFromSnapshotResponse (Prelude.Maybe Prelude.Text)
restoreFromSnapshotResponse_snapshotName = Lens.lens (\RestoreFromSnapshotResponse' {snapshotName} -> snapshotName) (\s@RestoreFromSnapshotResponse' {} a -> s {snapshotName = a} :: RestoreFromSnapshotResponse)

-- | The response's http status code.
restoreFromSnapshotResponse_httpStatus :: Lens.Lens' RestoreFromSnapshotResponse Prelude.Int
restoreFromSnapshotResponse_httpStatus = Lens.lens (\RestoreFromSnapshotResponse' {httpStatus} -> httpStatus) (\s@RestoreFromSnapshotResponse' {} a -> s {httpStatus = a} :: RestoreFromSnapshotResponse)

instance Prelude.NFData RestoreFromSnapshotResponse where
  rnf RestoreFromSnapshotResponse' {..} =
    Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf ownerAccount
      `Prelude.seq` Prelude.rnf snapshotName
      `Prelude.seq` Prelude.rnf httpStatus
