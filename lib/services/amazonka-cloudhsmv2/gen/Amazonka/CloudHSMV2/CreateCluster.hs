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
-- Module      : Amazonka.CloudHSMV2.CreateCluster
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new AWS CloudHSM cluster.
module Amazonka.CloudHSMV2.CreateCluster
  ( -- * Creating a Request
    CreateCluster (..),
    newCreateCluster,

    -- * Request Lenses
    createCluster_tagList,
    createCluster_backupRetentionPolicy,
    createCluster_sourceBackupId,
    createCluster_hsmType,
    createCluster_subnetIds,

    -- * Destructuring the Response
    CreateClusterResponse (..),
    newCreateClusterResponse,

    -- * Response Lenses
    createClusterResponse_cluster,
    createClusterResponse_httpStatus,
  )
where

import Amazonka.CloudHSMV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCluster' smart constructor.
data CreateCluster = CreateCluster'
  { -- | Tags to apply to the CloudHSM cluster during creation.
    tagList :: Prelude.Maybe [Tag],
    -- | A policy that defines how the service retains backups.
    backupRetentionPolicy :: Prelude.Maybe BackupRetentionPolicy,
    -- | The identifier (ID) of the cluster backup to restore. Use this value to
    -- restore the cluster from a backup instead of creating a new cluster. To
    -- find the backup ID, use DescribeBackups.
    sourceBackupId :: Prelude.Maybe Prelude.Text,
    -- | The type of HSM to use in the cluster. Currently the only allowed value
    -- is @hsm1.medium@.
    hsmType :: Prelude.Text,
    -- | The identifiers (IDs) of the subnets where you are creating the cluster.
    -- You must specify at least one subnet. If you specify multiple subnets,
    -- they must meet the following criteria:
    --
    -- -   All subnets must be in the same virtual private cloud (VPC).
    --
    -- -   You can specify only one subnet per Availability Zone.
    subnetIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagList', 'createCluster_tagList' - Tags to apply to the CloudHSM cluster during creation.
--
-- 'backupRetentionPolicy', 'createCluster_backupRetentionPolicy' - A policy that defines how the service retains backups.
--
-- 'sourceBackupId', 'createCluster_sourceBackupId' - The identifier (ID) of the cluster backup to restore. Use this value to
-- restore the cluster from a backup instead of creating a new cluster. To
-- find the backup ID, use DescribeBackups.
--
-- 'hsmType', 'createCluster_hsmType' - The type of HSM to use in the cluster. Currently the only allowed value
-- is @hsm1.medium@.
--
-- 'subnetIds', 'createCluster_subnetIds' - The identifiers (IDs) of the subnets where you are creating the cluster.
-- You must specify at least one subnet. If you specify multiple subnets,
-- they must meet the following criteria:
--
-- -   All subnets must be in the same virtual private cloud (VPC).
--
-- -   You can specify only one subnet per Availability Zone.
newCreateCluster ::
  -- | 'hsmType'
  Prelude.Text ->
  -- | 'subnetIds'
  Prelude.NonEmpty Prelude.Text ->
  CreateCluster
newCreateCluster pHsmType_ pSubnetIds_ =
  CreateCluster'
    { tagList = Prelude.Nothing,
      backupRetentionPolicy = Prelude.Nothing,
      sourceBackupId = Prelude.Nothing,
      hsmType = pHsmType_,
      subnetIds = Lens.coerced Lens.# pSubnetIds_
    }

-- | Tags to apply to the CloudHSM cluster during creation.
createCluster_tagList :: Lens.Lens' CreateCluster (Prelude.Maybe [Tag])
createCluster_tagList = Lens.lens (\CreateCluster' {tagList} -> tagList) (\s@CreateCluster' {} a -> s {tagList = a} :: CreateCluster) Prelude.. Lens.mapping Lens.coerced

-- | A policy that defines how the service retains backups.
createCluster_backupRetentionPolicy :: Lens.Lens' CreateCluster (Prelude.Maybe BackupRetentionPolicy)
createCluster_backupRetentionPolicy = Lens.lens (\CreateCluster' {backupRetentionPolicy} -> backupRetentionPolicy) (\s@CreateCluster' {} a -> s {backupRetentionPolicy = a} :: CreateCluster)

-- | The identifier (ID) of the cluster backup to restore. Use this value to
-- restore the cluster from a backup instead of creating a new cluster. To
-- find the backup ID, use DescribeBackups.
createCluster_sourceBackupId :: Lens.Lens' CreateCluster (Prelude.Maybe Prelude.Text)
createCluster_sourceBackupId = Lens.lens (\CreateCluster' {sourceBackupId} -> sourceBackupId) (\s@CreateCluster' {} a -> s {sourceBackupId = a} :: CreateCluster)

-- | The type of HSM to use in the cluster. Currently the only allowed value
-- is @hsm1.medium@.
createCluster_hsmType :: Lens.Lens' CreateCluster Prelude.Text
createCluster_hsmType = Lens.lens (\CreateCluster' {hsmType} -> hsmType) (\s@CreateCluster' {} a -> s {hsmType = a} :: CreateCluster)

-- | The identifiers (IDs) of the subnets where you are creating the cluster.
-- You must specify at least one subnet. If you specify multiple subnets,
-- they must meet the following criteria:
--
-- -   All subnets must be in the same virtual private cloud (VPC).
--
-- -   You can specify only one subnet per Availability Zone.
createCluster_subnetIds :: Lens.Lens' CreateCluster (Prelude.NonEmpty Prelude.Text)
createCluster_subnetIds = Lens.lens (\CreateCluster' {subnetIds} -> subnetIds) (\s@CreateCluster' {} a -> s {subnetIds = a} :: CreateCluster) Prelude.. Lens.coerced

instance Core.AWSRequest CreateCluster where
  type
    AWSResponse CreateCluster =
      CreateClusterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateClusterResponse'
            Prelude.<$> (x Data..?> "Cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCluster where
  hashWithSalt _salt CreateCluster' {..} =
    _salt `Prelude.hashWithSalt` tagList
      `Prelude.hashWithSalt` backupRetentionPolicy
      `Prelude.hashWithSalt` sourceBackupId
      `Prelude.hashWithSalt` hsmType
      `Prelude.hashWithSalt` subnetIds

instance Prelude.NFData CreateCluster where
  rnf CreateCluster' {..} =
    Prelude.rnf tagList
      `Prelude.seq` Prelude.rnf backupRetentionPolicy
      `Prelude.seq` Prelude.rnf sourceBackupId
      `Prelude.seq` Prelude.rnf hsmType
      `Prelude.seq` Prelude.rnf subnetIds

instance Data.ToHeaders CreateCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "BaldrApiService.CreateCluster" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCluster where
  toJSON CreateCluster' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TagList" Data..=) Prelude.<$> tagList,
            ("BackupRetentionPolicy" Data..=)
              Prelude.<$> backupRetentionPolicy,
            ("SourceBackupId" Data..=)
              Prelude.<$> sourceBackupId,
            Prelude.Just ("HsmType" Data..= hsmType),
            Prelude.Just ("SubnetIds" Data..= subnetIds)
          ]
      )

instance Data.ToPath CreateCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { -- | Information about the cluster that was created.
    cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'createClusterResponse_cluster' - Information about the cluster that was created.
--
-- 'httpStatus', 'createClusterResponse_httpStatus' - The response's http status code.
newCreateClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateClusterResponse
newCreateClusterResponse pHttpStatus_ =
  CreateClusterResponse'
    { cluster = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the cluster that was created.
createClusterResponse_cluster :: Lens.Lens' CreateClusterResponse (Prelude.Maybe Cluster)
createClusterResponse_cluster = Lens.lens (\CreateClusterResponse' {cluster} -> cluster) (\s@CreateClusterResponse' {} a -> s {cluster = a} :: CreateClusterResponse)

-- | The response's http status code.
createClusterResponse_httpStatus :: Lens.Lens' CreateClusterResponse Prelude.Int
createClusterResponse_httpStatus = Lens.lens (\CreateClusterResponse' {httpStatus} -> httpStatus) (\s@CreateClusterResponse' {} a -> s {httpStatus = a} :: CreateClusterResponse)

instance Prelude.NFData CreateClusterResponse where
  rnf CreateClusterResponse' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf httpStatus
