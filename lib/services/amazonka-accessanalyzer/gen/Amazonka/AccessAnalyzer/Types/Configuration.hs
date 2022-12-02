{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AccessAnalyzer.Types.Configuration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.Configuration where

import Amazonka.AccessAnalyzer.Types.EbsSnapshotConfiguration
import Amazonka.AccessAnalyzer.Types.EcrRepositoryConfiguration
import Amazonka.AccessAnalyzer.Types.EfsFileSystemConfiguration
import Amazonka.AccessAnalyzer.Types.IamRoleConfiguration
import Amazonka.AccessAnalyzer.Types.KmsKeyConfiguration
import Amazonka.AccessAnalyzer.Types.RdsDbClusterSnapshotConfiguration
import Amazonka.AccessAnalyzer.Types.RdsDbSnapshotConfiguration
import Amazonka.AccessAnalyzer.Types.S3BucketConfiguration
import Amazonka.AccessAnalyzer.Types.SecretsManagerSecretConfiguration
import Amazonka.AccessAnalyzer.Types.SnsTopicConfiguration
import Amazonka.AccessAnalyzer.Types.SqsQueueConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Access control configuration structures for your resource. You specify
-- the configuration as a type-value pair. You can specify only one type of
-- access control configuration.
--
-- /See:/ 'newConfiguration' smart constructor.
data Configuration = Configuration'
  { -- | The access control configuration is for an Amazon ECR repository.
    ecrRepository :: Prelude.Maybe EcrRepositoryConfiguration,
    -- | The access control configuration is for an Amazon S3 Bucket.
    s3Bucket :: Prelude.Maybe S3BucketConfiguration,
    -- | The access control configuration is for an IAM role.
    iamRole :: Prelude.Maybe IamRoleConfiguration,
    -- | The access control configuration is for an Amazon SNS topic
    snsTopic :: Prelude.Maybe SnsTopicConfiguration,
    -- | The access control configuration is for a KMS key.
    kmsKey :: Prelude.Maybe KmsKeyConfiguration,
    -- | The access control configuration is for an Amazon RDS DB snapshot.
    rdsDbSnapshot :: Prelude.Maybe RdsDbSnapshotConfiguration,
    -- | The access control configuration is for an Amazon RDS DB cluster
    -- snapshot.
    rdsDbClusterSnapshot :: Prelude.Maybe RdsDbClusterSnapshotConfiguration,
    -- | The access control configuration is for an Amazon EFS file system.
    efsFileSystem :: Prelude.Maybe EfsFileSystemConfiguration,
    -- | The access control configuration is for an Amazon SQS queue.
    sqsQueue :: Prelude.Maybe SqsQueueConfiguration,
    -- | The access control configuration is for a Secrets Manager secret.
    secretsManagerSecret :: Prelude.Maybe SecretsManagerSecretConfiguration,
    -- | The access control configuration is for an Amazon EBS volume snapshot.
    ebsSnapshot :: Prelude.Maybe EbsSnapshotConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Configuration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ecrRepository', 'configuration_ecrRepository' - The access control configuration is for an Amazon ECR repository.
--
-- 's3Bucket', 'configuration_s3Bucket' - The access control configuration is for an Amazon S3 Bucket.
--
-- 'iamRole', 'configuration_iamRole' - The access control configuration is for an IAM role.
--
-- 'snsTopic', 'configuration_snsTopic' - The access control configuration is for an Amazon SNS topic
--
-- 'kmsKey', 'configuration_kmsKey' - The access control configuration is for a KMS key.
--
-- 'rdsDbSnapshot', 'configuration_rdsDbSnapshot' - The access control configuration is for an Amazon RDS DB snapshot.
--
-- 'rdsDbClusterSnapshot', 'configuration_rdsDbClusterSnapshot' - The access control configuration is for an Amazon RDS DB cluster
-- snapshot.
--
-- 'efsFileSystem', 'configuration_efsFileSystem' - The access control configuration is for an Amazon EFS file system.
--
-- 'sqsQueue', 'configuration_sqsQueue' - The access control configuration is for an Amazon SQS queue.
--
-- 'secretsManagerSecret', 'configuration_secretsManagerSecret' - The access control configuration is for a Secrets Manager secret.
--
-- 'ebsSnapshot', 'configuration_ebsSnapshot' - The access control configuration is for an Amazon EBS volume snapshot.
newConfiguration ::
  Configuration
newConfiguration =
  Configuration'
    { ecrRepository = Prelude.Nothing,
      s3Bucket = Prelude.Nothing,
      iamRole = Prelude.Nothing,
      snsTopic = Prelude.Nothing,
      kmsKey = Prelude.Nothing,
      rdsDbSnapshot = Prelude.Nothing,
      rdsDbClusterSnapshot = Prelude.Nothing,
      efsFileSystem = Prelude.Nothing,
      sqsQueue = Prelude.Nothing,
      secretsManagerSecret = Prelude.Nothing,
      ebsSnapshot = Prelude.Nothing
    }

-- | The access control configuration is for an Amazon ECR repository.
configuration_ecrRepository :: Lens.Lens' Configuration (Prelude.Maybe EcrRepositoryConfiguration)
configuration_ecrRepository = Lens.lens (\Configuration' {ecrRepository} -> ecrRepository) (\s@Configuration' {} a -> s {ecrRepository = a} :: Configuration)

-- | The access control configuration is for an Amazon S3 Bucket.
configuration_s3Bucket :: Lens.Lens' Configuration (Prelude.Maybe S3BucketConfiguration)
configuration_s3Bucket = Lens.lens (\Configuration' {s3Bucket} -> s3Bucket) (\s@Configuration' {} a -> s {s3Bucket = a} :: Configuration)

-- | The access control configuration is for an IAM role.
configuration_iamRole :: Lens.Lens' Configuration (Prelude.Maybe IamRoleConfiguration)
configuration_iamRole = Lens.lens (\Configuration' {iamRole} -> iamRole) (\s@Configuration' {} a -> s {iamRole = a} :: Configuration)

-- | The access control configuration is for an Amazon SNS topic
configuration_snsTopic :: Lens.Lens' Configuration (Prelude.Maybe SnsTopicConfiguration)
configuration_snsTopic = Lens.lens (\Configuration' {snsTopic} -> snsTopic) (\s@Configuration' {} a -> s {snsTopic = a} :: Configuration)

-- | The access control configuration is for a KMS key.
configuration_kmsKey :: Lens.Lens' Configuration (Prelude.Maybe KmsKeyConfiguration)
configuration_kmsKey = Lens.lens (\Configuration' {kmsKey} -> kmsKey) (\s@Configuration' {} a -> s {kmsKey = a} :: Configuration)

-- | The access control configuration is for an Amazon RDS DB snapshot.
configuration_rdsDbSnapshot :: Lens.Lens' Configuration (Prelude.Maybe RdsDbSnapshotConfiguration)
configuration_rdsDbSnapshot = Lens.lens (\Configuration' {rdsDbSnapshot} -> rdsDbSnapshot) (\s@Configuration' {} a -> s {rdsDbSnapshot = a} :: Configuration)

-- | The access control configuration is for an Amazon RDS DB cluster
-- snapshot.
configuration_rdsDbClusterSnapshot :: Lens.Lens' Configuration (Prelude.Maybe RdsDbClusterSnapshotConfiguration)
configuration_rdsDbClusterSnapshot = Lens.lens (\Configuration' {rdsDbClusterSnapshot} -> rdsDbClusterSnapshot) (\s@Configuration' {} a -> s {rdsDbClusterSnapshot = a} :: Configuration)

-- | The access control configuration is for an Amazon EFS file system.
configuration_efsFileSystem :: Lens.Lens' Configuration (Prelude.Maybe EfsFileSystemConfiguration)
configuration_efsFileSystem = Lens.lens (\Configuration' {efsFileSystem} -> efsFileSystem) (\s@Configuration' {} a -> s {efsFileSystem = a} :: Configuration)

-- | The access control configuration is for an Amazon SQS queue.
configuration_sqsQueue :: Lens.Lens' Configuration (Prelude.Maybe SqsQueueConfiguration)
configuration_sqsQueue = Lens.lens (\Configuration' {sqsQueue} -> sqsQueue) (\s@Configuration' {} a -> s {sqsQueue = a} :: Configuration)

-- | The access control configuration is for a Secrets Manager secret.
configuration_secretsManagerSecret :: Lens.Lens' Configuration (Prelude.Maybe SecretsManagerSecretConfiguration)
configuration_secretsManagerSecret = Lens.lens (\Configuration' {secretsManagerSecret} -> secretsManagerSecret) (\s@Configuration' {} a -> s {secretsManagerSecret = a} :: Configuration)

-- | The access control configuration is for an Amazon EBS volume snapshot.
configuration_ebsSnapshot :: Lens.Lens' Configuration (Prelude.Maybe EbsSnapshotConfiguration)
configuration_ebsSnapshot = Lens.lens (\Configuration' {ebsSnapshot} -> ebsSnapshot) (\s@Configuration' {} a -> s {ebsSnapshot = a} :: Configuration)

instance Data.FromJSON Configuration where
  parseJSON =
    Data.withObject
      "Configuration"
      ( \x ->
          Configuration'
            Prelude.<$> (x Data..:? "ecrRepository")
            Prelude.<*> (x Data..:? "s3Bucket")
            Prelude.<*> (x Data..:? "iamRole")
            Prelude.<*> (x Data..:? "snsTopic")
            Prelude.<*> (x Data..:? "kmsKey")
            Prelude.<*> (x Data..:? "rdsDbSnapshot")
            Prelude.<*> (x Data..:? "rdsDbClusterSnapshot")
            Prelude.<*> (x Data..:? "efsFileSystem")
            Prelude.<*> (x Data..:? "sqsQueue")
            Prelude.<*> (x Data..:? "secretsManagerSecret")
            Prelude.<*> (x Data..:? "ebsSnapshot")
      )

instance Prelude.Hashable Configuration where
  hashWithSalt _salt Configuration' {..} =
    _salt `Prelude.hashWithSalt` ecrRepository
      `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` iamRole
      `Prelude.hashWithSalt` snsTopic
      `Prelude.hashWithSalt` kmsKey
      `Prelude.hashWithSalt` rdsDbSnapshot
      `Prelude.hashWithSalt` rdsDbClusterSnapshot
      `Prelude.hashWithSalt` efsFileSystem
      `Prelude.hashWithSalt` sqsQueue
      `Prelude.hashWithSalt` secretsManagerSecret
      `Prelude.hashWithSalt` ebsSnapshot

instance Prelude.NFData Configuration where
  rnf Configuration' {..} =
    Prelude.rnf ecrRepository
      `Prelude.seq` Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf iamRole
      `Prelude.seq` Prelude.rnf snsTopic
      `Prelude.seq` Prelude.rnf kmsKey
      `Prelude.seq` Prelude.rnf rdsDbSnapshot
      `Prelude.seq` Prelude.rnf rdsDbClusterSnapshot
      `Prelude.seq` Prelude.rnf efsFileSystem
      `Prelude.seq` Prelude.rnf sqsQueue
      `Prelude.seq` Prelude.rnf secretsManagerSecret
      `Prelude.seq` Prelude.rnf ebsSnapshot

instance Data.ToJSON Configuration where
  toJSON Configuration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ecrRepository" Data..=) Prelude.<$> ecrRepository,
            ("s3Bucket" Data..=) Prelude.<$> s3Bucket,
            ("iamRole" Data..=) Prelude.<$> iamRole,
            ("snsTopic" Data..=) Prelude.<$> snsTopic,
            ("kmsKey" Data..=) Prelude.<$> kmsKey,
            ("rdsDbSnapshot" Data..=) Prelude.<$> rdsDbSnapshot,
            ("rdsDbClusterSnapshot" Data..=)
              Prelude.<$> rdsDbClusterSnapshot,
            ("efsFileSystem" Data..=) Prelude.<$> efsFileSystem,
            ("sqsQueue" Data..=) Prelude.<$> sqsQueue,
            ("secretsManagerSecret" Data..=)
              Prelude.<$> secretsManagerSecret,
            ("ebsSnapshot" Data..=) Prelude.<$> ebsSnapshot
          ]
      )
