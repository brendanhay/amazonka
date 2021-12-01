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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.Configuration where

import Amazonka.AccessAnalyzer.Types.IamRoleConfiguration
import Amazonka.AccessAnalyzer.Types.KmsKeyConfiguration
import Amazonka.AccessAnalyzer.Types.S3BucketConfiguration
import Amazonka.AccessAnalyzer.Types.SecretsManagerSecretConfiguration
import Amazonka.AccessAnalyzer.Types.SqsQueueConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Access control configuration structures for your resource. You specify
-- the configuration as a type-value pair. You can specify only one type of
-- access control configuration.
--
-- /See:/ 'newConfiguration' smart constructor.
data Configuration = Configuration'
  { -- | The access control configuration is for a KMS key.
    kmsKey :: Prelude.Maybe KmsKeyConfiguration,
    -- | The access control configuration is for a Secrets Manager secret.
    secretsManagerSecret :: Prelude.Maybe SecretsManagerSecretConfiguration,
    -- | The access control configuration is for an Amazon SQS queue.
    sqsQueue :: Prelude.Maybe SqsQueueConfiguration,
    -- | The access control configuration is for an Amazon S3 Bucket.
    s3Bucket :: Prelude.Maybe S3BucketConfiguration,
    -- | The access control configuration is for an IAM role.
    iamRole :: Prelude.Maybe IamRoleConfiguration
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
-- 'kmsKey', 'configuration_kmsKey' - The access control configuration is for a KMS key.
--
-- 'secretsManagerSecret', 'configuration_secretsManagerSecret' - The access control configuration is for a Secrets Manager secret.
--
-- 'sqsQueue', 'configuration_sqsQueue' - The access control configuration is for an Amazon SQS queue.
--
-- 's3Bucket', 'configuration_s3Bucket' - The access control configuration is for an Amazon S3 Bucket.
--
-- 'iamRole', 'configuration_iamRole' - The access control configuration is for an IAM role.
newConfiguration ::
  Configuration
newConfiguration =
  Configuration'
    { kmsKey = Prelude.Nothing,
      secretsManagerSecret = Prelude.Nothing,
      sqsQueue = Prelude.Nothing,
      s3Bucket = Prelude.Nothing,
      iamRole = Prelude.Nothing
    }

-- | The access control configuration is for a KMS key.
configuration_kmsKey :: Lens.Lens' Configuration (Prelude.Maybe KmsKeyConfiguration)
configuration_kmsKey = Lens.lens (\Configuration' {kmsKey} -> kmsKey) (\s@Configuration' {} a -> s {kmsKey = a} :: Configuration)

-- | The access control configuration is for a Secrets Manager secret.
configuration_secretsManagerSecret :: Lens.Lens' Configuration (Prelude.Maybe SecretsManagerSecretConfiguration)
configuration_secretsManagerSecret = Lens.lens (\Configuration' {secretsManagerSecret} -> secretsManagerSecret) (\s@Configuration' {} a -> s {secretsManagerSecret = a} :: Configuration)

-- | The access control configuration is for an Amazon SQS queue.
configuration_sqsQueue :: Lens.Lens' Configuration (Prelude.Maybe SqsQueueConfiguration)
configuration_sqsQueue = Lens.lens (\Configuration' {sqsQueue} -> sqsQueue) (\s@Configuration' {} a -> s {sqsQueue = a} :: Configuration)

-- | The access control configuration is for an Amazon S3 Bucket.
configuration_s3Bucket :: Lens.Lens' Configuration (Prelude.Maybe S3BucketConfiguration)
configuration_s3Bucket = Lens.lens (\Configuration' {s3Bucket} -> s3Bucket) (\s@Configuration' {} a -> s {s3Bucket = a} :: Configuration)

-- | The access control configuration is for an IAM role.
configuration_iamRole :: Lens.Lens' Configuration (Prelude.Maybe IamRoleConfiguration)
configuration_iamRole = Lens.lens (\Configuration' {iamRole} -> iamRole) (\s@Configuration' {} a -> s {iamRole = a} :: Configuration)

instance Core.FromJSON Configuration where
  parseJSON =
    Core.withObject
      "Configuration"
      ( \x ->
          Configuration'
            Prelude.<$> (x Core..:? "kmsKey")
            Prelude.<*> (x Core..:? "secretsManagerSecret")
            Prelude.<*> (x Core..:? "sqsQueue")
            Prelude.<*> (x Core..:? "s3Bucket")
            Prelude.<*> (x Core..:? "iamRole")
      )

instance Prelude.Hashable Configuration where
  hashWithSalt salt' Configuration' {..} =
    salt' `Prelude.hashWithSalt` iamRole
      `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` sqsQueue
      `Prelude.hashWithSalt` secretsManagerSecret
      `Prelude.hashWithSalt` kmsKey

instance Prelude.NFData Configuration where
  rnf Configuration' {..} =
    Prelude.rnf kmsKey
      `Prelude.seq` Prelude.rnf iamRole
      `Prelude.seq` Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf sqsQueue
      `Prelude.seq` Prelude.rnf secretsManagerSecret

instance Core.ToJSON Configuration where
  toJSON Configuration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("kmsKey" Core..=) Prelude.<$> kmsKey,
            ("secretsManagerSecret" Core..=)
              Prelude.<$> secretsManagerSecret,
            ("sqsQueue" Core..=) Prelude.<$> sqsQueue,
            ("s3Bucket" Core..=) Prelude.<$> s3Bucket,
            ("iamRole" Core..=) Prelude.<$> iamRole
          ]
      )
