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
-- Module      : Amazonka.AppFlow.Types.RedshiftConnectorProfileProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.RedshiftConnectorProfileProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile properties when using Amazon Redshift.
--
-- /See:/ 'newRedshiftConnectorProfileProperties' smart constructor.
data RedshiftConnectorProfileProperties = RedshiftConnectorProfileProperties'
  { -- | The object key for the destination bucket in which Amazon AppFlow places
    -- the files.
    bucketPrefix :: Prelude.Maybe Prelude.Text,
    -- | The JDBC URL of the Amazon Redshift cluster.
    databaseUrl :: Prelude.Text,
    -- | A name for the associated Amazon S3 bucket.
    bucketName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RedshiftConnectorProfileProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketPrefix', 'redshiftConnectorProfileProperties_bucketPrefix' - The object key for the destination bucket in which Amazon AppFlow places
-- the files.
--
-- 'databaseUrl', 'redshiftConnectorProfileProperties_databaseUrl' - The JDBC URL of the Amazon Redshift cluster.
--
-- 'bucketName', 'redshiftConnectorProfileProperties_bucketName' - A name for the associated Amazon S3 bucket.
--
-- 'roleArn', 'redshiftConnectorProfileProperties_roleArn' - The Amazon Resource Name (ARN) of the IAM role.
newRedshiftConnectorProfileProperties ::
  -- | 'databaseUrl'
  Prelude.Text ->
  -- | 'bucketName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  RedshiftConnectorProfileProperties
newRedshiftConnectorProfileProperties
  pDatabaseUrl_
  pBucketName_
  pRoleArn_ =
    RedshiftConnectorProfileProperties'
      { bucketPrefix =
          Prelude.Nothing,
        databaseUrl = pDatabaseUrl_,
        bucketName = pBucketName_,
        roleArn = pRoleArn_
      }

-- | The object key for the destination bucket in which Amazon AppFlow places
-- the files.
redshiftConnectorProfileProperties_bucketPrefix :: Lens.Lens' RedshiftConnectorProfileProperties (Prelude.Maybe Prelude.Text)
redshiftConnectorProfileProperties_bucketPrefix = Lens.lens (\RedshiftConnectorProfileProperties' {bucketPrefix} -> bucketPrefix) (\s@RedshiftConnectorProfileProperties' {} a -> s {bucketPrefix = a} :: RedshiftConnectorProfileProperties)

-- | The JDBC URL of the Amazon Redshift cluster.
redshiftConnectorProfileProperties_databaseUrl :: Lens.Lens' RedshiftConnectorProfileProperties Prelude.Text
redshiftConnectorProfileProperties_databaseUrl = Lens.lens (\RedshiftConnectorProfileProperties' {databaseUrl} -> databaseUrl) (\s@RedshiftConnectorProfileProperties' {} a -> s {databaseUrl = a} :: RedshiftConnectorProfileProperties)

-- | A name for the associated Amazon S3 bucket.
redshiftConnectorProfileProperties_bucketName :: Lens.Lens' RedshiftConnectorProfileProperties Prelude.Text
redshiftConnectorProfileProperties_bucketName = Lens.lens (\RedshiftConnectorProfileProperties' {bucketName} -> bucketName) (\s@RedshiftConnectorProfileProperties' {} a -> s {bucketName = a} :: RedshiftConnectorProfileProperties)

-- | The Amazon Resource Name (ARN) of the IAM role.
redshiftConnectorProfileProperties_roleArn :: Lens.Lens' RedshiftConnectorProfileProperties Prelude.Text
redshiftConnectorProfileProperties_roleArn = Lens.lens (\RedshiftConnectorProfileProperties' {roleArn} -> roleArn) (\s@RedshiftConnectorProfileProperties' {} a -> s {roleArn = a} :: RedshiftConnectorProfileProperties)

instance
  Core.FromJSON
    RedshiftConnectorProfileProperties
  where
  parseJSON =
    Core.withObject
      "RedshiftConnectorProfileProperties"
      ( \x ->
          RedshiftConnectorProfileProperties'
            Prelude.<$> (x Core..:? "bucketPrefix")
            Prelude.<*> (x Core..: "databaseUrl")
            Prelude.<*> (x Core..: "bucketName")
            Prelude.<*> (x Core..: "roleArn")
      )

instance
  Prelude.Hashable
    RedshiftConnectorProfileProperties
  where
  hashWithSalt
    salt'
    RedshiftConnectorProfileProperties' {..} =
      salt' `Prelude.hashWithSalt` roleArn
        `Prelude.hashWithSalt` bucketName
        `Prelude.hashWithSalt` databaseUrl
        `Prelude.hashWithSalt` bucketPrefix

instance
  Prelude.NFData
    RedshiftConnectorProfileProperties
  where
  rnf RedshiftConnectorProfileProperties' {..} =
    Prelude.rnf bucketPrefix
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf databaseUrl

instance
  Core.ToJSON
    RedshiftConnectorProfileProperties
  where
  toJSON RedshiftConnectorProfileProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("bucketPrefix" Core..=) Prelude.<$> bucketPrefix,
            Prelude.Just ("databaseUrl" Core..= databaseUrl),
            Prelude.Just ("bucketName" Core..= bucketName),
            Prelude.Just ("roleArn" Core..= roleArn)
          ]
      )
