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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.RedshiftConnectorProfileProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile properties when using Amazon Redshift.
--
-- /See:/ 'newRedshiftConnectorProfileProperties' smart constructor.
data RedshiftConnectorProfileProperties = RedshiftConnectorProfileProperties'
  { -- | The object key for the destination bucket in which Amazon AppFlow places
    -- the files.
    bucketPrefix :: Prelude.Maybe Prelude.Text,
    -- | The unique ID that\'s assigned to an Amazon Redshift cluster.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an IAM role that permits Amazon
    -- AppFlow to access your Amazon Redshift database through the Data API.
    -- For more information, and for the polices that you attach to this role,
    -- see
    -- <https://docs.aws.amazon.com/appflow/latest/userguide/security_iam_service-role-policies.html#access-redshift Allow Amazon AppFlow to access Amazon Redshift databases with the Data API>.
    dataApiRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of an Amazon Redshift database.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The JDBC URL of the Amazon Redshift cluster.
    databaseUrl :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the connector profile defines a connection to an
    -- Amazon Redshift Serverless data warehouse.
    isRedshiftServerless :: Prelude.Maybe Prelude.Bool,
    -- | The name of an Amazon Redshift workgroup.
    workgroupName :: Prelude.Maybe Prelude.Text,
    -- | A name for the associated Amazon S3 bucket.
    bucketName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of IAM role that grants Amazon Redshift
    -- read-only access to Amazon S3. For more information, and for the polices
    -- that you attach to this role, see
    -- <https://docs.aws.amazon.com/appflow/latest/userguide/security_iam_service-role-policies.html#redshift-access-s3 Allow Amazon Redshift to access your Amazon AppFlow data in Amazon S3>.
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
-- 'clusterIdentifier', 'redshiftConnectorProfileProperties_clusterIdentifier' - The unique ID that\'s assigned to an Amazon Redshift cluster.
--
-- 'dataApiRoleArn', 'redshiftConnectorProfileProperties_dataApiRoleArn' - The Amazon Resource Name (ARN) of an IAM role that permits Amazon
-- AppFlow to access your Amazon Redshift database through the Data API.
-- For more information, and for the polices that you attach to this role,
-- see
-- <https://docs.aws.amazon.com/appflow/latest/userguide/security_iam_service-role-policies.html#access-redshift Allow Amazon AppFlow to access Amazon Redshift databases with the Data API>.
--
-- 'databaseName', 'redshiftConnectorProfileProperties_databaseName' - The name of an Amazon Redshift database.
--
-- 'databaseUrl', 'redshiftConnectorProfileProperties_databaseUrl' - The JDBC URL of the Amazon Redshift cluster.
--
-- 'isRedshiftServerless', 'redshiftConnectorProfileProperties_isRedshiftServerless' - Indicates whether the connector profile defines a connection to an
-- Amazon Redshift Serverless data warehouse.
--
-- 'workgroupName', 'redshiftConnectorProfileProperties_workgroupName' - The name of an Amazon Redshift workgroup.
--
-- 'bucketName', 'redshiftConnectorProfileProperties_bucketName' - A name for the associated Amazon S3 bucket.
--
-- 'roleArn', 'redshiftConnectorProfileProperties_roleArn' - The Amazon Resource Name (ARN) of IAM role that grants Amazon Redshift
-- read-only access to Amazon S3. For more information, and for the polices
-- that you attach to this role, see
-- <https://docs.aws.amazon.com/appflow/latest/userguide/security_iam_service-role-policies.html#redshift-access-s3 Allow Amazon Redshift to access your Amazon AppFlow data in Amazon S3>.
newRedshiftConnectorProfileProperties ::
  -- | 'bucketName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  RedshiftConnectorProfileProperties
newRedshiftConnectorProfileProperties
  pBucketName_
  pRoleArn_ =
    RedshiftConnectorProfileProperties'
      { bucketPrefix =
          Prelude.Nothing,
        clusterIdentifier = Prelude.Nothing,
        dataApiRoleArn = Prelude.Nothing,
        databaseName = Prelude.Nothing,
        databaseUrl = Prelude.Nothing,
        isRedshiftServerless = Prelude.Nothing,
        workgroupName = Prelude.Nothing,
        bucketName = pBucketName_,
        roleArn = pRoleArn_
      }

-- | The object key for the destination bucket in which Amazon AppFlow places
-- the files.
redshiftConnectorProfileProperties_bucketPrefix :: Lens.Lens' RedshiftConnectorProfileProperties (Prelude.Maybe Prelude.Text)
redshiftConnectorProfileProperties_bucketPrefix = Lens.lens (\RedshiftConnectorProfileProperties' {bucketPrefix} -> bucketPrefix) (\s@RedshiftConnectorProfileProperties' {} a -> s {bucketPrefix = a} :: RedshiftConnectorProfileProperties)

-- | The unique ID that\'s assigned to an Amazon Redshift cluster.
redshiftConnectorProfileProperties_clusterIdentifier :: Lens.Lens' RedshiftConnectorProfileProperties (Prelude.Maybe Prelude.Text)
redshiftConnectorProfileProperties_clusterIdentifier = Lens.lens (\RedshiftConnectorProfileProperties' {clusterIdentifier} -> clusterIdentifier) (\s@RedshiftConnectorProfileProperties' {} a -> s {clusterIdentifier = a} :: RedshiftConnectorProfileProperties)

-- | The Amazon Resource Name (ARN) of an IAM role that permits Amazon
-- AppFlow to access your Amazon Redshift database through the Data API.
-- For more information, and for the polices that you attach to this role,
-- see
-- <https://docs.aws.amazon.com/appflow/latest/userguide/security_iam_service-role-policies.html#access-redshift Allow Amazon AppFlow to access Amazon Redshift databases with the Data API>.
redshiftConnectorProfileProperties_dataApiRoleArn :: Lens.Lens' RedshiftConnectorProfileProperties (Prelude.Maybe Prelude.Text)
redshiftConnectorProfileProperties_dataApiRoleArn = Lens.lens (\RedshiftConnectorProfileProperties' {dataApiRoleArn} -> dataApiRoleArn) (\s@RedshiftConnectorProfileProperties' {} a -> s {dataApiRoleArn = a} :: RedshiftConnectorProfileProperties)

-- | The name of an Amazon Redshift database.
redshiftConnectorProfileProperties_databaseName :: Lens.Lens' RedshiftConnectorProfileProperties (Prelude.Maybe Prelude.Text)
redshiftConnectorProfileProperties_databaseName = Lens.lens (\RedshiftConnectorProfileProperties' {databaseName} -> databaseName) (\s@RedshiftConnectorProfileProperties' {} a -> s {databaseName = a} :: RedshiftConnectorProfileProperties)

-- | The JDBC URL of the Amazon Redshift cluster.
redshiftConnectorProfileProperties_databaseUrl :: Lens.Lens' RedshiftConnectorProfileProperties (Prelude.Maybe Prelude.Text)
redshiftConnectorProfileProperties_databaseUrl = Lens.lens (\RedshiftConnectorProfileProperties' {databaseUrl} -> databaseUrl) (\s@RedshiftConnectorProfileProperties' {} a -> s {databaseUrl = a} :: RedshiftConnectorProfileProperties)

-- | Indicates whether the connector profile defines a connection to an
-- Amazon Redshift Serverless data warehouse.
redshiftConnectorProfileProperties_isRedshiftServerless :: Lens.Lens' RedshiftConnectorProfileProperties (Prelude.Maybe Prelude.Bool)
redshiftConnectorProfileProperties_isRedshiftServerless = Lens.lens (\RedshiftConnectorProfileProperties' {isRedshiftServerless} -> isRedshiftServerless) (\s@RedshiftConnectorProfileProperties' {} a -> s {isRedshiftServerless = a} :: RedshiftConnectorProfileProperties)

-- | The name of an Amazon Redshift workgroup.
redshiftConnectorProfileProperties_workgroupName :: Lens.Lens' RedshiftConnectorProfileProperties (Prelude.Maybe Prelude.Text)
redshiftConnectorProfileProperties_workgroupName = Lens.lens (\RedshiftConnectorProfileProperties' {workgroupName} -> workgroupName) (\s@RedshiftConnectorProfileProperties' {} a -> s {workgroupName = a} :: RedshiftConnectorProfileProperties)

-- | A name for the associated Amazon S3 bucket.
redshiftConnectorProfileProperties_bucketName :: Lens.Lens' RedshiftConnectorProfileProperties Prelude.Text
redshiftConnectorProfileProperties_bucketName = Lens.lens (\RedshiftConnectorProfileProperties' {bucketName} -> bucketName) (\s@RedshiftConnectorProfileProperties' {} a -> s {bucketName = a} :: RedshiftConnectorProfileProperties)

-- | The Amazon Resource Name (ARN) of IAM role that grants Amazon Redshift
-- read-only access to Amazon S3. For more information, and for the polices
-- that you attach to this role, see
-- <https://docs.aws.amazon.com/appflow/latest/userguide/security_iam_service-role-policies.html#redshift-access-s3 Allow Amazon Redshift to access your Amazon AppFlow data in Amazon S3>.
redshiftConnectorProfileProperties_roleArn :: Lens.Lens' RedshiftConnectorProfileProperties Prelude.Text
redshiftConnectorProfileProperties_roleArn = Lens.lens (\RedshiftConnectorProfileProperties' {roleArn} -> roleArn) (\s@RedshiftConnectorProfileProperties' {} a -> s {roleArn = a} :: RedshiftConnectorProfileProperties)

instance
  Data.FromJSON
    RedshiftConnectorProfileProperties
  where
  parseJSON =
    Data.withObject
      "RedshiftConnectorProfileProperties"
      ( \x ->
          RedshiftConnectorProfileProperties'
            Prelude.<$> (x Data..:? "bucketPrefix")
            Prelude.<*> (x Data..:? "clusterIdentifier")
            Prelude.<*> (x Data..:? "dataApiRoleArn")
            Prelude.<*> (x Data..:? "databaseName")
            Prelude.<*> (x Data..:? "databaseUrl")
            Prelude.<*> (x Data..:? "isRedshiftServerless")
            Prelude.<*> (x Data..:? "workgroupName")
            Prelude.<*> (x Data..: "bucketName")
            Prelude.<*> (x Data..: "roleArn")
      )

instance
  Prelude.Hashable
    RedshiftConnectorProfileProperties
  where
  hashWithSalt
    _salt
    RedshiftConnectorProfileProperties' {..} =
      _salt
        `Prelude.hashWithSalt` bucketPrefix
        `Prelude.hashWithSalt` clusterIdentifier
        `Prelude.hashWithSalt` dataApiRoleArn
        `Prelude.hashWithSalt` databaseName
        `Prelude.hashWithSalt` databaseUrl
        `Prelude.hashWithSalt` isRedshiftServerless
        `Prelude.hashWithSalt` workgroupName
        `Prelude.hashWithSalt` bucketName
        `Prelude.hashWithSalt` roleArn

instance
  Prelude.NFData
    RedshiftConnectorProfileProperties
  where
  rnf RedshiftConnectorProfileProperties' {..} =
    Prelude.rnf bucketPrefix
      `Prelude.seq` Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf dataApiRoleArn
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf databaseUrl
      `Prelude.seq` Prelude.rnf isRedshiftServerless
      `Prelude.seq` Prelude.rnf workgroupName
      `Prelude.seq` Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf roleArn

instance
  Data.ToJSON
    RedshiftConnectorProfileProperties
  where
  toJSON RedshiftConnectorProfileProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bucketPrefix" Data..=) Prelude.<$> bucketPrefix,
            ("clusterIdentifier" Data..=)
              Prelude.<$> clusterIdentifier,
            ("dataApiRoleArn" Data..=)
              Prelude.<$> dataApiRoleArn,
            ("databaseName" Data..=) Prelude.<$> databaseName,
            ("databaseUrl" Data..=) Prelude.<$> databaseUrl,
            ("isRedshiftServerless" Data..=)
              Prelude.<$> isRedshiftServerless,
            ("workgroupName" Data..=) Prelude.<$> workgroupName,
            Prelude.Just ("bucketName" Data..= bucketName),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )
