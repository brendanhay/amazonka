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
-- Module      : Amazonka.SageMaker.Types.RedshiftDatasetDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.RedshiftDatasetDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.RedshiftResultCompressionType
import Amazonka.SageMaker.Types.RedshiftResultFormat

-- | Configuration for Redshift Dataset Definition input.
--
-- /See:/ 'newRedshiftDatasetDefinition' smart constructor.
data RedshiftDatasetDefinition = RedshiftDatasetDefinition'
  { -- | The Amazon Web Services Key Management Service (Amazon Web Services KMS)
    -- key that Amazon SageMaker uses to encrypt data from a Redshift
    -- execution.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    outputCompression :: Prelude.Maybe RedshiftResultCompressionType,
    clusterId :: Prelude.Text,
    database :: Prelude.Text,
    dbUser :: Prelude.Text,
    queryString :: Prelude.Text,
    -- | The IAM role attached to your Redshift cluster that Amazon SageMaker
    -- uses to generate datasets.
    clusterRoleArn :: Prelude.Text,
    -- | The location in Amazon S3 where the Redshift query results are stored.
    outputS3Uri :: Prelude.Text,
    outputFormat :: RedshiftResultFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RedshiftDatasetDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'redshiftDatasetDefinition_kmsKeyId' - The Amazon Web Services Key Management Service (Amazon Web Services KMS)
-- key that Amazon SageMaker uses to encrypt data from a Redshift
-- execution.
--
-- 'outputCompression', 'redshiftDatasetDefinition_outputCompression' - Undocumented member.
--
-- 'clusterId', 'redshiftDatasetDefinition_clusterId' - Undocumented member.
--
-- 'database', 'redshiftDatasetDefinition_database' - Undocumented member.
--
-- 'dbUser', 'redshiftDatasetDefinition_dbUser' - Undocumented member.
--
-- 'queryString', 'redshiftDatasetDefinition_queryString' - Undocumented member.
--
-- 'clusterRoleArn', 'redshiftDatasetDefinition_clusterRoleArn' - The IAM role attached to your Redshift cluster that Amazon SageMaker
-- uses to generate datasets.
--
-- 'outputS3Uri', 'redshiftDatasetDefinition_outputS3Uri' - The location in Amazon S3 where the Redshift query results are stored.
--
-- 'outputFormat', 'redshiftDatasetDefinition_outputFormat' - Undocumented member.
newRedshiftDatasetDefinition ::
  -- | 'clusterId'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  -- | 'dbUser'
  Prelude.Text ->
  -- | 'queryString'
  Prelude.Text ->
  -- | 'clusterRoleArn'
  Prelude.Text ->
  -- | 'outputS3Uri'
  Prelude.Text ->
  -- | 'outputFormat'
  RedshiftResultFormat ->
  RedshiftDatasetDefinition
newRedshiftDatasetDefinition
  pClusterId_
  pDatabase_
  pDbUser_
  pQueryString_
  pClusterRoleArn_
  pOutputS3Uri_
  pOutputFormat_ =
    RedshiftDatasetDefinition'
      { kmsKeyId =
          Prelude.Nothing,
        outputCompression = Prelude.Nothing,
        clusterId = pClusterId_,
        database = pDatabase_,
        dbUser = pDbUser_,
        queryString = pQueryString_,
        clusterRoleArn = pClusterRoleArn_,
        outputS3Uri = pOutputS3Uri_,
        outputFormat = pOutputFormat_
      }

-- | The Amazon Web Services Key Management Service (Amazon Web Services KMS)
-- key that Amazon SageMaker uses to encrypt data from a Redshift
-- execution.
redshiftDatasetDefinition_kmsKeyId :: Lens.Lens' RedshiftDatasetDefinition (Prelude.Maybe Prelude.Text)
redshiftDatasetDefinition_kmsKeyId = Lens.lens (\RedshiftDatasetDefinition' {kmsKeyId} -> kmsKeyId) (\s@RedshiftDatasetDefinition' {} a -> s {kmsKeyId = a} :: RedshiftDatasetDefinition)

-- | Undocumented member.
redshiftDatasetDefinition_outputCompression :: Lens.Lens' RedshiftDatasetDefinition (Prelude.Maybe RedshiftResultCompressionType)
redshiftDatasetDefinition_outputCompression = Lens.lens (\RedshiftDatasetDefinition' {outputCompression} -> outputCompression) (\s@RedshiftDatasetDefinition' {} a -> s {outputCompression = a} :: RedshiftDatasetDefinition)

-- | Undocumented member.
redshiftDatasetDefinition_clusterId :: Lens.Lens' RedshiftDatasetDefinition Prelude.Text
redshiftDatasetDefinition_clusterId = Lens.lens (\RedshiftDatasetDefinition' {clusterId} -> clusterId) (\s@RedshiftDatasetDefinition' {} a -> s {clusterId = a} :: RedshiftDatasetDefinition)

-- | Undocumented member.
redshiftDatasetDefinition_database :: Lens.Lens' RedshiftDatasetDefinition Prelude.Text
redshiftDatasetDefinition_database = Lens.lens (\RedshiftDatasetDefinition' {database} -> database) (\s@RedshiftDatasetDefinition' {} a -> s {database = a} :: RedshiftDatasetDefinition)

-- | Undocumented member.
redshiftDatasetDefinition_dbUser :: Lens.Lens' RedshiftDatasetDefinition Prelude.Text
redshiftDatasetDefinition_dbUser = Lens.lens (\RedshiftDatasetDefinition' {dbUser} -> dbUser) (\s@RedshiftDatasetDefinition' {} a -> s {dbUser = a} :: RedshiftDatasetDefinition)

-- | Undocumented member.
redshiftDatasetDefinition_queryString :: Lens.Lens' RedshiftDatasetDefinition Prelude.Text
redshiftDatasetDefinition_queryString = Lens.lens (\RedshiftDatasetDefinition' {queryString} -> queryString) (\s@RedshiftDatasetDefinition' {} a -> s {queryString = a} :: RedshiftDatasetDefinition)

-- | The IAM role attached to your Redshift cluster that Amazon SageMaker
-- uses to generate datasets.
redshiftDatasetDefinition_clusterRoleArn :: Lens.Lens' RedshiftDatasetDefinition Prelude.Text
redshiftDatasetDefinition_clusterRoleArn = Lens.lens (\RedshiftDatasetDefinition' {clusterRoleArn} -> clusterRoleArn) (\s@RedshiftDatasetDefinition' {} a -> s {clusterRoleArn = a} :: RedshiftDatasetDefinition)

-- | The location in Amazon S3 where the Redshift query results are stored.
redshiftDatasetDefinition_outputS3Uri :: Lens.Lens' RedshiftDatasetDefinition Prelude.Text
redshiftDatasetDefinition_outputS3Uri = Lens.lens (\RedshiftDatasetDefinition' {outputS3Uri} -> outputS3Uri) (\s@RedshiftDatasetDefinition' {} a -> s {outputS3Uri = a} :: RedshiftDatasetDefinition)

-- | Undocumented member.
redshiftDatasetDefinition_outputFormat :: Lens.Lens' RedshiftDatasetDefinition RedshiftResultFormat
redshiftDatasetDefinition_outputFormat = Lens.lens (\RedshiftDatasetDefinition' {outputFormat} -> outputFormat) (\s@RedshiftDatasetDefinition' {} a -> s {outputFormat = a} :: RedshiftDatasetDefinition)

instance Data.FromJSON RedshiftDatasetDefinition where
  parseJSON =
    Data.withObject
      "RedshiftDatasetDefinition"
      ( \x ->
          RedshiftDatasetDefinition'
            Prelude.<$> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "OutputCompression")
            Prelude.<*> (x Data..: "ClusterId")
            Prelude.<*> (x Data..: "Database")
            Prelude.<*> (x Data..: "DbUser")
            Prelude.<*> (x Data..: "QueryString")
            Prelude.<*> (x Data..: "ClusterRoleArn")
            Prelude.<*> (x Data..: "OutputS3Uri")
            Prelude.<*> (x Data..: "OutputFormat")
      )

instance Prelude.Hashable RedshiftDatasetDefinition where
  hashWithSalt _salt RedshiftDatasetDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` outputCompression
      `Prelude.hashWithSalt` clusterId
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` dbUser
      `Prelude.hashWithSalt` queryString
      `Prelude.hashWithSalt` clusterRoleArn
      `Prelude.hashWithSalt` outputS3Uri
      `Prelude.hashWithSalt` outputFormat

instance Prelude.NFData RedshiftDatasetDefinition where
  rnf RedshiftDatasetDefinition' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf outputCompression
      `Prelude.seq` Prelude.rnf clusterId
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf dbUser
      `Prelude.seq` Prelude.rnf queryString
      `Prelude.seq` Prelude.rnf clusterRoleArn
      `Prelude.seq` Prelude.rnf outputS3Uri
      `Prelude.seq` Prelude.rnf outputFormat

instance Data.ToJSON RedshiftDatasetDefinition where
  toJSON RedshiftDatasetDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("OutputCompression" Data..=)
              Prelude.<$> outputCompression,
            Prelude.Just ("ClusterId" Data..= clusterId),
            Prelude.Just ("Database" Data..= database),
            Prelude.Just ("DbUser" Data..= dbUser),
            Prelude.Just ("QueryString" Data..= queryString),
            Prelude.Just
              ("ClusterRoleArn" Data..= clusterRoleArn),
            Prelude.Just ("OutputS3Uri" Data..= outputS3Uri),
            Prelude.Just ("OutputFormat" Data..= outputFormat)
          ]
      )
