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
-- Module      : Network.AWS.SageMaker.Types.RedshiftDatasetDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.RedshiftDatasetDefinition where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.RedshiftResultCompressionType
import Network.AWS.SageMaker.Types.RedshiftResultFormat

-- | Configuration for Redshift Dataset Definition input.
--
-- /See:/ 'newRedshiftDatasetDefinition' smart constructor.
data RedshiftDatasetDefinition = RedshiftDatasetDefinition'
  { outputCompression :: Core.Maybe RedshiftResultCompressionType,
    -- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
    -- to encrypt data from a Redshift execution.
    kmsKeyId :: Core.Maybe Core.Text,
    clusterId :: Core.Text,
    database :: Core.Text,
    dbUser :: Core.Text,
    queryString :: Core.Text,
    -- | The IAM role attached to your Redshift cluster that Amazon SageMaker
    -- uses to generate datasets.
    clusterRoleArn :: Core.Text,
    -- | The location in Amazon S3 where the Redshift query results are stored.
    outputS3Uri :: Core.Text,
    outputFormat :: RedshiftResultFormat
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RedshiftDatasetDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputCompression', 'redshiftDatasetDefinition_outputCompression' - Undocumented member.
--
-- 'kmsKeyId', 'redshiftDatasetDefinition_kmsKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
-- to encrypt data from a Redshift execution.
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
  Core.Text ->
  -- | 'database'
  Core.Text ->
  -- | 'dbUser'
  Core.Text ->
  -- | 'queryString'
  Core.Text ->
  -- | 'clusterRoleArn'
  Core.Text ->
  -- | 'outputS3Uri'
  Core.Text ->
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
      { outputCompression =
          Core.Nothing,
        kmsKeyId = Core.Nothing,
        clusterId = pClusterId_,
        database = pDatabase_,
        dbUser = pDbUser_,
        queryString = pQueryString_,
        clusterRoleArn = pClusterRoleArn_,
        outputS3Uri = pOutputS3Uri_,
        outputFormat = pOutputFormat_
      }

-- | Undocumented member.
redshiftDatasetDefinition_outputCompression :: Lens.Lens' RedshiftDatasetDefinition (Core.Maybe RedshiftResultCompressionType)
redshiftDatasetDefinition_outputCompression = Lens.lens (\RedshiftDatasetDefinition' {outputCompression} -> outputCompression) (\s@RedshiftDatasetDefinition' {} a -> s {outputCompression = a} :: RedshiftDatasetDefinition)

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
-- to encrypt data from a Redshift execution.
redshiftDatasetDefinition_kmsKeyId :: Lens.Lens' RedshiftDatasetDefinition (Core.Maybe Core.Text)
redshiftDatasetDefinition_kmsKeyId = Lens.lens (\RedshiftDatasetDefinition' {kmsKeyId} -> kmsKeyId) (\s@RedshiftDatasetDefinition' {} a -> s {kmsKeyId = a} :: RedshiftDatasetDefinition)

-- | Undocumented member.
redshiftDatasetDefinition_clusterId :: Lens.Lens' RedshiftDatasetDefinition Core.Text
redshiftDatasetDefinition_clusterId = Lens.lens (\RedshiftDatasetDefinition' {clusterId} -> clusterId) (\s@RedshiftDatasetDefinition' {} a -> s {clusterId = a} :: RedshiftDatasetDefinition)

-- | Undocumented member.
redshiftDatasetDefinition_database :: Lens.Lens' RedshiftDatasetDefinition Core.Text
redshiftDatasetDefinition_database = Lens.lens (\RedshiftDatasetDefinition' {database} -> database) (\s@RedshiftDatasetDefinition' {} a -> s {database = a} :: RedshiftDatasetDefinition)

-- | Undocumented member.
redshiftDatasetDefinition_dbUser :: Lens.Lens' RedshiftDatasetDefinition Core.Text
redshiftDatasetDefinition_dbUser = Lens.lens (\RedshiftDatasetDefinition' {dbUser} -> dbUser) (\s@RedshiftDatasetDefinition' {} a -> s {dbUser = a} :: RedshiftDatasetDefinition)

-- | Undocumented member.
redshiftDatasetDefinition_queryString :: Lens.Lens' RedshiftDatasetDefinition Core.Text
redshiftDatasetDefinition_queryString = Lens.lens (\RedshiftDatasetDefinition' {queryString} -> queryString) (\s@RedshiftDatasetDefinition' {} a -> s {queryString = a} :: RedshiftDatasetDefinition)

-- | The IAM role attached to your Redshift cluster that Amazon SageMaker
-- uses to generate datasets.
redshiftDatasetDefinition_clusterRoleArn :: Lens.Lens' RedshiftDatasetDefinition Core.Text
redshiftDatasetDefinition_clusterRoleArn = Lens.lens (\RedshiftDatasetDefinition' {clusterRoleArn} -> clusterRoleArn) (\s@RedshiftDatasetDefinition' {} a -> s {clusterRoleArn = a} :: RedshiftDatasetDefinition)

-- | The location in Amazon S3 where the Redshift query results are stored.
redshiftDatasetDefinition_outputS3Uri :: Lens.Lens' RedshiftDatasetDefinition Core.Text
redshiftDatasetDefinition_outputS3Uri = Lens.lens (\RedshiftDatasetDefinition' {outputS3Uri} -> outputS3Uri) (\s@RedshiftDatasetDefinition' {} a -> s {outputS3Uri = a} :: RedshiftDatasetDefinition)

-- | Undocumented member.
redshiftDatasetDefinition_outputFormat :: Lens.Lens' RedshiftDatasetDefinition RedshiftResultFormat
redshiftDatasetDefinition_outputFormat = Lens.lens (\RedshiftDatasetDefinition' {outputFormat} -> outputFormat) (\s@RedshiftDatasetDefinition' {} a -> s {outputFormat = a} :: RedshiftDatasetDefinition)

instance Core.FromJSON RedshiftDatasetDefinition where
  parseJSON =
    Core.withObject
      "RedshiftDatasetDefinition"
      ( \x ->
          RedshiftDatasetDefinition'
            Core.<$> (x Core..:? "OutputCompression")
            Core.<*> (x Core..:? "KmsKeyId")
            Core.<*> (x Core..: "ClusterId")
            Core.<*> (x Core..: "Database")
            Core.<*> (x Core..: "DbUser")
            Core.<*> (x Core..: "QueryString")
            Core.<*> (x Core..: "ClusterRoleArn")
            Core.<*> (x Core..: "OutputS3Uri")
            Core.<*> (x Core..: "OutputFormat")
      )

instance Core.Hashable RedshiftDatasetDefinition

instance Core.NFData RedshiftDatasetDefinition

instance Core.ToJSON RedshiftDatasetDefinition where
  toJSON RedshiftDatasetDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("OutputCompression" Core..=)
              Core.<$> outputCompression,
            ("KmsKeyId" Core..=) Core.<$> kmsKeyId,
            Core.Just ("ClusterId" Core..= clusterId),
            Core.Just ("Database" Core..= database),
            Core.Just ("DbUser" Core..= dbUser),
            Core.Just ("QueryString" Core..= queryString),
            Core.Just ("ClusterRoleArn" Core..= clusterRoleArn),
            Core.Just ("OutputS3Uri" Core..= outputS3Uri),
            Core.Just ("OutputFormat" Core..= outputFormat)
          ]
      )
