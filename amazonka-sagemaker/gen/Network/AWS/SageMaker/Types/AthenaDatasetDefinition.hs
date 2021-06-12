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
-- Module      : Network.AWS.SageMaker.Types.AthenaDatasetDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AthenaDatasetDefinition where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.AthenaResultCompressionType
import Network.AWS.SageMaker.Types.AthenaResultFormat

-- | Configuration for Athena Dataset Definition input.
--
-- /See:/ 'newAthenaDatasetDefinition' smart constructor.
data AthenaDatasetDefinition = AthenaDatasetDefinition'
  { outputCompression :: Core.Maybe AthenaResultCompressionType,
    -- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
    -- to encrypt data generated from an Athena query execution.
    kmsKeyId :: Core.Maybe Core.Text,
    workGroup :: Core.Maybe Core.Text,
    catalog :: Core.Text,
    database :: Core.Text,
    queryString :: Core.Text,
    -- | The location in Amazon S3 where Athena query results are stored.
    outputS3Uri :: Core.Text,
    outputFormat :: AthenaResultFormat
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AthenaDatasetDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputCompression', 'athenaDatasetDefinition_outputCompression' - Undocumented member.
--
-- 'kmsKeyId', 'athenaDatasetDefinition_kmsKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
-- to encrypt data generated from an Athena query execution.
--
-- 'workGroup', 'athenaDatasetDefinition_workGroup' - Undocumented member.
--
-- 'catalog', 'athenaDatasetDefinition_catalog' - Undocumented member.
--
-- 'database', 'athenaDatasetDefinition_database' - Undocumented member.
--
-- 'queryString', 'athenaDatasetDefinition_queryString' - Undocumented member.
--
-- 'outputS3Uri', 'athenaDatasetDefinition_outputS3Uri' - The location in Amazon S3 where Athena query results are stored.
--
-- 'outputFormat', 'athenaDatasetDefinition_outputFormat' - Undocumented member.
newAthenaDatasetDefinition ::
  -- | 'catalog'
  Core.Text ->
  -- | 'database'
  Core.Text ->
  -- | 'queryString'
  Core.Text ->
  -- | 'outputS3Uri'
  Core.Text ->
  -- | 'outputFormat'
  AthenaResultFormat ->
  AthenaDatasetDefinition
newAthenaDatasetDefinition
  pCatalog_
  pDatabase_
  pQueryString_
  pOutputS3Uri_
  pOutputFormat_ =
    AthenaDatasetDefinition'
      { outputCompression =
          Core.Nothing,
        kmsKeyId = Core.Nothing,
        workGroup = Core.Nothing,
        catalog = pCatalog_,
        database = pDatabase_,
        queryString = pQueryString_,
        outputS3Uri = pOutputS3Uri_,
        outputFormat = pOutputFormat_
      }

-- | Undocumented member.
athenaDatasetDefinition_outputCompression :: Lens.Lens' AthenaDatasetDefinition (Core.Maybe AthenaResultCompressionType)
athenaDatasetDefinition_outputCompression = Lens.lens (\AthenaDatasetDefinition' {outputCompression} -> outputCompression) (\s@AthenaDatasetDefinition' {} a -> s {outputCompression = a} :: AthenaDatasetDefinition)

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
-- to encrypt data generated from an Athena query execution.
athenaDatasetDefinition_kmsKeyId :: Lens.Lens' AthenaDatasetDefinition (Core.Maybe Core.Text)
athenaDatasetDefinition_kmsKeyId = Lens.lens (\AthenaDatasetDefinition' {kmsKeyId} -> kmsKeyId) (\s@AthenaDatasetDefinition' {} a -> s {kmsKeyId = a} :: AthenaDatasetDefinition)

-- | Undocumented member.
athenaDatasetDefinition_workGroup :: Lens.Lens' AthenaDatasetDefinition (Core.Maybe Core.Text)
athenaDatasetDefinition_workGroup = Lens.lens (\AthenaDatasetDefinition' {workGroup} -> workGroup) (\s@AthenaDatasetDefinition' {} a -> s {workGroup = a} :: AthenaDatasetDefinition)

-- | Undocumented member.
athenaDatasetDefinition_catalog :: Lens.Lens' AthenaDatasetDefinition Core.Text
athenaDatasetDefinition_catalog = Lens.lens (\AthenaDatasetDefinition' {catalog} -> catalog) (\s@AthenaDatasetDefinition' {} a -> s {catalog = a} :: AthenaDatasetDefinition)

-- | Undocumented member.
athenaDatasetDefinition_database :: Lens.Lens' AthenaDatasetDefinition Core.Text
athenaDatasetDefinition_database = Lens.lens (\AthenaDatasetDefinition' {database} -> database) (\s@AthenaDatasetDefinition' {} a -> s {database = a} :: AthenaDatasetDefinition)

-- | Undocumented member.
athenaDatasetDefinition_queryString :: Lens.Lens' AthenaDatasetDefinition Core.Text
athenaDatasetDefinition_queryString = Lens.lens (\AthenaDatasetDefinition' {queryString} -> queryString) (\s@AthenaDatasetDefinition' {} a -> s {queryString = a} :: AthenaDatasetDefinition)

-- | The location in Amazon S3 where Athena query results are stored.
athenaDatasetDefinition_outputS3Uri :: Lens.Lens' AthenaDatasetDefinition Core.Text
athenaDatasetDefinition_outputS3Uri = Lens.lens (\AthenaDatasetDefinition' {outputS3Uri} -> outputS3Uri) (\s@AthenaDatasetDefinition' {} a -> s {outputS3Uri = a} :: AthenaDatasetDefinition)

-- | Undocumented member.
athenaDatasetDefinition_outputFormat :: Lens.Lens' AthenaDatasetDefinition AthenaResultFormat
athenaDatasetDefinition_outputFormat = Lens.lens (\AthenaDatasetDefinition' {outputFormat} -> outputFormat) (\s@AthenaDatasetDefinition' {} a -> s {outputFormat = a} :: AthenaDatasetDefinition)

instance Core.FromJSON AthenaDatasetDefinition where
  parseJSON =
    Core.withObject
      "AthenaDatasetDefinition"
      ( \x ->
          AthenaDatasetDefinition'
            Core.<$> (x Core..:? "OutputCompression")
            Core.<*> (x Core..:? "KmsKeyId")
            Core.<*> (x Core..:? "WorkGroup")
            Core.<*> (x Core..: "Catalog")
            Core.<*> (x Core..: "Database")
            Core.<*> (x Core..: "QueryString")
            Core.<*> (x Core..: "OutputS3Uri")
            Core.<*> (x Core..: "OutputFormat")
      )

instance Core.Hashable AthenaDatasetDefinition

instance Core.NFData AthenaDatasetDefinition

instance Core.ToJSON AthenaDatasetDefinition where
  toJSON AthenaDatasetDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("OutputCompression" Core..=)
              Core.<$> outputCompression,
            ("KmsKeyId" Core..=) Core.<$> kmsKeyId,
            ("WorkGroup" Core..=) Core.<$> workGroup,
            Core.Just ("Catalog" Core..= catalog),
            Core.Just ("Database" Core..= database),
            Core.Just ("QueryString" Core..= queryString),
            Core.Just ("OutputS3Uri" Core..= outputS3Uri),
            Core.Just ("OutputFormat" Core..= outputFormat)
          ]
      )
