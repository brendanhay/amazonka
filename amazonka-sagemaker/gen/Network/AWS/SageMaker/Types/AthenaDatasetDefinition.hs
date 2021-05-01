{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.AthenaResultCompressionType
import Network.AWS.SageMaker.Types.AthenaResultFormat

-- | Configuration for Athena Dataset Definition input.
--
-- /See:/ 'newAthenaDatasetDefinition' smart constructor.
data AthenaDatasetDefinition = AthenaDatasetDefinition'
  { outputCompression :: Prelude.Maybe AthenaResultCompressionType,
    -- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
    -- to encrypt data generated from an Athena query execution.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    workGroup :: Prelude.Maybe Prelude.Text,
    catalog :: Prelude.Text,
    database :: Prelude.Text,
    queryString :: Prelude.Text,
    -- | The location in Amazon S3 where Athena query results are stored.
    outputS3Uri :: Prelude.Text,
    outputFormat :: AthenaResultFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  -- | 'queryString'
  Prelude.Text ->
  -- | 'outputS3Uri'
  Prelude.Text ->
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
          Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        workGroup = Prelude.Nothing,
        catalog = pCatalog_,
        database = pDatabase_,
        queryString = pQueryString_,
        outputS3Uri = pOutputS3Uri_,
        outputFormat = pOutputFormat_
      }

-- | Undocumented member.
athenaDatasetDefinition_outputCompression :: Lens.Lens' AthenaDatasetDefinition (Prelude.Maybe AthenaResultCompressionType)
athenaDatasetDefinition_outputCompression = Lens.lens (\AthenaDatasetDefinition' {outputCompression} -> outputCompression) (\s@AthenaDatasetDefinition' {} a -> s {outputCompression = a} :: AthenaDatasetDefinition)

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
-- to encrypt data generated from an Athena query execution.
athenaDatasetDefinition_kmsKeyId :: Lens.Lens' AthenaDatasetDefinition (Prelude.Maybe Prelude.Text)
athenaDatasetDefinition_kmsKeyId = Lens.lens (\AthenaDatasetDefinition' {kmsKeyId} -> kmsKeyId) (\s@AthenaDatasetDefinition' {} a -> s {kmsKeyId = a} :: AthenaDatasetDefinition)

-- | Undocumented member.
athenaDatasetDefinition_workGroup :: Lens.Lens' AthenaDatasetDefinition (Prelude.Maybe Prelude.Text)
athenaDatasetDefinition_workGroup = Lens.lens (\AthenaDatasetDefinition' {workGroup} -> workGroup) (\s@AthenaDatasetDefinition' {} a -> s {workGroup = a} :: AthenaDatasetDefinition)

-- | Undocumented member.
athenaDatasetDefinition_catalog :: Lens.Lens' AthenaDatasetDefinition Prelude.Text
athenaDatasetDefinition_catalog = Lens.lens (\AthenaDatasetDefinition' {catalog} -> catalog) (\s@AthenaDatasetDefinition' {} a -> s {catalog = a} :: AthenaDatasetDefinition)

-- | Undocumented member.
athenaDatasetDefinition_database :: Lens.Lens' AthenaDatasetDefinition Prelude.Text
athenaDatasetDefinition_database = Lens.lens (\AthenaDatasetDefinition' {database} -> database) (\s@AthenaDatasetDefinition' {} a -> s {database = a} :: AthenaDatasetDefinition)

-- | Undocumented member.
athenaDatasetDefinition_queryString :: Lens.Lens' AthenaDatasetDefinition Prelude.Text
athenaDatasetDefinition_queryString = Lens.lens (\AthenaDatasetDefinition' {queryString} -> queryString) (\s@AthenaDatasetDefinition' {} a -> s {queryString = a} :: AthenaDatasetDefinition)

-- | The location in Amazon S3 where Athena query results are stored.
athenaDatasetDefinition_outputS3Uri :: Lens.Lens' AthenaDatasetDefinition Prelude.Text
athenaDatasetDefinition_outputS3Uri = Lens.lens (\AthenaDatasetDefinition' {outputS3Uri} -> outputS3Uri) (\s@AthenaDatasetDefinition' {} a -> s {outputS3Uri = a} :: AthenaDatasetDefinition)

-- | Undocumented member.
athenaDatasetDefinition_outputFormat :: Lens.Lens' AthenaDatasetDefinition AthenaResultFormat
athenaDatasetDefinition_outputFormat = Lens.lens (\AthenaDatasetDefinition' {outputFormat} -> outputFormat) (\s@AthenaDatasetDefinition' {} a -> s {outputFormat = a} :: AthenaDatasetDefinition)

instance Prelude.FromJSON AthenaDatasetDefinition where
  parseJSON =
    Prelude.withObject
      "AthenaDatasetDefinition"
      ( \x ->
          AthenaDatasetDefinition'
            Prelude.<$> (x Prelude..:? "OutputCompression")
            Prelude.<*> (x Prelude..:? "KmsKeyId")
            Prelude.<*> (x Prelude..:? "WorkGroup")
            Prelude.<*> (x Prelude..: "Catalog")
            Prelude.<*> (x Prelude..: "Database")
            Prelude.<*> (x Prelude..: "QueryString")
            Prelude.<*> (x Prelude..: "OutputS3Uri")
            Prelude.<*> (x Prelude..: "OutputFormat")
      )

instance Prelude.Hashable AthenaDatasetDefinition

instance Prelude.NFData AthenaDatasetDefinition

instance Prelude.ToJSON AthenaDatasetDefinition where
  toJSON AthenaDatasetDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("OutputCompression" Prelude..=)
              Prelude.<$> outputCompression,
            ("KmsKeyId" Prelude..=) Prelude.<$> kmsKeyId,
            ("WorkGroup" Prelude..=) Prelude.<$> workGroup,
            Prelude.Just ("Catalog" Prelude..= catalog),
            Prelude.Just ("Database" Prelude..= database),
            Prelude.Just ("QueryString" Prelude..= queryString),
            Prelude.Just ("OutputS3Uri" Prelude..= outputS3Uri),
            Prelude.Just
              ("OutputFormat" Prelude..= outputFormat)
          ]
      )
