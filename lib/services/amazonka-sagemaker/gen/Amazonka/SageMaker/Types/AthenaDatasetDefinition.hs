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
-- Module      : Amazonka.SageMaker.Types.AthenaDatasetDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AthenaDatasetDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AthenaResultCompressionType
import Amazonka.SageMaker.Types.AthenaResultFormat

-- | Configuration for Athena Dataset Definition input.
--
-- /See:/ 'newAthenaDatasetDefinition' smart constructor.
data AthenaDatasetDefinition = AthenaDatasetDefinition'
  { -- | The Amazon Web Services Key Management Service (Amazon Web Services KMS)
    -- key that Amazon SageMaker uses to encrypt data generated from an Athena
    -- query execution.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    outputCompression :: Prelude.Maybe AthenaResultCompressionType,
    workGroup :: Prelude.Maybe Prelude.Text,
    catalog :: Prelude.Text,
    database :: Prelude.Text,
    queryString :: Prelude.Text,
    -- | The location in Amazon S3 where Athena query results are stored.
    outputS3Uri :: Prelude.Text,
    outputFormat :: AthenaResultFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AthenaDatasetDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'athenaDatasetDefinition_kmsKeyId' - The Amazon Web Services Key Management Service (Amazon Web Services KMS)
-- key that Amazon SageMaker uses to encrypt data generated from an Athena
-- query execution.
--
-- 'outputCompression', 'athenaDatasetDefinition_outputCompression' - Undocumented member.
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
      { kmsKeyId =
          Prelude.Nothing,
        outputCompression = Prelude.Nothing,
        workGroup = Prelude.Nothing,
        catalog = pCatalog_,
        database = pDatabase_,
        queryString = pQueryString_,
        outputS3Uri = pOutputS3Uri_,
        outputFormat = pOutputFormat_
      }

-- | The Amazon Web Services Key Management Service (Amazon Web Services KMS)
-- key that Amazon SageMaker uses to encrypt data generated from an Athena
-- query execution.
athenaDatasetDefinition_kmsKeyId :: Lens.Lens' AthenaDatasetDefinition (Prelude.Maybe Prelude.Text)
athenaDatasetDefinition_kmsKeyId = Lens.lens (\AthenaDatasetDefinition' {kmsKeyId} -> kmsKeyId) (\s@AthenaDatasetDefinition' {} a -> s {kmsKeyId = a} :: AthenaDatasetDefinition)

-- | Undocumented member.
athenaDatasetDefinition_outputCompression :: Lens.Lens' AthenaDatasetDefinition (Prelude.Maybe AthenaResultCompressionType)
athenaDatasetDefinition_outputCompression = Lens.lens (\AthenaDatasetDefinition' {outputCompression} -> outputCompression) (\s@AthenaDatasetDefinition' {} a -> s {outputCompression = a} :: AthenaDatasetDefinition)

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

instance Data.FromJSON AthenaDatasetDefinition where
  parseJSON =
    Data.withObject
      "AthenaDatasetDefinition"
      ( \x ->
          AthenaDatasetDefinition'
            Prelude.<$> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "OutputCompression")
            Prelude.<*> (x Data..:? "WorkGroup")
            Prelude.<*> (x Data..: "Catalog")
            Prelude.<*> (x Data..: "Database")
            Prelude.<*> (x Data..: "QueryString")
            Prelude.<*> (x Data..: "OutputS3Uri")
            Prelude.<*> (x Data..: "OutputFormat")
      )

instance Prelude.Hashable AthenaDatasetDefinition where
  hashWithSalt _salt AthenaDatasetDefinition' {..} =
    _salt `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` outputCompression
      `Prelude.hashWithSalt` workGroup
      `Prelude.hashWithSalt` catalog
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` queryString
      `Prelude.hashWithSalt` outputS3Uri
      `Prelude.hashWithSalt` outputFormat

instance Prelude.NFData AthenaDatasetDefinition where
  rnf AthenaDatasetDefinition' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf outputCompression
      `Prelude.seq` Prelude.rnf workGroup
      `Prelude.seq` Prelude.rnf catalog
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf queryString
      `Prelude.seq` Prelude.rnf outputS3Uri
      `Prelude.seq` Prelude.rnf outputFormat

instance Data.ToJSON AthenaDatasetDefinition where
  toJSON AthenaDatasetDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("OutputCompression" Data..=)
              Prelude.<$> outputCompression,
            ("WorkGroup" Data..=) Prelude.<$> workGroup,
            Prelude.Just ("Catalog" Data..= catalog),
            Prelude.Just ("Database" Data..= database),
            Prelude.Just ("QueryString" Data..= queryString),
            Prelude.Just ("OutputS3Uri" Data..= outputS3Uri),
            Prelude.Just ("OutputFormat" Data..= outputFormat)
          ]
      )
