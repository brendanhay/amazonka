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
-- Module      : Amazonka.Glue.Types.S3HudiDirectTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.S3HudiDirectTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.DirectSchemaChangePolicy
import Amazonka.Glue.Types.HudiTargetCompressionType
import Amazonka.Glue.Types.TargetFormat
import qualified Amazonka.Prelude as Prelude

-- | Specifies a target that writes to a Hudi data source in Amazon S3.
--
-- /See:/ 'newS3HudiDirectTarget' smart constructor.
data S3HudiDirectTarget = S3HudiDirectTarget'
  { -- | Specifies native partitioning using a sequence of keys.
    partitionKeys :: Prelude.Maybe [[Prelude.Text]],
    -- | A policy that specifies update behavior for the crawler.
    schemaChangePolicy :: Prelude.Maybe DirectSchemaChangePolicy,
    -- | The name of the data target.
    name :: Prelude.Text,
    -- | The nodes that are inputs to the data target.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | The Amazon S3 path of your Hudi data source to write to.
    path :: Prelude.Text,
    -- | Specifies how the data is compressed. This is generally not necessary if
    -- the data has a standard file extension. Possible values are @\"gzip\"@
    -- and @\"bzip\"@).
    compression :: HudiTargetCompressionType,
    -- | Specifies the data output format for the target.
    format :: TargetFormat,
    -- | Specifies additional connection options for the connector.
    additionalOptions :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3HudiDirectTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partitionKeys', 's3HudiDirectTarget_partitionKeys' - Specifies native partitioning using a sequence of keys.
--
-- 'schemaChangePolicy', 's3HudiDirectTarget_schemaChangePolicy' - A policy that specifies update behavior for the crawler.
--
-- 'name', 's3HudiDirectTarget_name' - The name of the data target.
--
-- 'inputs', 's3HudiDirectTarget_inputs' - The nodes that are inputs to the data target.
--
-- 'path', 's3HudiDirectTarget_path' - The Amazon S3 path of your Hudi data source to write to.
--
-- 'compression', 's3HudiDirectTarget_compression' - Specifies how the data is compressed. This is generally not necessary if
-- the data has a standard file extension. Possible values are @\"gzip\"@
-- and @\"bzip\"@).
--
-- 'format', 's3HudiDirectTarget_format' - Specifies the data output format for the target.
--
-- 'additionalOptions', 's3HudiDirectTarget_additionalOptions' - Specifies additional connection options for the connector.
newS3HudiDirectTarget ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'path'
  Prelude.Text ->
  -- | 'compression'
  HudiTargetCompressionType ->
  -- | 'format'
  TargetFormat ->
  S3HudiDirectTarget
newS3HudiDirectTarget
  pName_
  pInputs_
  pPath_
  pCompression_
  pFormat_ =
    S3HudiDirectTarget'
      { partitionKeys =
          Prelude.Nothing,
        schemaChangePolicy = Prelude.Nothing,
        name = pName_,
        inputs = Lens.coerced Lens.# pInputs_,
        path = pPath_,
        compression = pCompression_,
        format = pFormat_,
        additionalOptions = Prelude.mempty
      }

-- | Specifies native partitioning using a sequence of keys.
s3HudiDirectTarget_partitionKeys :: Lens.Lens' S3HudiDirectTarget (Prelude.Maybe [[Prelude.Text]])
s3HudiDirectTarget_partitionKeys = Lens.lens (\S3HudiDirectTarget' {partitionKeys} -> partitionKeys) (\s@S3HudiDirectTarget' {} a -> s {partitionKeys = a} :: S3HudiDirectTarget) Prelude.. Lens.mapping Lens.coerced

-- | A policy that specifies update behavior for the crawler.
s3HudiDirectTarget_schemaChangePolicy :: Lens.Lens' S3HudiDirectTarget (Prelude.Maybe DirectSchemaChangePolicy)
s3HudiDirectTarget_schemaChangePolicy = Lens.lens (\S3HudiDirectTarget' {schemaChangePolicy} -> schemaChangePolicy) (\s@S3HudiDirectTarget' {} a -> s {schemaChangePolicy = a} :: S3HudiDirectTarget)

-- | The name of the data target.
s3HudiDirectTarget_name :: Lens.Lens' S3HudiDirectTarget Prelude.Text
s3HudiDirectTarget_name = Lens.lens (\S3HudiDirectTarget' {name} -> name) (\s@S3HudiDirectTarget' {} a -> s {name = a} :: S3HudiDirectTarget)

-- | The nodes that are inputs to the data target.
s3HudiDirectTarget_inputs :: Lens.Lens' S3HudiDirectTarget (Prelude.NonEmpty Prelude.Text)
s3HudiDirectTarget_inputs = Lens.lens (\S3HudiDirectTarget' {inputs} -> inputs) (\s@S3HudiDirectTarget' {} a -> s {inputs = a} :: S3HudiDirectTarget) Prelude.. Lens.coerced

-- | The Amazon S3 path of your Hudi data source to write to.
s3HudiDirectTarget_path :: Lens.Lens' S3HudiDirectTarget Prelude.Text
s3HudiDirectTarget_path = Lens.lens (\S3HudiDirectTarget' {path} -> path) (\s@S3HudiDirectTarget' {} a -> s {path = a} :: S3HudiDirectTarget)

-- | Specifies how the data is compressed. This is generally not necessary if
-- the data has a standard file extension. Possible values are @\"gzip\"@
-- and @\"bzip\"@).
s3HudiDirectTarget_compression :: Lens.Lens' S3HudiDirectTarget HudiTargetCompressionType
s3HudiDirectTarget_compression = Lens.lens (\S3HudiDirectTarget' {compression} -> compression) (\s@S3HudiDirectTarget' {} a -> s {compression = a} :: S3HudiDirectTarget)

-- | Specifies the data output format for the target.
s3HudiDirectTarget_format :: Lens.Lens' S3HudiDirectTarget TargetFormat
s3HudiDirectTarget_format = Lens.lens (\S3HudiDirectTarget' {format} -> format) (\s@S3HudiDirectTarget' {} a -> s {format = a} :: S3HudiDirectTarget)

-- | Specifies additional connection options for the connector.
s3HudiDirectTarget_additionalOptions :: Lens.Lens' S3HudiDirectTarget (Prelude.HashMap Prelude.Text Prelude.Text)
s3HudiDirectTarget_additionalOptions = Lens.lens (\S3HudiDirectTarget' {additionalOptions} -> additionalOptions) (\s@S3HudiDirectTarget' {} a -> s {additionalOptions = a} :: S3HudiDirectTarget) Prelude.. Lens.coerced

instance Data.FromJSON S3HudiDirectTarget where
  parseJSON =
    Data.withObject
      "S3HudiDirectTarget"
      ( \x ->
          S3HudiDirectTarget'
            Prelude.<$> (x Data..:? "PartitionKeys" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SchemaChangePolicy")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Inputs")
            Prelude.<*> (x Data..: "Path")
            Prelude.<*> (x Data..: "Compression")
            Prelude.<*> (x Data..: "Format")
            Prelude.<*> ( x
                            Data..:? "AdditionalOptions"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable S3HudiDirectTarget where
  hashWithSalt _salt S3HudiDirectTarget' {..} =
    _salt
      `Prelude.hashWithSalt` partitionKeys
      `Prelude.hashWithSalt` schemaChangePolicy
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` compression
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` additionalOptions

instance Prelude.NFData S3HudiDirectTarget where
  rnf S3HudiDirectTarget' {..} =
    Prelude.rnf partitionKeys
      `Prelude.seq` Prelude.rnf schemaChangePolicy
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf compression
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf additionalOptions

instance Data.ToJSON S3HudiDirectTarget where
  toJSON S3HudiDirectTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PartitionKeys" Data..=) Prelude.<$> partitionKeys,
            ("SchemaChangePolicy" Data..=)
              Prelude.<$> schemaChangePolicy,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Inputs" Data..= inputs),
            Prelude.Just ("Path" Data..= path),
            Prelude.Just ("Compression" Data..= compression),
            Prelude.Just ("Format" Data..= format),
            Prelude.Just
              ("AdditionalOptions" Data..= additionalOptions)
          ]
      )
