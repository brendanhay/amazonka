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
-- Module      : Amazonka.Glue.Types.S3DeltaDirectTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.S3DeltaDirectTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.DeltaTargetCompressionType
import Amazonka.Glue.Types.DirectSchemaChangePolicy
import Amazonka.Glue.Types.TargetFormat
import qualified Amazonka.Prelude as Prelude

-- | Specifies a target that writes to a Delta Lake data source in Amazon S3.
--
-- /See:/ 'newS3DeltaDirectTarget' smart constructor.
data S3DeltaDirectTarget = S3DeltaDirectTarget'
  { -- | Specifies additional connection options for the connector.
    additionalOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies native partitioning using a sequence of keys.
    partitionKeys :: Prelude.Maybe [[Prelude.Text]],
    -- | A policy that specifies update behavior for the crawler.
    schemaChangePolicy :: Prelude.Maybe DirectSchemaChangePolicy,
    -- | The name of the data target.
    name :: Prelude.Text,
    -- | The nodes that are inputs to the data target.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | The Amazon S3 path of your Delta Lake data source to write to.
    path :: Prelude.Text,
    -- | Specifies how the data is compressed. This is generally not necessary if
    -- the data has a standard file extension. Possible values are @\"gzip\"@
    -- and @\"bzip\"@).
    compression :: DeltaTargetCompressionType,
    -- | Specifies the data output format for the target.
    format :: TargetFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3DeltaDirectTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalOptions', 's3DeltaDirectTarget_additionalOptions' - Specifies additional connection options for the connector.
--
-- 'partitionKeys', 's3DeltaDirectTarget_partitionKeys' - Specifies native partitioning using a sequence of keys.
--
-- 'schemaChangePolicy', 's3DeltaDirectTarget_schemaChangePolicy' - A policy that specifies update behavior for the crawler.
--
-- 'name', 's3DeltaDirectTarget_name' - The name of the data target.
--
-- 'inputs', 's3DeltaDirectTarget_inputs' - The nodes that are inputs to the data target.
--
-- 'path', 's3DeltaDirectTarget_path' - The Amazon S3 path of your Delta Lake data source to write to.
--
-- 'compression', 's3DeltaDirectTarget_compression' - Specifies how the data is compressed. This is generally not necessary if
-- the data has a standard file extension. Possible values are @\"gzip\"@
-- and @\"bzip\"@).
--
-- 'format', 's3DeltaDirectTarget_format' - Specifies the data output format for the target.
newS3DeltaDirectTarget ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'path'
  Prelude.Text ->
  -- | 'compression'
  DeltaTargetCompressionType ->
  -- | 'format'
  TargetFormat ->
  S3DeltaDirectTarget
newS3DeltaDirectTarget
  pName_
  pInputs_
  pPath_
  pCompression_
  pFormat_ =
    S3DeltaDirectTarget'
      { additionalOptions =
          Prelude.Nothing,
        partitionKeys = Prelude.Nothing,
        schemaChangePolicy = Prelude.Nothing,
        name = pName_,
        inputs = Lens.coerced Lens.# pInputs_,
        path = pPath_,
        compression = pCompression_,
        format = pFormat_
      }

-- | Specifies additional connection options for the connector.
s3DeltaDirectTarget_additionalOptions :: Lens.Lens' S3DeltaDirectTarget (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
s3DeltaDirectTarget_additionalOptions = Lens.lens (\S3DeltaDirectTarget' {additionalOptions} -> additionalOptions) (\s@S3DeltaDirectTarget' {} a -> s {additionalOptions = a} :: S3DeltaDirectTarget) Prelude.. Lens.mapping Lens.coerced

-- | Specifies native partitioning using a sequence of keys.
s3DeltaDirectTarget_partitionKeys :: Lens.Lens' S3DeltaDirectTarget (Prelude.Maybe [[Prelude.Text]])
s3DeltaDirectTarget_partitionKeys = Lens.lens (\S3DeltaDirectTarget' {partitionKeys} -> partitionKeys) (\s@S3DeltaDirectTarget' {} a -> s {partitionKeys = a} :: S3DeltaDirectTarget) Prelude.. Lens.mapping Lens.coerced

-- | A policy that specifies update behavior for the crawler.
s3DeltaDirectTarget_schemaChangePolicy :: Lens.Lens' S3DeltaDirectTarget (Prelude.Maybe DirectSchemaChangePolicy)
s3DeltaDirectTarget_schemaChangePolicy = Lens.lens (\S3DeltaDirectTarget' {schemaChangePolicy} -> schemaChangePolicy) (\s@S3DeltaDirectTarget' {} a -> s {schemaChangePolicy = a} :: S3DeltaDirectTarget)

-- | The name of the data target.
s3DeltaDirectTarget_name :: Lens.Lens' S3DeltaDirectTarget Prelude.Text
s3DeltaDirectTarget_name = Lens.lens (\S3DeltaDirectTarget' {name} -> name) (\s@S3DeltaDirectTarget' {} a -> s {name = a} :: S3DeltaDirectTarget)

-- | The nodes that are inputs to the data target.
s3DeltaDirectTarget_inputs :: Lens.Lens' S3DeltaDirectTarget (Prelude.NonEmpty Prelude.Text)
s3DeltaDirectTarget_inputs = Lens.lens (\S3DeltaDirectTarget' {inputs} -> inputs) (\s@S3DeltaDirectTarget' {} a -> s {inputs = a} :: S3DeltaDirectTarget) Prelude.. Lens.coerced

-- | The Amazon S3 path of your Delta Lake data source to write to.
s3DeltaDirectTarget_path :: Lens.Lens' S3DeltaDirectTarget Prelude.Text
s3DeltaDirectTarget_path = Lens.lens (\S3DeltaDirectTarget' {path} -> path) (\s@S3DeltaDirectTarget' {} a -> s {path = a} :: S3DeltaDirectTarget)

-- | Specifies how the data is compressed. This is generally not necessary if
-- the data has a standard file extension. Possible values are @\"gzip\"@
-- and @\"bzip\"@).
s3DeltaDirectTarget_compression :: Lens.Lens' S3DeltaDirectTarget DeltaTargetCompressionType
s3DeltaDirectTarget_compression = Lens.lens (\S3DeltaDirectTarget' {compression} -> compression) (\s@S3DeltaDirectTarget' {} a -> s {compression = a} :: S3DeltaDirectTarget)

-- | Specifies the data output format for the target.
s3DeltaDirectTarget_format :: Lens.Lens' S3DeltaDirectTarget TargetFormat
s3DeltaDirectTarget_format = Lens.lens (\S3DeltaDirectTarget' {format} -> format) (\s@S3DeltaDirectTarget' {} a -> s {format = a} :: S3DeltaDirectTarget)

instance Data.FromJSON S3DeltaDirectTarget where
  parseJSON =
    Data.withObject
      "S3DeltaDirectTarget"
      ( \x ->
          S3DeltaDirectTarget'
            Prelude.<$> ( x
                            Data..:? "AdditionalOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "PartitionKeys" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SchemaChangePolicy")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Inputs")
            Prelude.<*> (x Data..: "Path")
            Prelude.<*> (x Data..: "Compression")
            Prelude.<*> (x Data..: "Format")
      )

instance Prelude.Hashable S3DeltaDirectTarget where
  hashWithSalt _salt S3DeltaDirectTarget' {..} =
    _salt
      `Prelude.hashWithSalt` additionalOptions
      `Prelude.hashWithSalt` partitionKeys
      `Prelude.hashWithSalt` schemaChangePolicy
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` compression
      `Prelude.hashWithSalt` format

instance Prelude.NFData S3DeltaDirectTarget where
  rnf S3DeltaDirectTarget' {..} =
    Prelude.rnf additionalOptions
      `Prelude.seq` Prelude.rnf partitionKeys
      `Prelude.seq` Prelude.rnf schemaChangePolicy
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf compression
      `Prelude.seq` Prelude.rnf format

instance Data.ToJSON S3DeltaDirectTarget where
  toJSON S3DeltaDirectTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalOptions" Data..=)
              Prelude.<$> additionalOptions,
            ("PartitionKeys" Data..=) Prelude.<$> partitionKeys,
            ("SchemaChangePolicy" Data..=)
              Prelude.<$> schemaChangePolicy,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Inputs" Data..= inputs),
            Prelude.Just ("Path" Data..= path),
            Prelude.Just ("Compression" Data..= compression),
            Prelude.Just ("Format" Data..= format)
          ]
      )
