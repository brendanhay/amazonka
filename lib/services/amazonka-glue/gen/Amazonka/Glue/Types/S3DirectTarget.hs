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
-- Module      : Amazonka.Glue.Types.S3DirectTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.S3DirectTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.DirectSchemaChangePolicy
import Amazonka.Glue.Types.TargetFormat
import qualified Amazonka.Prelude as Prelude

-- | Specifies a data target that writes to Amazon S3.
--
-- /See:/ 'newS3DirectTarget' smart constructor.
data S3DirectTarget = S3DirectTarget'
  { -- | Specifies how the data is compressed. This is generally not necessary if
    -- the data has a standard file extension. Possible values are @\"gzip\"@
    -- and @\"bzip\"@).
    compression :: Prelude.Maybe Prelude.Text,
    -- | Specifies native partitioning using a sequence of keys.
    partitionKeys :: Prelude.Maybe [[Prelude.Text]],
    -- | A policy that specifies update behavior for the crawler.
    schemaChangePolicy :: Prelude.Maybe DirectSchemaChangePolicy,
    -- | The name of the data target.
    name :: Prelude.Text,
    -- | The nodes that are inputs to the data target.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | A single Amazon S3 path to write to.
    path :: Prelude.Text,
    -- | Specifies the data output format for the target.
    format :: TargetFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3DirectTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compression', 's3DirectTarget_compression' - Specifies how the data is compressed. This is generally not necessary if
-- the data has a standard file extension. Possible values are @\"gzip\"@
-- and @\"bzip\"@).
--
-- 'partitionKeys', 's3DirectTarget_partitionKeys' - Specifies native partitioning using a sequence of keys.
--
-- 'schemaChangePolicy', 's3DirectTarget_schemaChangePolicy' - A policy that specifies update behavior for the crawler.
--
-- 'name', 's3DirectTarget_name' - The name of the data target.
--
-- 'inputs', 's3DirectTarget_inputs' - The nodes that are inputs to the data target.
--
-- 'path', 's3DirectTarget_path' - A single Amazon S3 path to write to.
--
-- 'format', 's3DirectTarget_format' - Specifies the data output format for the target.
newS3DirectTarget ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'path'
  Prelude.Text ->
  -- | 'format'
  TargetFormat ->
  S3DirectTarget
newS3DirectTarget pName_ pInputs_ pPath_ pFormat_ =
  S3DirectTarget'
    { compression = Prelude.Nothing,
      partitionKeys = Prelude.Nothing,
      schemaChangePolicy = Prelude.Nothing,
      name = pName_,
      inputs = Lens.coerced Lens.# pInputs_,
      path = pPath_,
      format = pFormat_
    }

-- | Specifies how the data is compressed. This is generally not necessary if
-- the data has a standard file extension. Possible values are @\"gzip\"@
-- and @\"bzip\"@).
s3DirectTarget_compression :: Lens.Lens' S3DirectTarget (Prelude.Maybe Prelude.Text)
s3DirectTarget_compression = Lens.lens (\S3DirectTarget' {compression} -> compression) (\s@S3DirectTarget' {} a -> s {compression = a} :: S3DirectTarget)

-- | Specifies native partitioning using a sequence of keys.
s3DirectTarget_partitionKeys :: Lens.Lens' S3DirectTarget (Prelude.Maybe [[Prelude.Text]])
s3DirectTarget_partitionKeys = Lens.lens (\S3DirectTarget' {partitionKeys} -> partitionKeys) (\s@S3DirectTarget' {} a -> s {partitionKeys = a} :: S3DirectTarget) Prelude.. Lens.mapping Lens.coerced

-- | A policy that specifies update behavior for the crawler.
s3DirectTarget_schemaChangePolicy :: Lens.Lens' S3DirectTarget (Prelude.Maybe DirectSchemaChangePolicy)
s3DirectTarget_schemaChangePolicy = Lens.lens (\S3DirectTarget' {schemaChangePolicy} -> schemaChangePolicy) (\s@S3DirectTarget' {} a -> s {schemaChangePolicy = a} :: S3DirectTarget)

-- | The name of the data target.
s3DirectTarget_name :: Lens.Lens' S3DirectTarget Prelude.Text
s3DirectTarget_name = Lens.lens (\S3DirectTarget' {name} -> name) (\s@S3DirectTarget' {} a -> s {name = a} :: S3DirectTarget)

-- | The nodes that are inputs to the data target.
s3DirectTarget_inputs :: Lens.Lens' S3DirectTarget (Prelude.NonEmpty Prelude.Text)
s3DirectTarget_inputs = Lens.lens (\S3DirectTarget' {inputs} -> inputs) (\s@S3DirectTarget' {} a -> s {inputs = a} :: S3DirectTarget) Prelude.. Lens.coerced

-- | A single Amazon S3 path to write to.
s3DirectTarget_path :: Lens.Lens' S3DirectTarget Prelude.Text
s3DirectTarget_path = Lens.lens (\S3DirectTarget' {path} -> path) (\s@S3DirectTarget' {} a -> s {path = a} :: S3DirectTarget)

-- | Specifies the data output format for the target.
s3DirectTarget_format :: Lens.Lens' S3DirectTarget TargetFormat
s3DirectTarget_format = Lens.lens (\S3DirectTarget' {format} -> format) (\s@S3DirectTarget' {} a -> s {format = a} :: S3DirectTarget)

instance Data.FromJSON S3DirectTarget where
  parseJSON =
    Data.withObject
      "S3DirectTarget"
      ( \x ->
          S3DirectTarget'
            Prelude.<$> (x Data..:? "Compression")
            Prelude.<*> (x Data..:? "PartitionKeys" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SchemaChangePolicy")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Inputs")
            Prelude.<*> (x Data..: "Path")
            Prelude.<*> (x Data..: "Format")
      )

instance Prelude.Hashable S3DirectTarget where
  hashWithSalt _salt S3DirectTarget' {..} =
    _salt
      `Prelude.hashWithSalt` compression
      `Prelude.hashWithSalt` partitionKeys
      `Prelude.hashWithSalt` schemaChangePolicy
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` format

instance Prelude.NFData S3DirectTarget where
  rnf S3DirectTarget' {..} =
    Prelude.rnf compression
      `Prelude.seq` Prelude.rnf partitionKeys
      `Prelude.seq` Prelude.rnf schemaChangePolicy
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf format

instance Data.ToJSON S3DirectTarget where
  toJSON S3DirectTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Compression" Data..=) Prelude.<$> compression,
            ("PartitionKeys" Data..=) Prelude.<$> partitionKeys,
            ("SchemaChangePolicy" Data..=)
              Prelude.<$> schemaChangePolicy,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Inputs" Data..= inputs),
            Prelude.Just ("Path" Data..= path),
            Prelude.Just ("Format" Data..= format)
          ]
      )
