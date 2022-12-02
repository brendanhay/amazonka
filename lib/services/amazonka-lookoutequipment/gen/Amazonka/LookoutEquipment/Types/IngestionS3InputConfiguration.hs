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
-- Module      : Amazonka.LookoutEquipment.Types.IngestionS3InputConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.IngestionS3InputConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies S3 configuration information for the input data for the data
-- ingestion job.
--
-- /See:/ 'newIngestionS3InputConfiguration' smart constructor.
data IngestionS3InputConfiguration = IngestionS3InputConfiguration'
  { -- | Pattern for matching the Amazon S3 files which will be used for
    -- ingestion. If no KeyPattern is provided, we will use the default
    -- hierarchy file structure, which is same as KeyPattern
    -- {prefix}\/{component_name}\/*
    keyPattern :: Prelude.Maybe Prelude.Text,
    -- | The prefix for the S3 location being used for the input data for the
    -- data ingestion.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket used for the input data for the data
    -- ingestion.
    bucket :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IngestionS3InputConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPattern', 'ingestionS3InputConfiguration_keyPattern' - Pattern for matching the Amazon S3 files which will be used for
-- ingestion. If no KeyPattern is provided, we will use the default
-- hierarchy file structure, which is same as KeyPattern
-- {prefix}\/{component_name}\/*
--
-- 'prefix', 'ingestionS3InputConfiguration_prefix' - The prefix for the S3 location being used for the input data for the
-- data ingestion.
--
-- 'bucket', 'ingestionS3InputConfiguration_bucket' - The name of the S3 bucket used for the input data for the data
-- ingestion.
newIngestionS3InputConfiguration ::
  -- | 'bucket'
  Prelude.Text ->
  IngestionS3InputConfiguration
newIngestionS3InputConfiguration pBucket_ =
  IngestionS3InputConfiguration'
    { keyPattern =
        Prelude.Nothing,
      prefix = Prelude.Nothing,
      bucket = pBucket_
    }

-- | Pattern for matching the Amazon S3 files which will be used for
-- ingestion. If no KeyPattern is provided, we will use the default
-- hierarchy file structure, which is same as KeyPattern
-- {prefix}\/{component_name}\/*
ingestionS3InputConfiguration_keyPattern :: Lens.Lens' IngestionS3InputConfiguration (Prelude.Maybe Prelude.Text)
ingestionS3InputConfiguration_keyPattern = Lens.lens (\IngestionS3InputConfiguration' {keyPattern} -> keyPattern) (\s@IngestionS3InputConfiguration' {} a -> s {keyPattern = a} :: IngestionS3InputConfiguration)

-- | The prefix for the S3 location being used for the input data for the
-- data ingestion.
ingestionS3InputConfiguration_prefix :: Lens.Lens' IngestionS3InputConfiguration (Prelude.Maybe Prelude.Text)
ingestionS3InputConfiguration_prefix = Lens.lens (\IngestionS3InputConfiguration' {prefix} -> prefix) (\s@IngestionS3InputConfiguration' {} a -> s {prefix = a} :: IngestionS3InputConfiguration)

-- | The name of the S3 bucket used for the input data for the data
-- ingestion.
ingestionS3InputConfiguration_bucket :: Lens.Lens' IngestionS3InputConfiguration Prelude.Text
ingestionS3InputConfiguration_bucket = Lens.lens (\IngestionS3InputConfiguration' {bucket} -> bucket) (\s@IngestionS3InputConfiguration' {} a -> s {bucket = a} :: IngestionS3InputConfiguration)

instance Data.FromJSON IngestionS3InputConfiguration where
  parseJSON =
    Data.withObject
      "IngestionS3InputConfiguration"
      ( \x ->
          IngestionS3InputConfiguration'
            Prelude.<$> (x Data..:? "KeyPattern")
            Prelude.<*> (x Data..:? "Prefix")
            Prelude.<*> (x Data..: "Bucket")
      )

instance
  Prelude.Hashable
    IngestionS3InputConfiguration
  where
  hashWithSalt _salt IngestionS3InputConfiguration' {..} =
    _salt `Prelude.hashWithSalt` keyPattern
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData IngestionS3InputConfiguration where
  rnf IngestionS3InputConfiguration' {..} =
    Prelude.rnf keyPattern
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf bucket

instance Data.ToJSON IngestionS3InputConfiguration where
  toJSON IngestionS3InputConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KeyPattern" Data..=) Prelude.<$> keyPattern,
            ("Prefix" Data..=) Prelude.<$> prefix,
            Prelude.Just ("Bucket" Data..= bucket)
          ]
      )
