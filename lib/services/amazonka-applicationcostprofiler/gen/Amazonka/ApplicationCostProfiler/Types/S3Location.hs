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
-- Module      : Amazonka.ApplicationCostProfiler.Types.S3Location
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationCostProfiler.Types.S3Location where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the Amazon Simple Storage Service (Amazon S3) location where
-- AWS Application Cost Profiler reports are generated and then written to.
--
-- /See:/ 'newS3Location' smart constructor.
data S3Location = S3Location'
  { -- | Name of the S3 bucket.
    bucket :: Prelude.Text,
    -- | Prefix for the location to write to.
    prefix :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Location' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 's3Location_bucket' - Name of the S3 bucket.
--
-- 'prefix', 's3Location_prefix' - Prefix for the location to write to.
newS3Location ::
  -- | 'bucket'
  Prelude.Text ->
  -- | 'prefix'
  Prelude.Text ->
  S3Location
newS3Location pBucket_ pPrefix_ =
  S3Location' {bucket = pBucket_, prefix = pPrefix_}

-- | Name of the S3 bucket.
s3Location_bucket :: Lens.Lens' S3Location Prelude.Text
s3Location_bucket = Lens.lens (\S3Location' {bucket} -> bucket) (\s@S3Location' {} a -> s {bucket = a} :: S3Location)

-- | Prefix for the location to write to.
s3Location_prefix :: Lens.Lens' S3Location Prelude.Text
s3Location_prefix = Lens.lens (\S3Location' {prefix} -> prefix) (\s@S3Location' {} a -> s {prefix = a} :: S3Location)

instance Data.FromJSON S3Location where
  parseJSON =
    Data.withObject
      "S3Location"
      ( \x ->
          S3Location'
            Prelude.<$> (x Data..: "bucket")
            Prelude.<*> (x Data..: "prefix")
      )

instance Prelude.Hashable S3Location where
  hashWithSalt _salt S3Location' {..} =
    _salt
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` prefix

instance Prelude.NFData S3Location where
  rnf S3Location' {..} =
    Prelude.rnf bucket `Prelude.seq` Prelude.rnf prefix

instance Data.ToJSON S3Location where
  toJSON S3Location' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("bucket" Data..= bucket),
            Prelude.Just ("prefix" Data..= prefix)
          ]
      )
