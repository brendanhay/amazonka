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
-- Module      : Amazonka.MacieV2.Types.S3ClassificationScopeExclusion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.S3ClassificationScopeExclusion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the names of the S3 buckets that are excluded from automated
-- sensitive data discovery.
--
-- /See:/ 'newS3ClassificationScopeExclusion' smart constructor.
data S3ClassificationScopeExclusion = S3ClassificationScopeExclusion'
  { -- | An array of strings, one for each S3 bucket that is excluded. Each
    -- string is the full name of an excluded bucket.
    bucketNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3ClassificationScopeExclusion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketNames', 's3ClassificationScopeExclusion_bucketNames' - An array of strings, one for each S3 bucket that is excluded. Each
-- string is the full name of an excluded bucket.
newS3ClassificationScopeExclusion ::
  S3ClassificationScopeExclusion
newS3ClassificationScopeExclusion =
  S3ClassificationScopeExclusion'
    { bucketNames =
        Prelude.mempty
    }

-- | An array of strings, one for each S3 bucket that is excluded. Each
-- string is the full name of an excluded bucket.
s3ClassificationScopeExclusion_bucketNames :: Lens.Lens' S3ClassificationScopeExclusion [Prelude.Text]
s3ClassificationScopeExclusion_bucketNames = Lens.lens (\S3ClassificationScopeExclusion' {bucketNames} -> bucketNames) (\s@S3ClassificationScopeExclusion' {} a -> s {bucketNames = a} :: S3ClassificationScopeExclusion) Prelude.. Lens.coerced

instance Data.FromJSON S3ClassificationScopeExclusion where
  parseJSON =
    Data.withObject
      "S3ClassificationScopeExclusion"
      ( \x ->
          S3ClassificationScopeExclusion'
            Prelude.<$> (x Data..:? "bucketNames" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    S3ClassificationScopeExclusion
  where
  hashWithSalt
    _salt
    S3ClassificationScopeExclusion' {..} =
      _salt `Prelude.hashWithSalt` bucketNames

instance
  Prelude.NFData
    S3ClassificationScopeExclusion
  where
  rnf S3ClassificationScopeExclusion' {..} =
    Prelude.rnf bucketNames
