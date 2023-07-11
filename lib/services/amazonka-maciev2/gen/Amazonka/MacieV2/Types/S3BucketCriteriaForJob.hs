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
-- Module      : Amazonka.MacieV2.Types.S3BucketCriteriaForJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.S3BucketCriteriaForJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.CriteriaBlockForJob
import qualified Amazonka.Prelude as Prelude

-- | Specifies property- and tag-based conditions that define criteria for
-- including or excluding S3 buckets from a classification job. Exclude
-- conditions take precedence over include conditions.
--
-- /See:/ 'newS3BucketCriteriaForJob' smart constructor.
data S3BucketCriteriaForJob = S3BucketCriteriaForJob'
  { -- | The property- and tag-based conditions that determine which buckets to
    -- exclude from the job.
    excludes :: Prelude.Maybe CriteriaBlockForJob,
    -- | The property- and tag-based conditions that determine which buckets to
    -- include in the job.
    includes :: Prelude.Maybe CriteriaBlockForJob
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3BucketCriteriaForJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excludes', 's3BucketCriteriaForJob_excludes' - The property- and tag-based conditions that determine which buckets to
-- exclude from the job.
--
-- 'includes', 's3BucketCriteriaForJob_includes' - The property- and tag-based conditions that determine which buckets to
-- include in the job.
newS3BucketCriteriaForJob ::
  S3BucketCriteriaForJob
newS3BucketCriteriaForJob =
  S3BucketCriteriaForJob'
    { excludes = Prelude.Nothing,
      includes = Prelude.Nothing
    }

-- | The property- and tag-based conditions that determine which buckets to
-- exclude from the job.
s3BucketCriteriaForJob_excludes :: Lens.Lens' S3BucketCriteriaForJob (Prelude.Maybe CriteriaBlockForJob)
s3BucketCriteriaForJob_excludes = Lens.lens (\S3BucketCriteriaForJob' {excludes} -> excludes) (\s@S3BucketCriteriaForJob' {} a -> s {excludes = a} :: S3BucketCriteriaForJob)

-- | The property- and tag-based conditions that determine which buckets to
-- include in the job.
s3BucketCriteriaForJob_includes :: Lens.Lens' S3BucketCriteriaForJob (Prelude.Maybe CriteriaBlockForJob)
s3BucketCriteriaForJob_includes = Lens.lens (\S3BucketCriteriaForJob' {includes} -> includes) (\s@S3BucketCriteriaForJob' {} a -> s {includes = a} :: S3BucketCriteriaForJob)

instance Data.FromJSON S3BucketCriteriaForJob where
  parseJSON =
    Data.withObject
      "S3BucketCriteriaForJob"
      ( \x ->
          S3BucketCriteriaForJob'
            Prelude.<$> (x Data..:? "excludes")
            Prelude.<*> (x Data..:? "includes")
      )

instance Prelude.Hashable S3BucketCriteriaForJob where
  hashWithSalt _salt S3BucketCriteriaForJob' {..} =
    _salt
      `Prelude.hashWithSalt` excludes
      `Prelude.hashWithSalt` includes

instance Prelude.NFData S3BucketCriteriaForJob where
  rnf S3BucketCriteriaForJob' {..} =
    Prelude.rnf excludes
      `Prelude.seq` Prelude.rnf includes

instance Data.ToJSON S3BucketCriteriaForJob where
  toJSON S3BucketCriteriaForJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("excludes" Data..=) Prelude.<$> excludes,
            ("includes" Data..=) Prelude.<$> includes
          ]
      )
