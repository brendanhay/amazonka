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
-- Module      : Amazonka.MacieV2.Types.S3JobDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.S3JobDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.S3BucketCriteriaForJob
import Amazonka.MacieV2.Types.S3BucketDefinitionForJob
import Amazonka.MacieV2.Types.Scoping
import qualified Amazonka.Prelude as Prelude

-- | Specifies which S3 buckets contain the objects that a classification job
-- analyzes, and the scope of that analysis. The bucket specification can
-- be static (bucketDefinitions) or dynamic (bucketCriteria). If it\'s
-- static, the job analyzes objects in the same predefined set of buckets
-- each time the job runs. If it\'s dynamic, the job analyzes objects in
-- any buckets that match the specified criteria each time the job starts
-- to run.
--
-- /See:/ 'newS3JobDefinition' smart constructor.
data S3JobDefinition = S3JobDefinition'
  { -- | The property- and tag-based conditions that determine which S3 buckets
    -- to include or exclude from the analysis. Each time the job runs, the job
    -- uses these criteria to determine which buckets contain objects to
    -- analyze. A job\'s definition can contain a bucketCriteria object or a
    -- bucketDefinitions array, not both.
    bucketCriteria :: Prelude.Maybe S3BucketCriteriaForJob,
    -- | An array of objects, one for each Amazon Web Services account that owns
    -- specific S3 buckets to analyze. Each object specifies the account ID for
    -- an account and one or more buckets to analyze for that account. A job\'s
    -- definition can contain a bucketDefinitions array or a bucketCriteria
    -- object, not both.
    bucketDefinitions :: Prelude.Maybe [S3BucketDefinitionForJob],
    -- | The property- and tag-based conditions that determine which S3 objects
    -- to include or exclude from the analysis. Each time the job runs, the job
    -- uses these criteria to determine which objects to analyze.
    scoping :: Prelude.Maybe Scoping
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3JobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketCriteria', 's3JobDefinition_bucketCriteria' - The property- and tag-based conditions that determine which S3 buckets
-- to include or exclude from the analysis. Each time the job runs, the job
-- uses these criteria to determine which buckets contain objects to
-- analyze. A job\'s definition can contain a bucketCriteria object or a
-- bucketDefinitions array, not both.
--
-- 'bucketDefinitions', 's3JobDefinition_bucketDefinitions' - An array of objects, one for each Amazon Web Services account that owns
-- specific S3 buckets to analyze. Each object specifies the account ID for
-- an account and one or more buckets to analyze for that account. A job\'s
-- definition can contain a bucketDefinitions array or a bucketCriteria
-- object, not both.
--
-- 'scoping', 's3JobDefinition_scoping' - The property- and tag-based conditions that determine which S3 objects
-- to include or exclude from the analysis. Each time the job runs, the job
-- uses these criteria to determine which objects to analyze.
newS3JobDefinition ::
  S3JobDefinition
newS3JobDefinition =
  S3JobDefinition'
    { bucketCriteria = Prelude.Nothing,
      bucketDefinitions = Prelude.Nothing,
      scoping = Prelude.Nothing
    }

-- | The property- and tag-based conditions that determine which S3 buckets
-- to include or exclude from the analysis. Each time the job runs, the job
-- uses these criteria to determine which buckets contain objects to
-- analyze. A job\'s definition can contain a bucketCriteria object or a
-- bucketDefinitions array, not both.
s3JobDefinition_bucketCriteria :: Lens.Lens' S3JobDefinition (Prelude.Maybe S3BucketCriteriaForJob)
s3JobDefinition_bucketCriteria = Lens.lens (\S3JobDefinition' {bucketCriteria} -> bucketCriteria) (\s@S3JobDefinition' {} a -> s {bucketCriteria = a} :: S3JobDefinition)

-- | An array of objects, one for each Amazon Web Services account that owns
-- specific S3 buckets to analyze. Each object specifies the account ID for
-- an account and one or more buckets to analyze for that account. A job\'s
-- definition can contain a bucketDefinitions array or a bucketCriteria
-- object, not both.
s3JobDefinition_bucketDefinitions :: Lens.Lens' S3JobDefinition (Prelude.Maybe [S3BucketDefinitionForJob])
s3JobDefinition_bucketDefinitions = Lens.lens (\S3JobDefinition' {bucketDefinitions} -> bucketDefinitions) (\s@S3JobDefinition' {} a -> s {bucketDefinitions = a} :: S3JobDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The property- and tag-based conditions that determine which S3 objects
-- to include or exclude from the analysis. Each time the job runs, the job
-- uses these criteria to determine which objects to analyze.
s3JobDefinition_scoping :: Lens.Lens' S3JobDefinition (Prelude.Maybe Scoping)
s3JobDefinition_scoping = Lens.lens (\S3JobDefinition' {scoping} -> scoping) (\s@S3JobDefinition' {} a -> s {scoping = a} :: S3JobDefinition)

instance Data.FromJSON S3JobDefinition where
  parseJSON =
    Data.withObject
      "S3JobDefinition"
      ( \x ->
          S3JobDefinition'
            Prelude.<$> (x Data..:? "bucketCriteria")
            Prelude.<*> ( x Data..:? "bucketDefinitions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "scoping")
      )

instance Prelude.Hashable S3JobDefinition where
  hashWithSalt _salt S3JobDefinition' {..} =
    _salt `Prelude.hashWithSalt` bucketCriteria
      `Prelude.hashWithSalt` bucketDefinitions
      `Prelude.hashWithSalt` scoping

instance Prelude.NFData S3JobDefinition where
  rnf S3JobDefinition' {..} =
    Prelude.rnf bucketCriteria
      `Prelude.seq` Prelude.rnf bucketDefinitions
      `Prelude.seq` Prelude.rnf scoping

instance Data.ToJSON S3JobDefinition where
  toJSON S3JobDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bucketCriteria" Data..=)
              Prelude.<$> bucketCriteria,
            ("bucketDefinitions" Data..=)
              Prelude.<$> bucketDefinitions,
            ("scoping" Data..=) Prelude.<$> scoping
          ]
      )
