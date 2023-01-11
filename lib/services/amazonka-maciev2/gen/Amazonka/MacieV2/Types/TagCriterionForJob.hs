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
-- Module      : Amazonka.MacieV2.Types.TagCriterionForJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.TagCriterionForJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.JobComparator
import Amazonka.MacieV2.Types.TagCriterionPairForJob
import qualified Amazonka.Prelude as Prelude

-- | Specifies a tag-based condition that determines whether an S3 bucket is
-- included or excluded from a classification job.
--
-- /See:/ 'newTagCriterionForJob' smart constructor.
data TagCriterionForJob = TagCriterionForJob'
  { -- | The operator to use in the condition. Valid values are EQ (equals) and
    -- NE (not equals).
    comparator :: Prelude.Maybe JobComparator,
    -- | The tag keys, tag values, or tag key and value pairs to use in the
    -- condition.
    tagValues :: Prelude.Maybe [TagCriterionPairForJob]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagCriterionForJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comparator', 'tagCriterionForJob_comparator' - The operator to use in the condition. Valid values are EQ (equals) and
-- NE (not equals).
--
-- 'tagValues', 'tagCriterionForJob_tagValues' - The tag keys, tag values, or tag key and value pairs to use in the
-- condition.
newTagCriterionForJob ::
  TagCriterionForJob
newTagCriterionForJob =
  TagCriterionForJob'
    { comparator = Prelude.Nothing,
      tagValues = Prelude.Nothing
    }

-- | The operator to use in the condition. Valid values are EQ (equals) and
-- NE (not equals).
tagCriterionForJob_comparator :: Lens.Lens' TagCriterionForJob (Prelude.Maybe JobComparator)
tagCriterionForJob_comparator = Lens.lens (\TagCriterionForJob' {comparator} -> comparator) (\s@TagCriterionForJob' {} a -> s {comparator = a} :: TagCriterionForJob)

-- | The tag keys, tag values, or tag key and value pairs to use in the
-- condition.
tagCriterionForJob_tagValues :: Lens.Lens' TagCriterionForJob (Prelude.Maybe [TagCriterionPairForJob])
tagCriterionForJob_tagValues = Lens.lens (\TagCriterionForJob' {tagValues} -> tagValues) (\s@TagCriterionForJob' {} a -> s {tagValues = a} :: TagCriterionForJob) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TagCriterionForJob where
  parseJSON =
    Data.withObject
      "TagCriterionForJob"
      ( \x ->
          TagCriterionForJob'
            Prelude.<$> (x Data..:? "comparator")
            Prelude.<*> (x Data..:? "tagValues" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable TagCriterionForJob where
  hashWithSalt _salt TagCriterionForJob' {..} =
    _salt `Prelude.hashWithSalt` comparator
      `Prelude.hashWithSalt` tagValues

instance Prelude.NFData TagCriterionForJob where
  rnf TagCriterionForJob' {..} =
    Prelude.rnf comparator
      `Prelude.seq` Prelude.rnf tagValues

instance Data.ToJSON TagCriterionForJob where
  toJSON TagCriterionForJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("comparator" Data..=) Prelude.<$> comparator,
            ("tagValues" Data..=) Prelude.<$> tagValues
          ]
      )
