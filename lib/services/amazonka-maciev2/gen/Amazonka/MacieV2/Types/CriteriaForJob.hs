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
-- Module      : Amazonka.MacieV2.Types.CriteriaForJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.CriteriaForJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.SimpleCriterionForJob
import Amazonka.MacieV2.Types.TagCriterionForJob
import qualified Amazonka.Prelude as Prelude

-- | Specifies a property- or tag-based condition that defines criteria for
-- including or excluding S3 buckets from a classification job.
--
-- /See:/ 'newCriteriaForJob' smart constructor.
data CriteriaForJob = CriteriaForJob'
  { -- | A property-based condition that defines a property, operator, and one or
    -- more values for including or excluding buckets from the job.
    simpleCriterion :: Prelude.Maybe SimpleCriterionForJob,
    -- | A tag-based condition that defines an operator and tag keys, tag values,
    -- or tag key and value pairs for including or excluding buckets from the
    -- job.
    tagCriterion :: Prelude.Maybe TagCriterionForJob
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CriteriaForJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'simpleCriterion', 'criteriaForJob_simpleCriterion' - A property-based condition that defines a property, operator, and one or
-- more values for including or excluding buckets from the job.
--
-- 'tagCriterion', 'criteriaForJob_tagCriterion' - A tag-based condition that defines an operator and tag keys, tag values,
-- or tag key and value pairs for including or excluding buckets from the
-- job.
newCriteriaForJob ::
  CriteriaForJob
newCriteriaForJob =
  CriteriaForJob'
    { simpleCriterion = Prelude.Nothing,
      tagCriterion = Prelude.Nothing
    }

-- | A property-based condition that defines a property, operator, and one or
-- more values for including or excluding buckets from the job.
criteriaForJob_simpleCriterion :: Lens.Lens' CriteriaForJob (Prelude.Maybe SimpleCriterionForJob)
criteriaForJob_simpleCriterion = Lens.lens (\CriteriaForJob' {simpleCriterion} -> simpleCriterion) (\s@CriteriaForJob' {} a -> s {simpleCriterion = a} :: CriteriaForJob)

-- | A tag-based condition that defines an operator and tag keys, tag values,
-- or tag key and value pairs for including or excluding buckets from the
-- job.
criteriaForJob_tagCriterion :: Lens.Lens' CriteriaForJob (Prelude.Maybe TagCriterionForJob)
criteriaForJob_tagCriterion = Lens.lens (\CriteriaForJob' {tagCriterion} -> tagCriterion) (\s@CriteriaForJob' {} a -> s {tagCriterion = a} :: CriteriaForJob)

instance Data.FromJSON CriteriaForJob where
  parseJSON =
    Data.withObject
      "CriteriaForJob"
      ( \x ->
          CriteriaForJob'
            Prelude.<$> (x Data..:? "simpleCriterion")
            Prelude.<*> (x Data..:? "tagCriterion")
      )

instance Prelude.Hashable CriteriaForJob where
  hashWithSalt _salt CriteriaForJob' {..} =
    _salt
      `Prelude.hashWithSalt` simpleCriterion
      `Prelude.hashWithSalt` tagCriterion

instance Prelude.NFData CriteriaForJob where
  rnf CriteriaForJob' {..} =
    Prelude.rnf simpleCriterion
      `Prelude.seq` Prelude.rnf tagCriterion

instance Data.ToJSON CriteriaForJob where
  toJSON CriteriaForJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("simpleCriterion" Data..=)
              Prelude.<$> simpleCriterion,
            ("tagCriterion" Data..=) Prelude.<$> tagCriterion
          ]
      )
