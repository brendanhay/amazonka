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
-- Module      : Amazonka.MacieV2.Types.CriteriaBlockForJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.CriteriaBlockForJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.CriteriaForJob
import qualified Amazonka.Prelude as Prelude

-- | Specifies one or more property- and tag-based conditions that define
-- criteria for including or excluding S3 buckets from a classification
-- job.
--
-- /See:/ 'newCriteriaBlockForJob' smart constructor.
data CriteriaBlockForJob = CriteriaBlockForJob'
  { -- | An array of conditions, one for each condition that determines which
    -- buckets to include or exclude from the job. If you specify more than one
    -- condition, Amazon Macie uses AND logic to join the conditions.
    and :: Prelude.Maybe [CriteriaForJob]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CriteriaBlockForJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'and', 'criteriaBlockForJob_and' - An array of conditions, one for each condition that determines which
-- buckets to include or exclude from the job. If you specify more than one
-- condition, Amazon Macie uses AND logic to join the conditions.
newCriteriaBlockForJob ::
  CriteriaBlockForJob
newCriteriaBlockForJob =
  CriteriaBlockForJob' {and = Prelude.Nothing}

-- | An array of conditions, one for each condition that determines which
-- buckets to include or exclude from the job. If you specify more than one
-- condition, Amazon Macie uses AND logic to join the conditions.
criteriaBlockForJob_and :: Lens.Lens' CriteriaBlockForJob (Prelude.Maybe [CriteriaForJob])
criteriaBlockForJob_and = Lens.lens (\CriteriaBlockForJob' {and} -> and) (\s@CriteriaBlockForJob' {} a -> s {and = a} :: CriteriaBlockForJob) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CriteriaBlockForJob where
  parseJSON =
    Data.withObject
      "CriteriaBlockForJob"
      ( \x ->
          CriteriaBlockForJob'
            Prelude.<$> (x Data..:? "and" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable CriteriaBlockForJob where
  hashWithSalt _salt CriteriaBlockForJob' {..} =
    _salt `Prelude.hashWithSalt` and

instance Prelude.NFData CriteriaBlockForJob where
  rnf CriteriaBlockForJob' {..} = Prelude.rnf and

instance Data.ToJSON CriteriaBlockForJob where
  toJSON CriteriaBlockForJob' {..} =
    Data.object
      (Prelude.catMaybes [("and" Data..=) Prelude.<$> and])
