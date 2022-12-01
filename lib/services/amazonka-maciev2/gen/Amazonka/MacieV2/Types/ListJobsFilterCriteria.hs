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
-- Module      : Amazonka.MacieV2.Types.ListJobsFilterCriteria
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.ListJobsFilterCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MacieV2.Types.ListJobsFilterTerm
import qualified Amazonka.Prelude as Prelude

-- | Specifies criteria for filtering the results of a request for
-- information about classification jobs.
--
-- /See:/ 'newListJobsFilterCriteria' smart constructor.
data ListJobsFilterCriteria = ListJobsFilterCriteria'
  { -- | An array of objects, one for each condition that determines which jobs
    -- to exclude from the results.
    excludes :: Prelude.Maybe [ListJobsFilterTerm],
    -- | An array of objects, one for each condition that determines which jobs
    -- to include in the results.
    includes :: Prelude.Maybe [ListJobsFilterTerm]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJobsFilterCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excludes', 'listJobsFilterCriteria_excludes' - An array of objects, one for each condition that determines which jobs
-- to exclude from the results.
--
-- 'includes', 'listJobsFilterCriteria_includes' - An array of objects, one for each condition that determines which jobs
-- to include in the results.
newListJobsFilterCriteria ::
  ListJobsFilterCriteria
newListJobsFilterCriteria =
  ListJobsFilterCriteria'
    { excludes = Prelude.Nothing,
      includes = Prelude.Nothing
    }

-- | An array of objects, one for each condition that determines which jobs
-- to exclude from the results.
listJobsFilterCriteria_excludes :: Lens.Lens' ListJobsFilterCriteria (Prelude.Maybe [ListJobsFilterTerm])
listJobsFilterCriteria_excludes = Lens.lens (\ListJobsFilterCriteria' {excludes} -> excludes) (\s@ListJobsFilterCriteria' {} a -> s {excludes = a} :: ListJobsFilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | An array of objects, one for each condition that determines which jobs
-- to include in the results.
listJobsFilterCriteria_includes :: Lens.Lens' ListJobsFilterCriteria (Prelude.Maybe [ListJobsFilterTerm])
listJobsFilterCriteria_includes = Lens.lens (\ListJobsFilterCriteria' {includes} -> includes) (\s@ListJobsFilterCriteria' {} a -> s {includes = a} :: ListJobsFilterCriteria) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ListJobsFilterCriteria where
  hashWithSalt _salt ListJobsFilterCriteria' {..} =
    _salt `Prelude.hashWithSalt` excludes
      `Prelude.hashWithSalt` includes

instance Prelude.NFData ListJobsFilterCriteria where
  rnf ListJobsFilterCriteria' {..} =
    Prelude.rnf excludes
      `Prelude.seq` Prelude.rnf includes

instance Core.ToJSON ListJobsFilterCriteria where
  toJSON ListJobsFilterCriteria' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("excludes" Core..=) Prelude.<$> excludes,
            ("includes" Core..=) Prelude.<$> includes
          ]
      )
