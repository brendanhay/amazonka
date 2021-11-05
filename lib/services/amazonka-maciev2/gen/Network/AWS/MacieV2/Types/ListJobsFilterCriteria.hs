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
-- Module      : Network.AWS.MacieV2.Types.ListJobsFilterCriteria
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MacieV2.Types.ListJobsFilterCriteria where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MacieV2.Types.ListJobsFilterTerm
import qualified Network.AWS.Prelude as Prelude

-- | Specifies criteria for filtering the results of a request for
-- information about classification jobs.
--
-- /See:/ 'newListJobsFilterCriteria' smart constructor.
data ListJobsFilterCriteria = ListJobsFilterCriteria'
  { -- | An array of objects, one for each condition that determines which jobs
    -- to include in the results.
    includes :: Prelude.Maybe [ListJobsFilterTerm],
    -- | An array of objects, one for each condition that determines which jobs
    -- to exclude from the results.
    excludes :: Prelude.Maybe [ListJobsFilterTerm]
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
-- 'includes', 'listJobsFilterCriteria_includes' - An array of objects, one for each condition that determines which jobs
-- to include in the results.
--
-- 'excludes', 'listJobsFilterCriteria_excludes' - An array of objects, one for each condition that determines which jobs
-- to exclude from the results.
newListJobsFilterCriteria ::
  ListJobsFilterCriteria
newListJobsFilterCriteria =
  ListJobsFilterCriteria'
    { includes = Prelude.Nothing,
      excludes = Prelude.Nothing
    }

-- | An array of objects, one for each condition that determines which jobs
-- to include in the results.
listJobsFilterCriteria_includes :: Lens.Lens' ListJobsFilterCriteria (Prelude.Maybe [ListJobsFilterTerm])
listJobsFilterCriteria_includes = Lens.lens (\ListJobsFilterCriteria' {includes} -> includes) (\s@ListJobsFilterCriteria' {} a -> s {includes = a} :: ListJobsFilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | An array of objects, one for each condition that determines which jobs
-- to exclude from the results.
listJobsFilterCriteria_excludes :: Lens.Lens' ListJobsFilterCriteria (Prelude.Maybe [ListJobsFilterTerm])
listJobsFilterCriteria_excludes = Lens.lens (\ListJobsFilterCriteria' {excludes} -> excludes) (\s@ListJobsFilterCriteria' {} a -> s {excludes = a} :: ListJobsFilterCriteria) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ListJobsFilterCriteria

instance Prelude.NFData ListJobsFilterCriteria

instance Core.ToJSON ListJobsFilterCriteria where
  toJSON ListJobsFilterCriteria' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("includes" Core..=) Prelude.<$> includes,
            ("excludes" Core..=) Prelude.<$> excludes
          ]
      )
