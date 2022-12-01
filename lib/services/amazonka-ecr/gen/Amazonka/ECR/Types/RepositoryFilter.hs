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
-- Module      : Amazonka.ECR.Types.RepositoryFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.RepositoryFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ECR.Types.RepositoryFilterType
import qualified Amazonka.Prelude as Prelude

-- | The filter settings used with image replication. Specifying a repository
-- filter to a replication rule provides a method for controlling which
-- repositories in a private registry are replicated. If no repository
-- filter is specified, all images in the repository are replicated.
--
-- /See:/ 'newRepositoryFilter' smart constructor.
data RepositoryFilter = RepositoryFilter'
  { -- | The repository filter details. When the @PREFIX_MATCH@ filter type is
    -- specified, this value is required and should be the repository name
    -- prefix to configure replication for.
    filter' :: Prelude.Text,
    -- | The repository filter type. The only supported value is @PREFIX_MATCH@,
    -- which is a repository name prefix specified with the @filter@ parameter.
    filterType :: RepositoryFilterType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RepositoryFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'repositoryFilter_filter' - The repository filter details. When the @PREFIX_MATCH@ filter type is
-- specified, this value is required and should be the repository name
-- prefix to configure replication for.
--
-- 'filterType', 'repositoryFilter_filterType' - The repository filter type. The only supported value is @PREFIX_MATCH@,
-- which is a repository name prefix specified with the @filter@ parameter.
newRepositoryFilter ::
  -- | 'filter''
  Prelude.Text ->
  -- | 'filterType'
  RepositoryFilterType ->
  RepositoryFilter
newRepositoryFilter pFilter_ pFilterType_ =
  RepositoryFilter'
    { filter' = pFilter_,
      filterType = pFilterType_
    }

-- | The repository filter details. When the @PREFIX_MATCH@ filter type is
-- specified, this value is required and should be the repository name
-- prefix to configure replication for.
repositoryFilter_filter :: Lens.Lens' RepositoryFilter Prelude.Text
repositoryFilter_filter = Lens.lens (\RepositoryFilter' {filter'} -> filter') (\s@RepositoryFilter' {} a -> s {filter' = a} :: RepositoryFilter)

-- | The repository filter type. The only supported value is @PREFIX_MATCH@,
-- which is a repository name prefix specified with the @filter@ parameter.
repositoryFilter_filterType :: Lens.Lens' RepositoryFilter RepositoryFilterType
repositoryFilter_filterType = Lens.lens (\RepositoryFilter' {filterType} -> filterType) (\s@RepositoryFilter' {} a -> s {filterType = a} :: RepositoryFilter)

instance Core.FromJSON RepositoryFilter where
  parseJSON =
    Core.withObject
      "RepositoryFilter"
      ( \x ->
          RepositoryFilter'
            Prelude.<$> (x Core..: "filter")
            Prelude.<*> (x Core..: "filterType")
      )

instance Prelude.Hashable RepositoryFilter where
  hashWithSalt _salt RepositoryFilter' {..} =
    _salt `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` filterType

instance Prelude.NFData RepositoryFilter where
  rnf RepositoryFilter' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf filterType

instance Core.ToJSON RepositoryFilter where
  toJSON RepositoryFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("filter" Core..= filter'),
            Prelude.Just ("filterType" Core..= filterType)
          ]
      )
