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
-- Module      : Amazonka.Inspector2.Types.AwsEcrContainerAggregation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.AwsEcrContainerAggregation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Inspector2.Types.AwsEcrContainerSortBy
import Amazonka.Inspector2.Types.SortOrder
import Amazonka.Inspector2.Types.StringFilter
import qualified Amazonka.Prelude as Prelude

-- | An aggregation of information about Amazon ECR containers.
--
-- /See:/ 'newAwsEcrContainerAggregation' smart constructor.
data AwsEcrContainerAggregation = AwsEcrContainerAggregation'
  { -- | The sort order (ascending or descending).
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The image SHA values.
    imageShas :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | The value to sort by.
    sortBy :: Prelude.Maybe AwsEcrContainerSortBy,
    -- | The container repositories.
    repositories :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | The container resource IDs.
    resourceIds :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | The image tags.
    imageTags :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | The architecture of the containers.
    architectures :: Prelude.Maybe (Prelude.NonEmpty StringFilter)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcrContainerAggregation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'awsEcrContainerAggregation_sortOrder' - The sort order (ascending or descending).
--
-- 'imageShas', 'awsEcrContainerAggregation_imageShas' - The image SHA values.
--
-- 'sortBy', 'awsEcrContainerAggregation_sortBy' - The value to sort by.
--
-- 'repositories', 'awsEcrContainerAggregation_repositories' - The container repositories.
--
-- 'resourceIds', 'awsEcrContainerAggregation_resourceIds' - The container resource IDs.
--
-- 'imageTags', 'awsEcrContainerAggregation_imageTags' - The image tags.
--
-- 'architectures', 'awsEcrContainerAggregation_architectures' - The architecture of the containers.
newAwsEcrContainerAggregation ::
  AwsEcrContainerAggregation
newAwsEcrContainerAggregation =
  AwsEcrContainerAggregation'
    { sortOrder =
        Prelude.Nothing,
      imageShas = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      repositories = Prelude.Nothing,
      resourceIds = Prelude.Nothing,
      imageTags = Prelude.Nothing,
      architectures = Prelude.Nothing
    }

-- | The sort order (ascending or descending).
awsEcrContainerAggregation_sortOrder :: Lens.Lens' AwsEcrContainerAggregation (Prelude.Maybe SortOrder)
awsEcrContainerAggregation_sortOrder = Lens.lens (\AwsEcrContainerAggregation' {sortOrder} -> sortOrder) (\s@AwsEcrContainerAggregation' {} a -> s {sortOrder = a} :: AwsEcrContainerAggregation)

-- | The image SHA values.
awsEcrContainerAggregation_imageShas :: Lens.Lens' AwsEcrContainerAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
awsEcrContainerAggregation_imageShas = Lens.lens (\AwsEcrContainerAggregation' {imageShas} -> imageShas) (\s@AwsEcrContainerAggregation' {} a -> s {imageShas = a} :: AwsEcrContainerAggregation) Prelude.. Lens.mapping Lens.coerced

-- | The value to sort by.
awsEcrContainerAggregation_sortBy :: Lens.Lens' AwsEcrContainerAggregation (Prelude.Maybe AwsEcrContainerSortBy)
awsEcrContainerAggregation_sortBy = Lens.lens (\AwsEcrContainerAggregation' {sortBy} -> sortBy) (\s@AwsEcrContainerAggregation' {} a -> s {sortBy = a} :: AwsEcrContainerAggregation)

-- | The container repositories.
awsEcrContainerAggregation_repositories :: Lens.Lens' AwsEcrContainerAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
awsEcrContainerAggregation_repositories = Lens.lens (\AwsEcrContainerAggregation' {repositories} -> repositories) (\s@AwsEcrContainerAggregation' {} a -> s {repositories = a} :: AwsEcrContainerAggregation) Prelude.. Lens.mapping Lens.coerced

-- | The container resource IDs.
awsEcrContainerAggregation_resourceIds :: Lens.Lens' AwsEcrContainerAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
awsEcrContainerAggregation_resourceIds = Lens.lens (\AwsEcrContainerAggregation' {resourceIds} -> resourceIds) (\s@AwsEcrContainerAggregation' {} a -> s {resourceIds = a} :: AwsEcrContainerAggregation) Prelude.. Lens.mapping Lens.coerced

-- | The image tags.
awsEcrContainerAggregation_imageTags :: Lens.Lens' AwsEcrContainerAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
awsEcrContainerAggregation_imageTags = Lens.lens (\AwsEcrContainerAggregation' {imageTags} -> imageTags) (\s@AwsEcrContainerAggregation' {} a -> s {imageTags = a} :: AwsEcrContainerAggregation) Prelude.. Lens.mapping Lens.coerced

-- | The architecture of the containers.
awsEcrContainerAggregation_architectures :: Lens.Lens' AwsEcrContainerAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
awsEcrContainerAggregation_architectures = Lens.lens (\AwsEcrContainerAggregation' {architectures} -> architectures) (\s@AwsEcrContainerAggregation' {} a -> s {architectures = a} :: AwsEcrContainerAggregation) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable AwsEcrContainerAggregation where
  hashWithSalt _salt AwsEcrContainerAggregation' {..} =
    _salt `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` imageShas
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` repositories
      `Prelude.hashWithSalt` resourceIds
      `Prelude.hashWithSalt` imageTags
      `Prelude.hashWithSalt` architectures

instance Prelude.NFData AwsEcrContainerAggregation where
  rnf AwsEcrContainerAggregation' {..} =
    Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf imageShas
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf repositories
      `Prelude.seq` Prelude.rnf resourceIds
      `Prelude.seq` Prelude.rnf imageTags
      `Prelude.seq` Prelude.rnf architectures

instance Core.ToJSON AwsEcrContainerAggregation where
  toJSON AwsEcrContainerAggregation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("sortOrder" Core..=) Prelude.<$> sortOrder,
            ("imageShas" Core..=) Prelude.<$> imageShas,
            ("sortBy" Core..=) Prelude.<$> sortBy,
            ("repositories" Core..=) Prelude.<$> repositories,
            ("resourceIds" Core..=) Prelude.<$> resourceIds,
            ("imageTags" Core..=) Prelude.<$> imageTags,
            ("architectures" Core..=) Prelude.<$> architectures
          ]
      )
