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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.AwsEcrContainerAggregation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.AwsEcrContainerSortBy
import Amazonka.Inspector2.Types.SortOrder
import Amazonka.Inspector2.Types.StringFilter
import qualified Amazonka.Prelude as Prelude

-- | An aggregation of information about Amazon ECR containers.
--
-- /See:/ 'newAwsEcrContainerAggregation' smart constructor.
data AwsEcrContainerAggregation = AwsEcrContainerAggregation'
  { -- | The architecture of the containers.
    architectures :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | The image SHA values.
    imageShas :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | The image tags.
    imageTags :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | The container repositories.
    repositories :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | The container resource IDs.
    resourceIds :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | The value to sort by.
    sortBy :: Prelude.Maybe AwsEcrContainerSortBy,
    -- | The sort order (ascending or descending).
    sortOrder :: Prelude.Maybe SortOrder
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
-- 'architectures', 'awsEcrContainerAggregation_architectures' - The architecture of the containers.
--
-- 'imageShas', 'awsEcrContainerAggregation_imageShas' - The image SHA values.
--
-- 'imageTags', 'awsEcrContainerAggregation_imageTags' - The image tags.
--
-- 'repositories', 'awsEcrContainerAggregation_repositories' - The container repositories.
--
-- 'resourceIds', 'awsEcrContainerAggregation_resourceIds' - The container resource IDs.
--
-- 'sortBy', 'awsEcrContainerAggregation_sortBy' - The value to sort by.
--
-- 'sortOrder', 'awsEcrContainerAggregation_sortOrder' - The sort order (ascending or descending).
newAwsEcrContainerAggregation ::
  AwsEcrContainerAggregation
newAwsEcrContainerAggregation =
  AwsEcrContainerAggregation'
    { architectures =
        Prelude.Nothing,
      imageShas = Prelude.Nothing,
      imageTags = Prelude.Nothing,
      repositories = Prelude.Nothing,
      resourceIds = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | The architecture of the containers.
awsEcrContainerAggregation_architectures :: Lens.Lens' AwsEcrContainerAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
awsEcrContainerAggregation_architectures = Lens.lens (\AwsEcrContainerAggregation' {architectures} -> architectures) (\s@AwsEcrContainerAggregation' {} a -> s {architectures = a} :: AwsEcrContainerAggregation) Prelude.. Lens.mapping Lens.coerced

-- | The image SHA values.
awsEcrContainerAggregation_imageShas :: Lens.Lens' AwsEcrContainerAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
awsEcrContainerAggregation_imageShas = Lens.lens (\AwsEcrContainerAggregation' {imageShas} -> imageShas) (\s@AwsEcrContainerAggregation' {} a -> s {imageShas = a} :: AwsEcrContainerAggregation) Prelude.. Lens.mapping Lens.coerced

-- | The image tags.
awsEcrContainerAggregation_imageTags :: Lens.Lens' AwsEcrContainerAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
awsEcrContainerAggregation_imageTags = Lens.lens (\AwsEcrContainerAggregation' {imageTags} -> imageTags) (\s@AwsEcrContainerAggregation' {} a -> s {imageTags = a} :: AwsEcrContainerAggregation) Prelude.. Lens.mapping Lens.coerced

-- | The container repositories.
awsEcrContainerAggregation_repositories :: Lens.Lens' AwsEcrContainerAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
awsEcrContainerAggregation_repositories = Lens.lens (\AwsEcrContainerAggregation' {repositories} -> repositories) (\s@AwsEcrContainerAggregation' {} a -> s {repositories = a} :: AwsEcrContainerAggregation) Prelude.. Lens.mapping Lens.coerced

-- | The container resource IDs.
awsEcrContainerAggregation_resourceIds :: Lens.Lens' AwsEcrContainerAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
awsEcrContainerAggregation_resourceIds = Lens.lens (\AwsEcrContainerAggregation' {resourceIds} -> resourceIds) (\s@AwsEcrContainerAggregation' {} a -> s {resourceIds = a} :: AwsEcrContainerAggregation) Prelude.. Lens.mapping Lens.coerced

-- | The value to sort by.
awsEcrContainerAggregation_sortBy :: Lens.Lens' AwsEcrContainerAggregation (Prelude.Maybe AwsEcrContainerSortBy)
awsEcrContainerAggregation_sortBy = Lens.lens (\AwsEcrContainerAggregation' {sortBy} -> sortBy) (\s@AwsEcrContainerAggregation' {} a -> s {sortBy = a} :: AwsEcrContainerAggregation)

-- | The sort order (ascending or descending).
awsEcrContainerAggregation_sortOrder :: Lens.Lens' AwsEcrContainerAggregation (Prelude.Maybe SortOrder)
awsEcrContainerAggregation_sortOrder = Lens.lens (\AwsEcrContainerAggregation' {sortOrder} -> sortOrder) (\s@AwsEcrContainerAggregation' {} a -> s {sortOrder = a} :: AwsEcrContainerAggregation)

instance Prelude.Hashable AwsEcrContainerAggregation where
  hashWithSalt _salt AwsEcrContainerAggregation' {..} =
    _salt `Prelude.hashWithSalt` architectures
      `Prelude.hashWithSalt` imageShas
      `Prelude.hashWithSalt` imageTags
      `Prelude.hashWithSalt` repositories
      `Prelude.hashWithSalt` resourceIds
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData AwsEcrContainerAggregation where
  rnf AwsEcrContainerAggregation' {..} =
    Prelude.rnf architectures
      `Prelude.seq` Prelude.rnf imageShas
      `Prelude.seq` Prelude.rnf imageTags
      `Prelude.seq` Prelude.rnf repositories
      `Prelude.seq` Prelude.rnf resourceIds
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToJSON AwsEcrContainerAggregation where
  toJSON AwsEcrContainerAggregation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("architectures" Data..=) Prelude.<$> architectures,
            ("imageShas" Data..=) Prelude.<$> imageShas,
            ("imageTags" Data..=) Prelude.<$> imageTags,
            ("repositories" Data..=) Prelude.<$> repositories,
            ("resourceIds" Data..=) Prelude.<$> resourceIds,
            ("sortBy" Data..=) Prelude.<$> sortBy,
            ("sortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )
