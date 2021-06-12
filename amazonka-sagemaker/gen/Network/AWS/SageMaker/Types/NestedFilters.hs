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
-- Module      : Network.AWS.SageMaker.Types.NestedFilters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NestedFilters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.Filter

-- | A list of nested Filter objects. A resource must satisfy the conditions
-- of all filters to be included in the results returned from the Search
-- API.
--
-- For example, to filter on a training job\'s @InputDataConfig@ property
-- with a specific channel name and @S3Uri@ prefix, define the following
-- filters:
--
-- -   @\'{Name:\"InputDataConfig.ChannelName\", \"Operator\":\"Equals\", \"Value\":\"train\"}\',@
--
-- -   @\'{Name:\"InputDataConfig.DataSource.S3DataSource.S3Uri\", \"Operator\":\"Contains\", \"Value\":\"mybucket\/catdata\"}\'@
--
-- /See:/ 'newNestedFilters' smart constructor.
data NestedFilters = NestedFilters'
  { -- | The name of the property to use in the nested filters. The value must
    -- match a listed property name, such as @InputDataConfig@.
    nestedPropertyName :: Core.Text,
    -- | A list of filters. Each filter acts on a property. Filters must contain
    -- at least one @Filters@ value. For example, a @NestedFilters@ call might
    -- include a filter on the @PropertyName@ parameter of the
    -- @InputDataConfig@ property:
    -- @InputDataConfig.DataSource.S3DataSource.S3Uri@.
    filters :: Core.NonEmpty Filter
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NestedFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nestedPropertyName', 'nestedFilters_nestedPropertyName' - The name of the property to use in the nested filters. The value must
-- match a listed property name, such as @InputDataConfig@.
--
-- 'filters', 'nestedFilters_filters' - A list of filters. Each filter acts on a property. Filters must contain
-- at least one @Filters@ value. For example, a @NestedFilters@ call might
-- include a filter on the @PropertyName@ parameter of the
-- @InputDataConfig@ property:
-- @InputDataConfig.DataSource.S3DataSource.S3Uri@.
newNestedFilters ::
  -- | 'nestedPropertyName'
  Core.Text ->
  -- | 'filters'
  Core.NonEmpty Filter ->
  NestedFilters
newNestedFilters pNestedPropertyName_ pFilters_ =
  NestedFilters'
    { nestedPropertyName =
        pNestedPropertyName_,
      filters = Lens._Coerce Lens.# pFilters_
    }

-- | The name of the property to use in the nested filters. The value must
-- match a listed property name, such as @InputDataConfig@.
nestedFilters_nestedPropertyName :: Lens.Lens' NestedFilters Core.Text
nestedFilters_nestedPropertyName = Lens.lens (\NestedFilters' {nestedPropertyName} -> nestedPropertyName) (\s@NestedFilters' {} a -> s {nestedPropertyName = a} :: NestedFilters)

-- | A list of filters. Each filter acts on a property. Filters must contain
-- at least one @Filters@ value. For example, a @NestedFilters@ call might
-- include a filter on the @PropertyName@ parameter of the
-- @InputDataConfig@ property:
-- @InputDataConfig.DataSource.S3DataSource.S3Uri@.
nestedFilters_filters :: Lens.Lens' NestedFilters (Core.NonEmpty Filter)
nestedFilters_filters = Lens.lens (\NestedFilters' {filters} -> filters) (\s@NestedFilters' {} a -> s {filters = a} :: NestedFilters) Core.. Lens._Coerce

instance Core.Hashable NestedFilters

instance Core.NFData NestedFilters

instance Core.ToJSON NestedFilters where
  toJSON NestedFilters' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("NestedPropertyName" Core..= nestedPropertyName),
            Core.Just ("Filters" Core..= filters)
          ]
      )
