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
-- Module      : Amazonka.SageMaker.Types.NestedFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.NestedFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.Filter

-- | A list of nested
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_Filter.html Filter>
-- objects. A resource must satisfy the conditions of all filters to be
-- included in the results returned from the
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_Search.html Search>
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
    nestedPropertyName :: Prelude.Text,
    -- | A list of filters. Each filter acts on a property. Filters must contain
    -- at least one @Filters@ value. For example, a @NestedFilters@ call might
    -- include a filter on the @PropertyName@ parameter of the
    -- @InputDataConfig@ property:
    -- @InputDataConfig.DataSource.S3DataSource.S3Uri@.
    filters :: Prelude.NonEmpty Filter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'filters'
  Prelude.NonEmpty Filter ->
  NestedFilters
newNestedFilters pNestedPropertyName_ pFilters_ =
  NestedFilters'
    { nestedPropertyName =
        pNestedPropertyName_,
      filters = Lens.coerced Lens.# pFilters_
    }

-- | The name of the property to use in the nested filters. The value must
-- match a listed property name, such as @InputDataConfig@.
nestedFilters_nestedPropertyName :: Lens.Lens' NestedFilters Prelude.Text
nestedFilters_nestedPropertyName = Lens.lens (\NestedFilters' {nestedPropertyName} -> nestedPropertyName) (\s@NestedFilters' {} a -> s {nestedPropertyName = a} :: NestedFilters)

-- | A list of filters. Each filter acts on a property. Filters must contain
-- at least one @Filters@ value. For example, a @NestedFilters@ call might
-- include a filter on the @PropertyName@ parameter of the
-- @InputDataConfig@ property:
-- @InputDataConfig.DataSource.S3DataSource.S3Uri@.
nestedFilters_filters :: Lens.Lens' NestedFilters (Prelude.NonEmpty Filter)
nestedFilters_filters = Lens.lens (\NestedFilters' {filters} -> filters) (\s@NestedFilters' {} a -> s {filters = a} :: NestedFilters) Prelude.. Lens.coerced

instance Prelude.Hashable NestedFilters where
  hashWithSalt _salt NestedFilters' {..} =
    _salt
      `Prelude.hashWithSalt` nestedPropertyName
      `Prelude.hashWithSalt` filters

instance Prelude.NFData NestedFilters where
  rnf NestedFilters' {..} =
    Prelude.rnf nestedPropertyName
      `Prelude.seq` Prelude.rnf filters

instance Data.ToJSON NestedFilters where
  toJSON NestedFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("NestedPropertyName" Data..= nestedPropertyName),
            Prelude.Just ("Filters" Data..= filters)
          ]
      )
