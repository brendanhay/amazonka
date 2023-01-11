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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.PropertyFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.PropertyFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.LogicalOperator
import Amazonka.SageMakerGeoSpatial.Types.PropertyFilter

-- |
--
-- /See:/ 'newPropertyFilters' smart constructor.
data PropertyFilters = PropertyFilters'
  { logicalOperator :: Prelude.Maybe LogicalOperator,
    properties :: Prelude.Maybe [PropertyFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PropertyFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logicalOperator', 'propertyFilters_logicalOperator' -
--
-- 'properties', 'propertyFilters_properties' -
newPropertyFilters ::
  PropertyFilters
newPropertyFilters =
  PropertyFilters'
    { logicalOperator = Prelude.Nothing,
      properties = Prelude.Nothing
    }

-- |
propertyFilters_logicalOperator :: Lens.Lens' PropertyFilters (Prelude.Maybe LogicalOperator)
propertyFilters_logicalOperator = Lens.lens (\PropertyFilters' {logicalOperator} -> logicalOperator) (\s@PropertyFilters' {} a -> s {logicalOperator = a} :: PropertyFilters)

-- |
propertyFilters_properties :: Lens.Lens' PropertyFilters (Prelude.Maybe [PropertyFilter])
propertyFilters_properties = Lens.lens (\PropertyFilters' {properties} -> properties) (\s@PropertyFilters' {} a -> s {properties = a} :: PropertyFilters) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PropertyFilters where
  parseJSON =
    Data.withObject
      "PropertyFilters"
      ( \x ->
          PropertyFilters'
            Prelude.<$> (x Data..:? "LogicalOperator")
            Prelude.<*> (x Data..:? "Properties" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable PropertyFilters where
  hashWithSalt _salt PropertyFilters' {..} =
    _salt `Prelude.hashWithSalt` logicalOperator
      `Prelude.hashWithSalt` properties

instance Prelude.NFData PropertyFilters where
  rnf PropertyFilters' {..} =
    Prelude.rnf logicalOperator
      `Prelude.seq` Prelude.rnf properties

instance Data.ToJSON PropertyFilters where
  toJSON PropertyFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LogicalOperator" Data..=)
              Prelude.<$> logicalOperator,
            ("Properties" Data..=) Prelude.<$> properties
          ]
      )
