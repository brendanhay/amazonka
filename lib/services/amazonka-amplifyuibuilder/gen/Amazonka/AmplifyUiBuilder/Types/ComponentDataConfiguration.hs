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
-- Module      : Amazonka.AmplifyUiBuilder.Types.ComponentDataConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.ComponentDataConfiguration where

import Amazonka.AmplifyUiBuilder.Types.Predicate
import Amazonka.AmplifyUiBuilder.Types.SortProperty
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration for binding a component\'s properties to
-- data.
--
-- /See:/ 'newComponentDataConfiguration' smart constructor.
data ComponentDataConfiguration = ComponentDataConfiguration'
  { -- | A list of IDs to use to bind data to a component. Use this property to
    -- bind specifically chosen data, rather than data retrieved from a query.
    identifiers :: Prelude.Maybe [Prelude.Text],
    -- | Represents the conditional logic to use when binding data to a
    -- component. Use this property to retrieve only a subset of the data in a
    -- collection.
    predicate :: Prelude.Maybe Predicate,
    -- | Describes how to sort the component\'s properties.
    sort :: Prelude.Maybe [SortProperty],
    -- | The name of the data model to use to bind data to a component.
    model :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentDataConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifiers', 'componentDataConfiguration_identifiers' - A list of IDs to use to bind data to a component. Use this property to
-- bind specifically chosen data, rather than data retrieved from a query.
--
-- 'predicate', 'componentDataConfiguration_predicate' - Represents the conditional logic to use when binding data to a
-- component. Use this property to retrieve only a subset of the data in a
-- collection.
--
-- 'sort', 'componentDataConfiguration_sort' - Describes how to sort the component\'s properties.
--
-- 'model', 'componentDataConfiguration_model' - The name of the data model to use to bind data to a component.
newComponentDataConfiguration ::
  -- | 'model'
  Prelude.Text ->
  ComponentDataConfiguration
newComponentDataConfiguration pModel_ =
  ComponentDataConfiguration'
    { identifiers =
        Prelude.Nothing,
      predicate = Prelude.Nothing,
      sort = Prelude.Nothing,
      model = pModel_
    }

-- | A list of IDs to use to bind data to a component. Use this property to
-- bind specifically chosen data, rather than data retrieved from a query.
componentDataConfiguration_identifiers :: Lens.Lens' ComponentDataConfiguration (Prelude.Maybe [Prelude.Text])
componentDataConfiguration_identifiers = Lens.lens (\ComponentDataConfiguration' {identifiers} -> identifiers) (\s@ComponentDataConfiguration' {} a -> s {identifiers = a} :: ComponentDataConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Represents the conditional logic to use when binding data to a
-- component. Use this property to retrieve only a subset of the data in a
-- collection.
componentDataConfiguration_predicate :: Lens.Lens' ComponentDataConfiguration (Prelude.Maybe Predicate)
componentDataConfiguration_predicate = Lens.lens (\ComponentDataConfiguration' {predicate} -> predicate) (\s@ComponentDataConfiguration' {} a -> s {predicate = a} :: ComponentDataConfiguration)

-- | Describes how to sort the component\'s properties.
componentDataConfiguration_sort :: Lens.Lens' ComponentDataConfiguration (Prelude.Maybe [SortProperty])
componentDataConfiguration_sort = Lens.lens (\ComponentDataConfiguration' {sort} -> sort) (\s@ComponentDataConfiguration' {} a -> s {sort = a} :: ComponentDataConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the data model to use to bind data to a component.
componentDataConfiguration_model :: Lens.Lens' ComponentDataConfiguration Prelude.Text
componentDataConfiguration_model = Lens.lens (\ComponentDataConfiguration' {model} -> model) (\s@ComponentDataConfiguration' {} a -> s {model = a} :: ComponentDataConfiguration)

instance Data.FromJSON ComponentDataConfiguration where
  parseJSON =
    Data.withObject
      "ComponentDataConfiguration"
      ( \x ->
          ComponentDataConfiguration'
            Prelude.<$> (x Data..:? "identifiers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "predicate")
            Prelude.<*> (x Data..:? "sort" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "model")
      )

instance Prelude.Hashable ComponentDataConfiguration where
  hashWithSalt _salt ComponentDataConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` identifiers
      `Prelude.hashWithSalt` predicate
      `Prelude.hashWithSalt` sort
      `Prelude.hashWithSalt` model

instance Prelude.NFData ComponentDataConfiguration where
  rnf ComponentDataConfiguration' {..} =
    Prelude.rnf identifiers `Prelude.seq`
      Prelude.rnf predicate `Prelude.seq`
        Prelude.rnf sort `Prelude.seq`
          Prelude.rnf model

instance Data.ToJSON ComponentDataConfiguration where
  toJSON ComponentDataConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("identifiers" Data..=) Prelude.<$> identifiers,
            ("predicate" Data..=) Prelude.<$> predicate,
            ("sort" Data..=) Prelude.<$> sort,
            Prelude.Just ("model" Data..= model)
          ]
      )
