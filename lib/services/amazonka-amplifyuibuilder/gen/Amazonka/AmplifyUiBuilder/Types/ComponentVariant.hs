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
-- Module      : Amazonka.AmplifyUiBuilder.Types.ComponentVariant
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.ComponentVariant where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the style configuration of a unique variation of a main
-- component.
--
-- /See:/ 'newComponentVariant' smart constructor.
data ComponentVariant = ComponentVariant'
  { -- | The properties of the component variant that can be overriden when
    -- customizing an instance of the component. You can\'t specify @tags@ as a
    -- valid property for @overrides@.
    overrides :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The combination of variants that comprise this variant. You can\'t
    -- specify @tags@ as a valid property for @variantValues@.
    variantValues :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentVariant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'overrides', 'componentVariant_overrides' - The properties of the component variant that can be overriden when
-- customizing an instance of the component. You can\'t specify @tags@ as a
-- valid property for @overrides@.
--
-- 'variantValues', 'componentVariant_variantValues' - The combination of variants that comprise this variant. You can\'t
-- specify @tags@ as a valid property for @variantValues@.
newComponentVariant ::
  ComponentVariant
newComponentVariant =
  ComponentVariant'
    { overrides = Prelude.Nothing,
      variantValues = Prelude.Nothing
    }

-- | The properties of the component variant that can be overriden when
-- customizing an instance of the component. You can\'t specify @tags@ as a
-- valid property for @overrides@.
componentVariant_overrides :: Lens.Lens' ComponentVariant (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text)))
componentVariant_overrides = Lens.lens (\ComponentVariant' {overrides} -> overrides) (\s@ComponentVariant' {} a -> s {overrides = a} :: ComponentVariant) Prelude.. Lens.mapping Lens.coerced

-- | The combination of variants that comprise this variant. You can\'t
-- specify @tags@ as a valid property for @variantValues@.
componentVariant_variantValues :: Lens.Lens' ComponentVariant (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
componentVariant_variantValues = Lens.lens (\ComponentVariant' {variantValues} -> variantValues) (\s@ComponentVariant' {} a -> s {variantValues = a} :: ComponentVariant) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ComponentVariant where
  parseJSON =
    Data.withObject
      "ComponentVariant"
      ( \x ->
          ComponentVariant'
            Prelude.<$> (x Data..:? "overrides" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "variantValues" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ComponentVariant where
  hashWithSalt _salt ComponentVariant' {..} =
    _salt
      `Prelude.hashWithSalt` overrides
      `Prelude.hashWithSalt` variantValues

instance Prelude.NFData ComponentVariant where
  rnf ComponentVariant' {..} =
    Prelude.rnf overrides `Prelude.seq`
      Prelude.rnf variantValues

instance Data.ToJSON ComponentVariant where
  toJSON ComponentVariant' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("overrides" Data..=) Prelude.<$> overrides,
            ("variantValues" Data..=) Prelude.<$> variantValues
          ]
      )
