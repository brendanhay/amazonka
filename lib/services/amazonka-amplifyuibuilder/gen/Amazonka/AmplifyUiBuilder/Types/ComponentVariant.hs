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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.ComponentVariant where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the style configuration of a unique variation of a main
-- component.
--
-- /See:/ 'newComponentVariant' smart constructor.
data ComponentVariant = ComponentVariant'
  { -- | The combination of variants that comprise this variant. You can\'t
    -- specify @tags@ as a valid property for @variantValues@.
    variantValues :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The properties of the component variant that can be overriden when
    -- customizing an instance of the component. You can\'t specify @tags@ as a
    -- valid property for @overrides@.
    overrides :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text))
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
-- 'variantValues', 'componentVariant_variantValues' - The combination of variants that comprise this variant. You can\'t
-- specify @tags@ as a valid property for @variantValues@.
--
-- 'overrides', 'componentVariant_overrides' - The properties of the component variant that can be overriden when
-- customizing an instance of the component. You can\'t specify @tags@ as a
-- valid property for @overrides@.
newComponentVariant ::
  ComponentVariant
newComponentVariant =
  ComponentVariant'
    { variantValues = Prelude.Nothing,
      overrides = Prelude.Nothing
    }

-- | The combination of variants that comprise this variant. You can\'t
-- specify @tags@ as a valid property for @variantValues@.
componentVariant_variantValues :: Lens.Lens' ComponentVariant (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
componentVariant_variantValues = Lens.lens (\ComponentVariant' {variantValues} -> variantValues) (\s@ComponentVariant' {} a -> s {variantValues = a} :: ComponentVariant) Prelude.. Lens.mapping Lens.coerced

-- | The properties of the component variant that can be overriden when
-- customizing an instance of the component. You can\'t specify @tags@ as a
-- valid property for @overrides@.
componentVariant_overrides :: Lens.Lens' ComponentVariant (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text)))
componentVariant_overrides = Lens.lens (\ComponentVariant' {overrides} -> overrides) (\s@ComponentVariant' {} a -> s {overrides = a} :: ComponentVariant) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ComponentVariant where
  parseJSON =
    Core.withObject
      "ComponentVariant"
      ( \x ->
          ComponentVariant'
            Prelude.<$> (x Core..:? "variantValues" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "overrides" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ComponentVariant where
  hashWithSalt _salt ComponentVariant' {..} =
    _salt `Prelude.hashWithSalt` variantValues
      `Prelude.hashWithSalt` overrides

instance Prelude.NFData ComponentVariant where
  rnf ComponentVariant' {..} =
    Prelude.rnf variantValues
      `Prelude.seq` Prelude.rnf overrides

instance Core.ToJSON ComponentVariant where
  toJSON ComponentVariant' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("variantValues" Core..=) Prelude.<$> variantValues,
            ("overrides" Core..=) Prelude.<$> overrides
          ]
      )
