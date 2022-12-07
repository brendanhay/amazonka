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
-- Module      : Amazonka.Evidently.Types.Variation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.Variation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types.VariableValue
import qualified Amazonka.Prelude as Prelude

-- | This structure contains the name and variation value of one variation of
-- a feature.
--
-- /See:/ 'newVariation' smart constructor.
data Variation = Variation'
  { -- | The name of the variation.
    name :: Prelude.Maybe Prelude.Text,
    -- | The value assigned to this variation.
    value :: Prelude.Maybe VariableValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Variation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'variation_name' - The name of the variation.
--
-- 'value', 'variation_value' - The value assigned to this variation.
newVariation ::
  Variation
newVariation =
  Variation'
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the variation.
variation_name :: Lens.Lens' Variation (Prelude.Maybe Prelude.Text)
variation_name = Lens.lens (\Variation' {name} -> name) (\s@Variation' {} a -> s {name = a} :: Variation)

-- | The value assigned to this variation.
variation_value :: Lens.Lens' Variation (Prelude.Maybe VariableValue)
variation_value = Lens.lens (\Variation' {value} -> value) (\s@Variation' {} a -> s {value = a} :: Variation)

instance Data.FromJSON Variation where
  parseJSON =
    Data.withObject
      "Variation"
      ( \x ->
          Variation'
            Prelude.<$> (x Data..:? "name") Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable Variation where
  hashWithSalt _salt Variation' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData Variation where
  rnf Variation' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value
