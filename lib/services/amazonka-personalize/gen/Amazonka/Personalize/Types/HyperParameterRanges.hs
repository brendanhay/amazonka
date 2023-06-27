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
-- Module      : Amazonka.Personalize.Types.HyperParameterRanges
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.HyperParameterRanges where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types.CategoricalHyperParameterRange
import Amazonka.Personalize.Types.ContinuousHyperParameterRange
import Amazonka.Personalize.Types.IntegerHyperParameterRange
import qualified Amazonka.Prelude as Prelude

-- | Specifies the hyperparameters and their ranges. Hyperparameters can be
-- categorical, continuous, or integer-valued.
--
-- /See:/ 'newHyperParameterRanges' smart constructor.
data HyperParameterRanges = HyperParameterRanges'
  { -- | The categorical hyperparameters and their ranges.
    categoricalHyperParameterRanges :: Prelude.Maybe [CategoricalHyperParameterRange],
    -- | The continuous hyperparameters and their ranges.
    continuousHyperParameterRanges :: Prelude.Maybe [ContinuousHyperParameterRange],
    -- | The integer-valued hyperparameters and their ranges.
    integerHyperParameterRanges :: Prelude.Maybe [IntegerHyperParameterRange]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HyperParameterRanges' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoricalHyperParameterRanges', 'hyperParameterRanges_categoricalHyperParameterRanges' - The categorical hyperparameters and their ranges.
--
-- 'continuousHyperParameterRanges', 'hyperParameterRanges_continuousHyperParameterRanges' - The continuous hyperparameters and their ranges.
--
-- 'integerHyperParameterRanges', 'hyperParameterRanges_integerHyperParameterRanges' - The integer-valued hyperparameters and their ranges.
newHyperParameterRanges ::
  HyperParameterRanges
newHyperParameterRanges =
  HyperParameterRanges'
    { categoricalHyperParameterRanges =
        Prelude.Nothing,
      continuousHyperParameterRanges = Prelude.Nothing,
      integerHyperParameterRanges = Prelude.Nothing
    }

-- | The categorical hyperparameters and their ranges.
hyperParameterRanges_categoricalHyperParameterRanges :: Lens.Lens' HyperParameterRanges (Prelude.Maybe [CategoricalHyperParameterRange])
hyperParameterRanges_categoricalHyperParameterRanges = Lens.lens (\HyperParameterRanges' {categoricalHyperParameterRanges} -> categoricalHyperParameterRanges) (\s@HyperParameterRanges' {} a -> s {categoricalHyperParameterRanges = a} :: HyperParameterRanges) Prelude.. Lens.mapping Lens.coerced

-- | The continuous hyperparameters and their ranges.
hyperParameterRanges_continuousHyperParameterRanges :: Lens.Lens' HyperParameterRanges (Prelude.Maybe [ContinuousHyperParameterRange])
hyperParameterRanges_continuousHyperParameterRanges = Lens.lens (\HyperParameterRanges' {continuousHyperParameterRanges} -> continuousHyperParameterRanges) (\s@HyperParameterRanges' {} a -> s {continuousHyperParameterRanges = a} :: HyperParameterRanges) Prelude.. Lens.mapping Lens.coerced

-- | The integer-valued hyperparameters and their ranges.
hyperParameterRanges_integerHyperParameterRanges :: Lens.Lens' HyperParameterRanges (Prelude.Maybe [IntegerHyperParameterRange])
hyperParameterRanges_integerHyperParameterRanges = Lens.lens (\HyperParameterRanges' {integerHyperParameterRanges} -> integerHyperParameterRanges) (\s@HyperParameterRanges' {} a -> s {integerHyperParameterRanges = a} :: HyperParameterRanges) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON HyperParameterRanges where
  parseJSON =
    Data.withObject
      "HyperParameterRanges"
      ( \x ->
          HyperParameterRanges'
            Prelude.<$> ( x
                            Data..:? "categoricalHyperParameterRanges"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "continuousHyperParameterRanges"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "integerHyperParameterRanges"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable HyperParameterRanges where
  hashWithSalt _salt HyperParameterRanges' {..} =
    _salt
      `Prelude.hashWithSalt` categoricalHyperParameterRanges
      `Prelude.hashWithSalt` continuousHyperParameterRanges
      `Prelude.hashWithSalt` integerHyperParameterRanges

instance Prelude.NFData HyperParameterRanges where
  rnf HyperParameterRanges' {..} =
    Prelude.rnf categoricalHyperParameterRanges
      `Prelude.seq` Prelude.rnf continuousHyperParameterRanges
      `Prelude.seq` Prelude.rnf integerHyperParameterRanges

instance Data.ToJSON HyperParameterRanges where
  toJSON HyperParameterRanges' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("categoricalHyperParameterRanges" Data..=)
              Prelude.<$> categoricalHyperParameterRanges,
            ("continuousHyperParameterRanges" Data..=)
              Prelude.<$> continuousHyperParameterRanges,
            ("integerHyperParameterRanges" Data..=)
              Prelude.<$> integerHyperParameterRanges
          ]
      )
