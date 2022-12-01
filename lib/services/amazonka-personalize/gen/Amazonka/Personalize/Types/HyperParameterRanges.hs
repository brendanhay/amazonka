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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.HyperParameterRanges where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Personalize.Types.CategoricalHyperParameterRange
import Amazonka.Personalize.Types.ContinuousHyperParameterRange
import Amazonka.Personalize.Types.IntegerHyperParameterRange
import qualified Amazonka.Prelude as Prelude

-- | Specifies the hyperparameters and their ranges. Hyperparameters can be
-- categorical, continuous, or integer-valued.
--
-- /See:/ 'newHyperParameterRanges' smart constructor.
data HyperParameterRanges = HyperParameterRanges'
  { -- | The integer-valued hyperparameters and their ranges.
    integerHyperParameterRanges :: Prelude.Maybe [IntegerHyperParameterRange],
    -- | The categorical hyperparameters and their ranges.
    categoricalHyperParameterRanges :: Prelude.Maybe [CategoricalHyperParameterRange],
    -- | The continuous hyperparameters and their ranges.
    continuousHyperParameterRanges :: Prelude.Maybe [ContinuousHyperParameterRange]
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
-- 'integerHyperParameterRanges', 'hyperParameterRanges_integerHyperParameterRanges' - The integer-valued hyperparameters and their ranges.
--
-- 'categoricalHyperParameterRanges', 'hyperParameterRanges_categoricalHyperParameterRanges' - The categorical hyperparameters and their ranges.
--
-- 'continuousHyperParameterRanges', 'hyperParameterRanges_continuousHyperParameterRanges' - The continuous hyperparameters and their ranges.
newHyperParameterRanges ::
  HyperParameterRanges
newHyperParameterRanges =
  HyperParameterRanges'
    { integerHyperParameterRanges =
        Prelude.Nothing,
      categoricalHyperParameterRanges = Prelude.Nothing,
      continuousHyperParameterRanges = Prelude.Nothing
    }

-- | The integer-valued hyperparameters and their ranges.
hyperParameterRanges_integerHyperParameterRanges :: Lens.Lens' HyperParameterRanges (Prelude.Maybe [IntegerHyperParameterRange])
hyperParameterRanges_integerHyperParameterRanges = Lens.lens (\HyperParameterRanges' {integerHyperParameterRanges} -> integerHyperParameterRanges) (\s@HyperParameterRanges' {} a -> s {integerHyperParameterRanges = a} :: HyperParameterRanges) Prelude.. Lens.mapping Lens.coerced

-- | The categorical hyperparameters and their ranges.
hyperParameterRanges_categoricalHyperParameterRanges :: Lens.Lens' HyperParameterRanges (Prelude.Maybe [CategoricalHyperParameterRange])
hyperParameterRanges_categoricalHyperParameterRanges = Lens.lens (\HyperParameterRanges' {categoricalHyperParameterRanges} -> categoricalHyperParameterRanges) (\s@HyperParameterRanges' {} a -> s {categoricalHyperParameterRanges = a} :: HyperParameterRanges) Prelude.. Lens.mapping Lens.coerced

-- | The continuous hyperparameters and their ranges.
hyperParameterRanges_continuousHyperParameterRanges :: Lens.Lens' HyperParameterRanges (Prelude.Maybe [ContinuousHyperParameterRange])
hyperParameterRanges_continuousHyperParameterRanges = Lens.lens (\HyperParameterRanges' {continuousHyperParameterRanges} -> continuousHyperParameterRanges) (\s@HyperParameterRanges' {} a -> s {continuousHyperParameterRanges = a} :: HyperParameterRanges) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON HyperParameterRanges where
  parseJSON =
    Core.withObject
      "HyperParameterRanges"
      ( \x ->
          HyperParameterRanges'
            Prelude.<$> ( x Core..:? "integerHyperParameterRanges"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "categoricalHyperParameterRanges"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "continuousHyperParameterRanges"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable HyperParameterRanges where
  hashWithSalt _salt HyperParameterRanges' {..} =
    _salt
      `Prelude.hashWithSalt` integerHyperParameterRanges
      `Prelude.hashWithSalt` categoricalHyperParameterRanges
      `Prelude.hashWithSalt` continuousHyperParameterRanges

instance Prelude.NFData HyperParameterRanges where
  rnf HyperParameterRanges' {..} =
    Prelude.rnf integerHyperParameterRanges
      `Prelude.seq` Prelude.rnf categoricalHyperParameterRanges
      `Prelude.seq` Prelude.rnf continuousHyperParameterRanges

instance Core.ToJSON HyperParameterRanges where
  toJSON HyperParameterRanges' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("integerHyperParameterRanges" Core..=)
              Prelude.<$> integerHyperParameterRanges,
            ("categoricalHyperParameterRanges" Core..=)
              Prelude.<$> categoricalHyperParameterRanges,
            ("continuousHyperParameterRanges" Core..=)
              Prelude.<$> continuousHyperParameterRanges
          ]
      )
