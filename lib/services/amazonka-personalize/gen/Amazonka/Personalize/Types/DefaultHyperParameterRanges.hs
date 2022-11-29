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
-- Module      : Amazonka.Personalize.Types.DefaultHyperParameterRanges
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.DefaultHyperParameterRanges where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Personalize.Types.DefaultCategoricalHyperParameterRange
import Amazonka.Personalize.Types.DefaultContinuousHyperParameterRange
import Amazonka.Personalize.Types.DefaultIntegerHyperParameterRange
import qualified Amazonka.Prelude as Prelude

-- | Specifies the hyperparameters and their default ranges. Hyperparameters
-- can be categorical, continuous, or integer-valued.
--
-- /See:/ 'newDefaultHyperParameterRanges' smart constructor.
data DefaultHyperParameterRanges = DefaultHyperParameterRanges'
  { -- | The integer-valued hyperparameters and their default ranges.
    integerHyperParameterRanges :: Prelude.Maybe [DefaultIntegerHyperParameterRange],
    -- | The categorical hyperparameters and their default ranges.
    categoricalHyperParameterRanges :: Prelude.Maybe [DefaultCategoricalHyperParameterRange],
    -- | The continuous hyperparameters and their default ranges.
    continuousHyperParameterRanges :: Prelude.Maybe [DefaultContinuousHyperParameterRange]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultHyperParameterRanges' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'integerHyperParameterRanges', 'defaultHyperParameterRanges_integerHyperParameterRanges' - The integer-valued hyperparameters and their default ranges.
--
-- 'categoricalHyperParameterRanges', 'defaultHyperParameterRanges_categoricalHyperParameterRanges' - The categorical hyperparameters and their default ranges.
--
-- 'continuousHyperParameterRanges', 'defaultHyperParameterRanges_continuousHyperParameterRanges' - The continuous hyperparameters and their default ranges.
newDefaultHyperParameterRanges ::
  DefaultHyperParameterRanges
newDefaultHyperParameterRanges =
  DefaultHyperParameterRanges'
    { integerHyperParameterRanges =
        Prelude.Nothing,
      categoricalHyperParameterRanges =
        Prelude.Nothing,
      continuousHyperParameterRanges =
        Prelude.Nothing
    }

-- | The integer-valued hyperparameters and their default ranges.
defaultHyperParameterRanges_integerHyperParameterRanges :: Lens.Lens' DefaultHyperParameterRanges (Prelude.Maybe [DefaultIntegerHyperParameterRange])
defaultHyperParameterRanges_integerHyperParameterRanges = Lens.lens (\DefaultHyperParameterRanges' {integerHyperParameterRanges} -> integerHyperParameterRanges) (\s@DefaultHyperParameterRanges' {} a -> s {integerHyperParameterRanges = a} :: DefaultHyperParameterRanges) Prelude.. Lens.mapping Lens.coerced

-- | The categorical hyperparameters and their default ranges.
defaultHyperParameterRanges_categoricalHyperParameterRanges :: Lens.Lens' DefaultHyperParameterRanges (Prelude.Maybe [DefaultCategoricalHyperParameterRange])
defaultHyperParameterRanges_categoricalHyperParameterRanges = Lens.lens (\DefaultHyperParameterRanges' {categoricalHyperParameterRanges} -> categoricalHyperParameterRanges) (\s@DefaultHyperParameterRanges' {} a -> s {categoricalHyperParameterRanges = a} :: DefaultHyperParameterRanges) Prelude.. Lens.mapping Lens.coerced

-- | The continuous hyperparameters and their default ranges.
defaultHyperParameterRanges_continuousHyperParameterRanges :: Lens.Lens' DefaultHyperParameterRanges (Prelude.Maybe [DefaultContinuousHyperParameterRange])
defaultHyperParameterRanges_continuousHyperParameterRanges = Lens.lens (\DefaultHyperParameterRanges' {continuousHyperParameterRanges} -> continuousHyperParameterRanges) (\s@DefaultHyperParameterRanges' {} a -> s {continuousHyperParameterRanges = a} :: DefaultHyperParameterRanges) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON DefaultHyperParameterRanges where
  parseJSON =
    Core.withObject
      "DefaultHyperParameterRanges"
      ( \x ->
          DefaultHyperParameterRanges'
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

instance Prelude.Hashable DefaultHyperParameterRanges where
  hashWithSalt _salt DefaultHyperParameterRanges' {..} =
    _salt
      `Prelude.hashWithSalt` integerHyperParameterRanges
      `Prelude.hashWithSalt` categoricalHyperParameterRanges
      `Prelude.hashWithSalt` continuousHyperParameterRanges

instance Prelude.NFData DefaultHyperParameterRanges where
  rnf DefaultHyperParameterRanges' {..} =
    Prelude.rnf integerHyperParameterRanges
      `Prelude.seq` Prelude.rnf categoricalHyperParameterRanges
      `Prelude.seq` Prelude.rnf continuousHyperParameterRanges
