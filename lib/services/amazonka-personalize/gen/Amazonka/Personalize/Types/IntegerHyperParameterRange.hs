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
-- Module      : Amazonka.Personalize.Types.IntegerHyperParameterRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.IntegerHyperParameterRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides the name and range of an integer-valued hyperparameter.
--
-- /See:/ 'newIntegerHyperParameterRange' smart constructor.
data IntegerHyperParameterRange = IntegerHyperParameterRange'
  { -- | The maximum allowable value for the hyperparameter.
    maxValue :: Prelude.Maybe Prelude.Int,
    -- | The name of the hyperparameter.
    name :: Prelude.Maybe Prelude.Text,
    -- | The minimum allowable value for the hyperparameter.
    minValue :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntegerHyperParameterRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxValue', 'integerHyperParameterRange_maxValue' - The maximum allowable value for the hyperparameter.
--
-- 'name', 'integerHyperParameterRange_name' - The name of the hyperparameter.
--
-- 'minValue', 'integerHyperParameterRange_minValue' - The minimum allowable value for the hyperparameter.
newIntegerHyperParameterRange ::
  IntegerHyperParameterRange
newIntegerHyperParameterRange =
  IntegerHyperParameterRange'
    { maxValue =
        Prelude.Nothing,
      name = Prelude.Nothing,
      minValue = Prelude.Nothing
    }

-- | The maximum allowable value for the hyperparameter.
integerHyperParameterRange_maxValue :: Lens.Lens' IntegerHyperParameterRange (Prelude.Maybe Prelude.Int)
integerHyperParameterRange_maxValue = Lens.lens (\IntegerHyperParameterRange' {maxValue} -> maxValue) (\s@IntegerHyperParameterRange' {} a -> s {maxValue = a} :: IntegerHyperParameterRange)

-- | The name of the hyperparameter.
integerHyperParameterRange_name :: Lens.Lens' IntegerHyperParameterRange (Prelude.Maybe Prelude.Text)
integerHyperParameterRange_name = Lens.lens (\IntegerHyperParameterRange' {name} -> name) (\s@IntegerHyperParameterRange' {} a -> s {name = a} :: IntegerHyperParameterRange)

-- | The minimum allowable value for the hyperparameter.
integerHyperParameterRange_minValue :: Lens.Lens' IntegerHyperParameterRange (Prelude.Maybe Prelude.Int)
integerHyperParameterRange_minValue = Lens.lens (\IntegerHyperParameterRange' {minValue} -> minValue) (\s@IntegerHyperParameterRange' {} a -> s {minValue = a} :: IntegerHyperParameterRange)

instance Core.FromJSON IntegerHyperParameterRange where
  parseJSON =
    Core.withObject
      "IntegerHyperParameterRange"
      ( \x ->
          IntegerHyperParameterRange'
            Prelude.<$> (x Core..:? "maxValue")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "minValue")
      )

instance Prelude.Hashable IntegerHyperParameterRange where
  hashWithSalt _salt IntegerHyperParameterRange' {..} =
    _salt `Prelude.hashWithSalt` maxValue
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` minValue

instance Prelude.NFData IntegerHyperParameterRange where
  rnf IntegerHyperParameterRange' {..} =
    Prelude.rnf maxValue
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf minValue

instance Core.ToJSON IntegerHyperParameterRange where
  toJSON IntegerHyperParameterRange' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("maxValue" Core..=) Prelude.<$> maxValue,
            ("name" Core..=) Prelude.<$> name,
            ("minValue" Core..=) Prelude.<$> minValue
          ]
      )
