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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.IntegerHyperParameterRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the name and range of an integer-valued hyperparameter.
--
-- /See:/ 'newIntegerHyperParameterRange' smart constructor.
data IntegerHyperParameterRange = IntegerHyperParameterRange'
  { -- | The maximum allowable value for the hyperparameter.
    maxValue :: Prelude.Maybe Prelude.Int,
    -- | The minimum allowable value for the hyperparameter.
    minValue :: Prelude.Maybe Prelude.Int,
    -- | The name of the hyperparameter.
    name :: Prelude.Maybe Prelude.Text
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
-- 'minValue', 'integerHyperParameterRange_minValue' - The minimum allowable value for the hyperparameter.
--
-- 'name', 'integerHyperParameterRange_name' - The name of the hyperparameter.
newIntegerHyperParameterRange ::
  IntegerHyperParameterRange
newIntegerHyperParameterRange =
  IntegerHyperParameterRange'
    { maxValue =
        Prelude.Nothing,
      minValue = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The maximum allowable value for the hyperparameter.
integerHyperParameterRange_maxValue :: Lens.Lens' IntegerHyperParameterRange (Prelude.Maybe Prelude.Int)
integerHyperParameterRange_maxValue = Lens.lens (\IntegerHyperParameterRange' {maxValue} -> maxValue) (\s@IntegerHyperParameterRange' {} a -> s {maxValue = a} :: IntegerHyperParameterRange)

-- | The minimum allowable value for the hyperparameter.
integerHyperParameterRange_minValue :: Lens.Lens' IntegerHyperParameterRange (Prelude.Maybe Prelude.Int)
integerHyperParameterRange_minValue = Lens.lens (\IntegerHyperParameterRange' {minValue} -> minValue) (\s@IntegerHyperParameterRange' {} a -> s {minValue = a} :: IntegerHyperParameterRange)

-- | The name of the hyperparameter.
integerHyperParameterRange_name :: Lens.Lens' IntegerHyperParameterRange (Prelude.Maybe Prelude.Text)
integerHyperParameterRange_name = Lens.lens (\IntegerHyperParameterRange' {name} -> name) (\s@IntegerHyperParameterRange' {} a -> s {name = a} :: IntegerHyperParameterRange)

instance Data.FromJSON IntegerHyperParameterRange where
  parseJSON =
    Data.withObject
      "IntegerHyperParameterRange"
      ( \x ->
          IntegerHyperParameterRange'
            Prelude.<$> (x Data..:? "maxValue")
            Prelude.<*> (x Data..:? "minValue")
            Prelude.<*> (x Data..:? "name")
      )

instance Prelude.Hashable IntegerHyperParameterRange where
  hashWithSalt _salt IntegerHyperParameterRange' {..} =
    _salt
      `Prelude.hashWithSalt` maxValue
      `Prelude.hashWithSalt` minValue
      `Prelude.hashWithSalt` name

instance Prelude.NFData IntegerHyperParameterRange where
  rnf IntegerHyperParameterRange' {..} =
    Prelude.rnf maxValue `Prelude.seq`
      Prelude.rnf minValue `Prelude.seq`
        Prelude.rnf name

instance Data.ToJSON IntegerHyperParameterRange where
  toJSON IntegerHyperParameterRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxValue" Data..=) Prelude.<$> maxValue,
            ("minValue" Data..=) Prelude.<$> minValue,
            ("name" Data..=) Prelude.<$> name
          ]
      )
