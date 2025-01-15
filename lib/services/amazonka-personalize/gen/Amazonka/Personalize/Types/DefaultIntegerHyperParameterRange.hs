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
-- Module      : Amazonka.Personalize.Types.DefaultIntegerHyperParameterRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.DefaultIntegerHyperParameterRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the name and default range of a integer-valued hyperparameter
-- and whether the hyperparameter is tunable. A tunable hyperparameter can
-- have its value determined during hyperparameter optimization (HPO).
--
-- /See:/ 'newDefaultIntegerHyperParameterRange' smart constructor.
data DefaultIntegerHyperParameterRange = DefaultIntegerHyperParameterRange'
  { -- | Indicates whether the hyperparameter is tunable.
    isTunable :: Prelude.Maybe Prelude.Bool,
    -- | The maximum allowable value for the hyperparameter.
    maxValue :: Prelude.Maybe Prelude.Int,
    -- | The minimum allowable value for the hyperparameter.
    minValue :: Prelude.Maybe Prelude.Int,
    -- | The name of the hyperparameter.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultIntegerHyperParameterRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTunable', 'defaultIntegerHyperParameterRange_isTunable' - Indicates whether the hyperparameter is tunable.
--
-- 'maxValue', 'defaultIntegerHyperParameterRange_maxValue' - The maximum allowable value for the hyperparameter.
--
-- 'minValue', 'defaultIntegerHyperParameterRange_minValue' - The minimum allowable value for the hyperparameter.
--
-- 'name', 'defaultIntegerHyperParameterRange_name' - The name of the hyperparameter.
newDefaultIntegerHyperParameterRange ::
  DefaultIntegerHyperParameterRange
newDefaultIntegerHyperParameterRange =
  DefaultIntegerHyperParameterRange'
    { isTunable =
        Prelude.Nothing,
      maxValue = Prelude.Nothing,
      minValue = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | Indicates whether the hyperparameter is tunable.
defaultIntegerHyperParameterRange_isTunable :: Lens.Lens' DefaultIntegerHyperParameterRange (Prelude.Maybe Prelude.Bool)
defaultIntegerHyperParameterRange_isTunable = Lens.lens (\DefaultIntegerHyperParameterRange' {isTunable} -> isTunable) (\s@DefaultIntegerHyperParameterRange' {} a -> s {isTunable = a} :: DefaultIntegerHyperParameterRange)

-- | The maximum allowable value for the hyperparameter.
defaultIntegerHyperParameterRange_maxValue :: Lens.Lens' DefaultIntegerHyperParameterRange (Prelude.Maybe Prelude.Int)
defaultIntegerHyperParameterRange_maxValue = Lens.lens (\DefaultIntegerHyperParameterRange' {maxValue} -> maxValue) (\s@DefaultIntegerHyperParameterRange' {} a -> s {maxValue = a} :: DefaultIntegerHyperParameterRange)

-- | The minimum allowable value for the hyperparameter.
defaultIntegerHyperParameterRange_minValue :: Lens.Lens' DefaultIntegerHyperParameterRange (Prelude.Maybe Prelude.Int)
defaultIntegerHyperParameterRange_minValue = Lens.lens (\DefaultIntegerHyperParameterRange' {minValue} -> minValue) (\s@DefaultIntegerHyperParameterRange' {} a -> s {minValue = a} :: DefaultIntegerHyperParameterRange)

-- | The name of the hyperparameter.
defaultIntegerHyperParameterRange_name :: Lens.Lens' DefaultIntegerHyperParameterRange (Prelude.Maybe Prelude.Text)
defaultIntegerHyperParameterRange_name = Lens.lens (\DefaultIntegerHyperParameterRange' {name} -> name) (\s@DefaultIntegerHyperParameterRange' {} a -> s {name = a} :: DefaultIntegerHyperParameterRange)

instance
  Data.FromJSON
    DefaultIntegerHyperParameterRange
  where
  parseJSON =
    Data.withObject
      "DefaultIntegerHyperParameterRange"
      ( \x ->
          DefaultIntegerHyperParameterRange'
            Prelude.<$> (x Data..:? "isTunable")
            Prelude.<*> (x Data..:? "maxValue")
            Prelude.<*> (x Data..:? "minValue")
            Prelude.<*> (x Data..:? "name")
      )

instance
  Prelude.Hashable
    DefaultIntegerHyperParameterRange
  where
  hashWithSalt
    _salt
    DefaultIntegerHyperParameterRange' {..} =
      _salt
        `Prelude.hashWithSalt` isTunable
        `Prelude.hashWithSalt` maxValue
        `Prelude.hashWithSalt` minValue
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    DefaultIntegerHyperParameterRange
  where
  rnf DefaultIntegerHyperParameterRange' {..} =
    Prelude.rnf isTunable `Prelude.seq`
      Prelude.rnf maxValue `Prelude.seq`
        Prelude.rnf minValue `Prelude.seq`
          Prelude.rnf name
