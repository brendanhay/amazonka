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
-- Module      : Amazonka.Personalize.Types.DefaultContinuousHyperParameterRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.DefaultContinuousHyperParameterRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the name and default range of a continuous hyperparameter and
-- whether the hyperparameter is tunable. A tunable hyperparameter can have
-- its value determined during hyperparameter optimization (HPO).
--
-- /See:/ 'newDefaultContinuousHyperParameterRange' smart constructor.
data DefaultContinuousHyperParameterRange = DefaultContinuousHyperParameterRange'
  { -- | Whether the hyperparameter is tunable.
    isTunable :: Prelude.Maybe Prelude.Bool,
    -- | The maximum allowable value for the hyperparameter.
    maxValue :: Prelude.Maybe Prelude.Double,
    -- | The minimum allowable value for the hyperparameter.
    minValue :: Prelude.Maybe Prelude.Double,
    -- | The name of the hyperparameter.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultContinuousHyperParameterRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTunable', 'defaultContinuousHyperParameterRange_isTunable' - Whether the hyperparameter is tunable.
--
-- 'maxValue', 'defaultContinuousHyperParameterRange_maxValue' - The maximum allowable value for the hyperparameter.
--
-- 'minValue', 'defaultContinuousHyperParameterRange_minValue' - The minimum allowable value for the hyperparameter.
--
-- 'name', 'defaultContinuousHyperParameterRange_name' - The name of the hyperparameter.
newDefaultContinuousHyperParameterRange ::
  DefaultContinuousHyperParameterRange
newDefaultContinuousHyperParameterRange =
  DefaultContinuousHyperParameterRange'
    { isTunable =
        Prelude.Nothing,
      maxValue = Prelude.Nothing,
      minValue = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | Whether the hyperparameter is tunable.
defaultContinuousHyperParameterRange_isTunable :: Lens.Lens' DefaultContinuousHyperParameterRange (Prelude.Maybe Prelude.Bool)
defaultContinuousHyperParameterRange_isTunable = Lens.lens (\DefaultContinuousHyperParameterRange' {isTunable} -> isTunable) (\s@DefaultContinuousHyperParameterRange' {} a -> s {isTunable = a} :: DefaultContinuousHyperParameterRange)

-- | The maximum allowable value for the hyperparameter.
defaultContinuousHyperParameterRange_maxValue :: Lens.Lens' DefaultContinuousHyperParameterRange (Prelude.Maybe Prelude.Double)
defaultContinuousHyperParameterRange_maxValue = Lens.lens (\DefaultContinuousHyperParameterRange' {maxValue} -> maxValue) (\s@DefaultContinuousHyperParameterRange' {} a -> s {maxValue = a} :: DefaultContinuousHyperParameterRange)

-- | The minimum allowable value for the hyperparameter.
defaultContinuousHyperParameterRange_minValue :: Lens.Lens' DefaultContinuousHyperParameterRange (Prelude.Maybe Prelude.Double)
defaultContinuousHyperParameterRange_minValue = Lens.lens (\DefaultContinuousHyperParameterRange' {minValue} -> minValue) (\s@DefaultContinuousHyperParameterRange' {} a -> s {minValue = a} :: DefaultContinuousHyperParameterRange)

-- | The name of the hyperparameter.
defaultContinuousHyperParameterRange_name :: Lens.Lens' DefaultContinuousHyperParameterRange (Prelude.Maybe Prelude.Text)
defaultContinuousHyperParameterRange_name = Lens.lens (\DefaultContinuousHyperParameterRange' {name} -> name) (\s@DefaultContinuousHyperParameterRange' {} a -> s {name = a} :: DefaultContinuousHyperParameterRange)

instance
  Data.FromJSON
    DefaultContinuousHyperParameterRange
  where
  parseJSON =
    Data.withObject
      "DefaultContinuousHyperParameterRange"
      ( \x ->
          DefaultContinuousHyperParameterRange'
            Prelude.<$> (x Data..:? "isTunable")
            Prelude.<*> (x Data..:? "maxValue")
            Prelude.<*> (x Data..:? "minValue")
            Prelude.<*> (x Data..:? "name")
      )

instance
  Prelude.Hashable
    DefaultContinuousHyperParameterRange
  where
  hashWithSalt
    _salt
    DefaultContinuousHyperParameterRange' {..} =
      _salt
        `Prelude.hashWithSalt` isTunable
        `Prelude.hashWithSalt` maxValue
        `Prelude.hashWithSalt` minValue
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    DefaultContinuousHyperParameterRange
  where
  rnf DefaultContinuousHyperParameterRange' {..} =
    Prelude.rnf isTunable
      `Prelude.seq` Prelude.rnf maxValue
      `Prelude.seq` Prelude.rnf minValue
      `Prelude.seq` Prelude.rnf name
