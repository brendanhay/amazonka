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
-- Module      : Amazonka.Personalize.Types.ContinuousHyperParameterRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.ContinuousHyperParameterRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the name and range of a continuous hyperparameter.
--
-- /See:/ 'newContinuousHyperParameterRange' smart constructor.
data ContinuousHyperParameterRange = ContinuousHyperParameterRange'
  { -- | The maximum allowable value for the hyperparameter.
    maxValue :: Prelude.Maybe Prelude.Double,
    -- | The minimum allowable value for the hyperparameter.
    minValue :: Prelude.Maybe Prelude.Double,
    -- | The name of the hyperparameter.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContinuousHyperParameterRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxValue', 'continuousHyperParameterRange_maxValue' - The maximum allowable value for the hyperparameter.
--
-- 'minValue', 'continuousHyperParameterRange_minValue' - The minimum allowable value for the hyperparameter.
--
-- 'name', 'continuousHyperParameterRange_name' - The name of the hyperparameter.
newContinuousHyperParameterRange ::
  ContinuousHyperParameterRange
newContinuousHyperParameterRange =
  ContinuousHyperParameterRange'
    { maxValue =
        Prelude.Nothing,
      minValue = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The maximum allowable value for the hyperparameter.
continuousHyperParameterRange_maxValue :: Lens.Lens' ContinuousHyperParameterRange (Prelude.Maybe Prelude.Double)
continuousHyperParameterRange_maxValue = Lens.lens (\ContinuousHyperParameterRange' {maxValue} -> maxValue) (\s@ContinuousHyperParameterRange' {} a -> s {maxValue = a} :: ContinuousHyperParameterRange)

-- | The minimum allowable value for the hyperparameter.
continuousHyperParameterRange_minValue :: Lens.Lens' ContinuousHyperParameterRange (Prelude.Maybe Prelude.Double)
continuousHyperParameterRange_minValue = Lens.lens (\ContinuousHyperParameterRange' {minValue} -> minValue) (\s@ContinuousHyperParameterRange' {} a -> s {minValue = a} :: ContinuousHyperParameterRange)

-- | The name of the hyperparameter.
continuousHyperParameterRange_name :: Lens.Lens' ContinuousHyperParameterRange (Prelude.Maybe Prelude.Text)
continuousHyperParameterRange_name = Lens.lens (\ContinuousHyperParameterRange' {name} -> name) (\s@ContinuousHyperParameterRange' {} a -> s {name = a} :: ContinuousHyperParameterRange)

instance Data.FromJSON ContinuousHyperParameterRange where
  parseJSON =
    Data.withObject
      "ContinuousHyperParameterRange"
      ( \x ->
          ContinuousHyperParameterRange'
            Prelude.<$> (x Data..:? "maxValue")
            Prelude.<*> (x Data..:? "minValue")
            Prelude.<*> (x Data..:? "name")
      )

instance
  Prelude.Hashable
    ContinuousHyperParameterRange
  where
  hashWithSalt _salt ContinuousHyperParameterRange' {..} =
    _salt
      `Prelude.hashWithSalt` maxValue
      `Prelude.hashWithSalt` minValue
      `Prelude.hashWithSalt` name

instance Prelude.NFData ContinuousHyperParameterRange where
  rnf ContinuousHyperParameterRange' {..} =
    Prelude.rnf maxValue
      `Prelude.seq` Prelude.rnf minValue
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON ContinuousHyperParameterRange where
  toJSON ContinuousHyperParameterRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxValue" Data..=) Prelude.<$> maxValue,
            ("minValue" Data..=) Prelude.<$> minValue,
            ("name" Data..=) Prelude.<$> name
          ]
      )
