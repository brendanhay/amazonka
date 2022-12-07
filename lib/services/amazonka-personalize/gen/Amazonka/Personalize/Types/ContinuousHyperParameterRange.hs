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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
  { -- | The name of the hyperparameter.
    name :: Prelude.Maybe Prelude.Text,
    -- | The minimum allowable value for the hyperparameter.
    minValue :: Prelude.Maybe Prelude.Double,
    -- | The maximum allowable value for the hyperparameter.
    maxValue :: Prelude.Maybe Prelude.Double
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
-- 'name', 'continuousHyperParameterRange_name' - The name of the hyperparameter.
--
-- 'minValue', 'continuousHyperParameterRange_minValue' - The minimum allowable value for the hyperparameter.
--
-- 'maxValue', 'continuousHyperParameterRange_maxValue' - The maximum allowable value for the hyperparameter.
newContinuousHyperParameterRange ::
  ContinuousHyperParameterRange
newContinuousHyperParameterRange =
  ContinuousHyperParameterRange'
    { name =
        Prelude.Nothing,
      minValue = Prelude.Nothing,
      maxValue = Prelude.Nothing
    }

-- | The name of the hyperparameter.
continuousHyperParameterRange_name :: Lens.Lens' ContinuousHyperParameterRange (Prelude.Maybe Prelude.Text)
continuousHyperParameterRange_name = Lens.lens (\ContinuousHyperParameterRange' {name} -> name) (\s@ContinuousHyperParameterRange' {} a -> s {name = a} :: ContinuousHyperParameterRange)

-- | The minimum allowable value for the hyperparameter.
continuousHyperParameterRange_minValue :: Lens.Lens' ContinuousHyperParameterRange (Prelude.Maybe Prelude.Double)
continuousHyperParameterRange_minValue = Lens.lens (\ContinuousHyperParameterRange' {minValue} -> minValue) (\s@ContinuousHyperParameterRange' {} a -> s {minValue = a} :: ContinuousHyperParameterRange)

-- | The maximum allowable value for the hyperparameter.
continuousHyperParameterRange_maxValue :: Lens.Lens' ContinuousHyperParameterRange (Prelude.Maybe Prelude.Double)
continuousHyperParameterRange_maxValue = Lens.lens (\ContinuousHyperParameterRange' {maxValue} -> maxValue) (\s@ContinuousHyperParameterRange' {} a -> s {maxValue = a} :: ContinuousHyperParameterRange)

instance Data.FromJSON ContinuousHyperParameterRange where
  parseJSON =
    Data.withObject
      "ContinuousHyperParameterRange"
      ( \x ->
          ContinuousHyperParameterRange'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "minValue")
            Prelude.<*> (x Data..:? "maxValue")
      )

instance
  Prelude.Hashable
    ContinuousHyperParameterRange
  where
  hashWithSalt _salt ContinuousHyperParameterRange' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` minValue
      `Prelude.hashWithSalt` maxValue

instance Prelude.NFData ContinuousHyperParameterRange where
  rnf ContinuousHyperParameterRange' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf minValue
      `Prelude.seq` Prelude.rnf maxValue

instance Data.ToJSON ContinuousHyperParameterRange where
  toJSON ContinuousHyperParameterRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("minValue" Data..=) Prelude.<$> minValue,
            ("maxValue" Data..=) Prelude.<$> maxValue
          ]
      )
