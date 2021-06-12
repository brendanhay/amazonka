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
-- Module      : Network.AWS.SageMaker.Types.CategoricalParameterRangeSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CategoricalParameterRangeSpecification where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Defines the possible values for a categorical hyperparameter.
--
-- /See:/ 'newCategoricalParameterRangeSpecification' smart constructor.
data CategoricalParameterRangeSpecification = CategoricalParameterRangeSpecification'
  { -- | The allowed categories for the hyperparameter.
    values :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CategoricalParameterRangeSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'categoricalParameterRangeSpecification_values' - The allowed categories for the hyperparameter.
newCategoricalParameterRangeSpecification ::
  -- | 'values'
  Core.NonEmpty Core.Text ->
  CategoricalParameterRangeSpecification
newCategoricalParameterRangeSpecification pValues_ =
  CategoricalParameterRangeSpecification'
    { values =
        Lens._Coerce Lens.# pValues_
    }

-- | The allowed categories for the hyperparameter.
categoricalParameterRangeSpecification_values :: Lens.Lens' CategoricalParameterRangeSpecification (Core.NonEmpty Core.Text)
categoricalParameterRangeSpecification_values = Lens.lens (\CategoricalParameterRangeSpecification' {values} -> values) (\s@CategoricalParameterRangeSpecification' {} a -> s {values = a} :: CategoricalParameterRangeSpecification) Core.. Lens._Coerce

instance
  Core.FromJSON
    CategoricalParameterRangeSpecification
  where
  parseJSON =
    Core.withObject
      "CategoricalParameterRangeSpecification"
      ( \x ->
          CategoricalParameterRangeSpecification'
            Core.<$> (x Core..: "Values")
      )

instance
  Core.Hashable
    CategoricalParameterRangeSpecification

instance
  Core.NFData
    CategoricalParameterRangeSpecification

instance
  Core.ToJSON
    CategoricalParameterRangeSpecification
  where
  toJSON CategoricalParameterRangeSpecification' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("Values" Core..= values)]
      )
