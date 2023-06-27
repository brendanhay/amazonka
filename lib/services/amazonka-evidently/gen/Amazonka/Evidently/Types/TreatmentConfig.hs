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
-- Module      : Amazonka.Evidently.Types.TreatmentConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.TreatmentConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that defines one treatment in an experiment. A treatment is
-- a variation of the feature that you are including in the experiment.
--
-- /See:/ 'newTreatmentConfig' smart constructor.
data TreatmentConfig = TreatmentConfig'
  { -- | A description for this treatment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The feature that this experiment is testing.
    feature :: Prelude.Text,
    -- | A name for this treatment.
    name :: Prelude.Text,
    -- | The name of the variation to use as this treatment in the experiment.
    variation :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TreatmentConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'treatmentConfig_description' - A description for this treatment.
--
-- 'feature', 'treatmentConfig_feature' - The feature that this experiment is testing.
--
-- 'name', 'treatmentConfig_name' - A name for this treatment.
--
-- 'variation', 'treatmentConfig_variation' - The name of the variation to use as this treatment in the experiment.
newTreatmentConfig ::
  -- | 'feature'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'variation'
  Prelude.Text ->
  TreatmentConfig
newTreatmentConfig pFeature_ pName_ pVariation_ =
  TreatmentConfig'
    { description = Prelude.Nothing,
      feature = pFeature_,
      name = pName_,
      variation = pVariation_
    }

-- | A description for this treatment.
treatmentConfig_description :: Lens.Lens' TreatmentConfig (Prelude.Maybe Prelude.Text)
treatmentConfig_description = Lens.lens (\TreatmentConfig' {description} -> description) (\s@TreatmentConfig' {} a -> s {description = a} :: TreatmentConfig)

-- | The feature that this experiment is testing.
treatmentConfig_feature :: Lens.Lens' TreatmentConfig Prelude.Text
treatmentConfig_feature = Lens.lens (\TreatmentConfig' {feature} -> feature) (\s@TreatmentConfig' {} a -> s {feature = a} :: TreatmentConfig)

-- | A name for this treatment.
treatmentConfig_name :: Lens.Lens' TreatmentConfig Prelude.Text
treatmentConfig_name = Lens.lens (\TreatmentConfig' {name} -> name) (\s@TreatmentConfig' {} a -> s {name = a} :: TreatmentConfig)

-- | The name of the variation to use as this treatment in the experiment.
treatmentConfig_variation :: Lens.Lens' TreatmentConfig Prelude.Text
treatmentConfig_variation = Lens.lens (\TreatmentConfig' {variation} -> variation) (\s@TreatmentConfig' {} a -> s {variation = a} :: TreatmentConfig)

instance Prelude.Hashable TreatmentConfig where
  hashWithSalt _salt TreatmentConfig' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` feature
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` variation

instance Prelude.NFData TreatmentConfig where
  rnf TreatmentConfig' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf feature
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf variation

instance Data.ToJSON TreatmentConfig where
  toJSON TreatmentConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("feature" Data..= feature),
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("variation" Data..= variation)
          ]
      )
