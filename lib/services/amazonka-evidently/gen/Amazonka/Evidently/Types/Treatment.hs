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
-- Module      : Amazonka.Evidently.Types.Treatment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.Treatment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that defines one treatment in an experiment. A treatment is
-- a variation of the feature that you are including in the experiment.
--
-- /See:/ 'newTreatment' smart constructor.
data Treatment = Treatment'
  { -- | The description of the treatment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The feature variation used for this treatment. This is a key-value pair.
    -- The key is the feature name, and the value is the variation name.
    featureVariations :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of this treatment.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Treatment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'treatment_description' - The description of the treatment.
--
-- 'featureVariations', 'treatment_featureVariations' - The feature variation used for this treatment. This is a key-value pair.
-- The key is the feature name, and the value is the variation name.
--
-- 'name', 'treatment_name' - The name of this treatment.
newTreatment ::
  -- | 'name'
  Prelude.Text ->
  Treatment
newTreatment pName_ =
  Treatment'
    { description = Prelude.Nothing,
      featureVariations = Prelude.Nothing,
      name = pName_
    }

-- | The description of the treatment.
treatment_description :: Lens.Lens' Treatment (Prelude.Maybe Prelude.Text)
treatment_description = Lens.lens (\Treatment' {description} -> description) (\s@Treatment' {} a -> s {description = a} :: Treatment)

-- | The feature variation used for this treatment. This is a key-value pair.
-- The key is the feature name, and the value is the variation name.
treatment_featureVariations :: Lens.Lens' Treatment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
treatment_featureVariations = Lens.lens (\Treatment' {featureVariations} -> featureVariations) (\s@Treatment' {} a -> s {featureVariations = a} :: Treatment) Prelude.. Lens.mapping Lens.coerced

-- | The name of this treatment.
treatment_name :: Lens.Lens' Treatment Prelude.Text
treatment_name = Lens.lens (\Treatment' {name} -> name) (\s@Treatment' {} a -> s {name = a} :: Treatment)

instance Data.FromJSON Treatment where
  parseJSON =
    Data.withObject
      "Treatment"
      ( \x ->
          Treatment'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> ( x Data..:? "featureVariations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable Treatment where
  hashWithSalt _salt Treatment' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` featureVariations
      `Prelude.hashWithSalt` name

instance Prelude.NFData Treatment where
  rnf Treatment' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf featureVariations
      `Prelude.seq` Prelude.rnf name
