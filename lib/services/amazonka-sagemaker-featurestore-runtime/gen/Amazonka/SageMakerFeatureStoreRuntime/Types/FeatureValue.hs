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
-- Module      : Amazonka.SageMakerFeatureStoreRuntime.Types.FeatureValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerFeatureStoreRuntime.Types.FeatureValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The value associated with a feature.
--
-- /See:/ 'newFeatureValue' smart constructor.
data FeatureValue = FeatureValue'
  { -- | The name of a feature that a feature value corresponds to.
    featureName :: Prelude.Text,
    -- | The value associated with a feature, in string format. Note that
    -- features types can be String, Integral, or Fractional. This value
    -- represents all three types as a string.
    valueAsString :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FeatureValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featureName', 'featureValue_featureName' - The name of a feature that a feature value corresponds to.
--
-- 'valueAsString', 'featureValue_valueAsString' - The value associated with a feature, in string format. Note that
-- features types can be String, Integral, or Fractional. This value
-- represents all three types as a string.
newFeatureValue ::
  -- | 'featureName'
  Prelude.Text ->
  -- | 'valueAsString'
  Prelude.Text ->
  FeatureValue
newFeatureValue pFeatureName_ pValueAsString_ =
  FeatureValue'
    { featureName = pFeatureName_,
      valueAsString = pValueAsString_
    }

-- | The name of a feature that a feature value corresponds to.
featureValue_featureName :: Lens.Lens' FeatureValue Prelude.Text
featureValue_featureName = Lens.lens (\FeatureValue' {featureName} -> featureName) (\s@FeatureValue' {} a -> s {featureName = a} :: FeatureValue)

-- | The value associated with a feature, in string format. Note that
-- features types can be String, Integral, or Fractional. This value
-- represents all three types as a string.
featureValue_valueAsString :: Lens.Lens' FeatureValue Prelude.Text
featureValue_valueAsString = Lens.lens (\FeatureValue' {valueAsString} -> valueAsString) (\s@FeatureValue' {} a -> s {valueAsString = a} :: FeatureValue)

instance Data.FromJSON FeatureValue where
  parseJSON =
    Data.withObject
      "FeatureValue"
      ( \x ->
          FeatureValue'
            Prelude.<$> (x Data..: "FeatureName")
            Prelude.<*> (x Data..: "ValueAsString")
      )

instance Prelude.Hashable FeatureValue where
  hashWithSalt _salt FeatureValue' {..} =
    _salt `Prelude.hashWithSalt` featureName
      `Prelude.hashWithSalt` valueAsString

instance Prelude.NFData FeatureValue where
  rnf FeatureValue' {..} =
    Prelude.rnf featureName
      `Prelude.seq` Prelude.rnf valueAsString

instance Data.ToJSON FeatureValue where
  toJSON FeatureValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FeatureName" Data..= featureName),
            Prelude.Just
              ("ValueAsString" Data..= valueAsString)
          ]
      )
