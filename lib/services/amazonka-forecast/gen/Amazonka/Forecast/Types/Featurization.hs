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
-- Module      : Amazonka.Forecast.Types.Featurization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.Featurization where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types.FeaturizationMethod
import qualified Amazonka.Prelude as Prelude

-- | This object belongs to the CreatePredictor operation. If you created
-- your predictor with CreateAutoPredictor, see AttributeConfig.
--
-- Provides featurization (transformation) information for a dataset field.
-- This object is part of the FeaturizationConfig object.
--
-- For example:
--
-- @{@
--
-- @\"AttributeName\": \"demand\",@
--
-- @FeaturizationPipeline [ {@
--
-- @\"FeaturizationMethodName\": \"filling\",@
--
-- @\"FeaturizationMethodParameters\": {\"aggregation\": \"avg\", \"backfill\": \"nan\"}@
--
-- @} ]@
--
-- @}@
--
-- /See:/ 'newFeaturization' smart constructor.
data Featurization = Featurization'
  { -- | An array of one @FeaturizationMethod@ object that specifies the feature
    -- transformation method.
    featurizationPipeline :: Prelude.Maybe (Prelude.NonEmpty FeaturizationMethod),
    -- | The name of the schema attribute that specifies the data field to be
    -- featurized. Amazon Forecast supports the target field of the
    -- @TARGET_TIME_SERIES@ and the @RELATED_TIME_SERIES@ datasets. For
    -- example, for the @RETAIL@ domain, the target is @demand@, and for the
    -- @CUSTOM@ domain, the target is @target_value@. For more information, see
    -- howitworks-missing-values.
    attributeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Featurization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featurizationPipeline', 'featurization_featurizationPipeline' - An array of one @FeaturizationMethod@ object that specifies the feature
-- transformation method.
--
-- 'attributeName', 'featurization_attributeName' - The name of the schema attribute that specifies the data field to be
-- featurized. Amazon Forecast supports the target field of the
-- @TARGET_TIME_SERIES@ and the @RELATED_TIME_SERIES@ datasets. For
-- example, for the @RETAIL@ domain, the target is @demand@, and for the
-- @CUSTOM@ domain, the target is @target_value@. For more information, see
-- howitworks-missing-values.
newFeaturization ::
  -- | 'attributeName'
  Prelude.Text ->
  Featurization
newFeaturization pAttributeName_ =
  Featurization'
    { featurizationPipeline =
        Prelude.Nothing,
      attributeName = pAttributeName_
    }

-- | An array of one @FeaturizationMethod@ object that specifies the feature
-- transformation method.
featurization_featurizationPipeline :: Lens.Lens' Featurization (Prelude.Maybe (Prelude.NonEmpty FeaturizationMethod))
featurization_featurizationPipeline = Lens.lens (\Featurization' {featurizationPipeline} -> featurizationPipeline) (\s@Featurization' {} a -> s {featurizationPipeline = a} :: Featurization) Prelude.. Lens.mapping Lens.coerced

-- | The name of the schema attribute that specifies the data field to be
-- featurized. Amazon Forecast supports the target field of the
-- @TARGET_TIME_SERIES@ and the @RELATED_TIME_SERIES@ datasets. For
-- example, for the @RETAIL@ domain, the target is @demand@, and for the
-- @CUSTOM@ domain, the target is @target_value@. For more information, see
-- howitworks-missing-values.
featurization_attributeName :: Lens.Lens' Featurization Prelude.Text
featurization_attributeName = Lens.lens (\Featurization' {attributeName} -> attributeName) (\s@Featurization' {} a -> s {attributeName = a} :: Featurization)

instance Data.FromJSON Featurization where
  parseJSON =
    Data.withObject
      "Featurization"
      ( \x ->
          Featurization'
            Prelude.<$> (x Data..:? "FeaturizationPipeline")
            Prelude.<*> (x Data..: "AttributeName")
      )

instance Prelude.Hashable Featurization where
  hashWithSalt _salt Featurization' {..} =
    _salt
      `Prelude.hashWithSalt` featurizationPipeline
      `Prelude.hashWithSalt` attributeName

instance Prelude.NFData Featurization where
  rnf Featurization' {..} =
    Prelude.rnf featurizationPipeline
      `Prelude.seq` Prelude.rnf attributeName

instance Data.ToJSON Featurization where
  toJSON Featurization' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FeaturizationPipeline" Data..=)
              Prelude.<$> featurizationPipeline,
            Prelude.Just
              ("AttributeName" Data..= attributeName)
          ]
      )
