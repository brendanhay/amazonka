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
-- Module      : Amazonka.SageMaker.Types.FeatureDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.FeatureDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.FeatureType

-- | A list of features. You must include @FeatureName@ and @FeatureType@.
-- Valid feature @FeatureType@s are @Integral@, @Fractional@ and @String@.
--
-- /See:/ 'newFeatureDefinition' smart constructor.
data FeatureDefinition = FeatureDefinition'
  { -- | The name of a feature. The type must be a string. @FeatureName@ cannot
    -- be any of the following: @is_deleted@, @write_time@,
    -- @api_invocation_time@.
    featureName :: Prelude.Maybe Prelude.Text,
    -- | The value type of a feature. Valid values are Integral, Fractional, or
    -- String.
    featureType :: Prelude.Maybe FeatureType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FeatureDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featureName', 'featureDefinition_featureName' - The name of a feature. The type must be a string. @FeatureName@ cannot
-- be any of the following: @is_deleted@, @write_time@,
-- @api_invocation_time@.
--
-- 'featureType', 'featureDefinition_featureType' - The value type of a feature. Valid values are Integral, Fractional, or
-- String.
newFeatureDefinition ::
  FeatureDefinition
newFeatureDefinition =
  FeatureDefinition'
    { featureName = Prelude.Nothing,
      featureType = Prelude.Nothing
    }

-- | The name of a feature. The type must be a string. @FeatureName@ cannot
-- be any of the following: @is_deleted@, @write_time@,
-- @api_invocation_time@.
featureDefinition_featureName :: Lens.Lens' FeatureDefinition (Prelude.Maybe Prelude.Text)
featureDefinition_featureName = Lens.lens (\FeatureDefinition' {featureName} -> featureName) (\s@FeatureDefinition' {} a -> s {featureName = a} :: FeatureDefinition)

-- | The value type of a feature. Valid values are Integral, Fractional, or
-- String.
featureDefinition_featureType :: Lens.Lens' FeatureDefinition (Prelude.Maybe FeatureType)
featureDefinition_featureType = Lens.lens (\FeatureDefinition' {featureType} -> featureType) (\s@FeatureDefinition' {} a -> s {featureType = a} :: FeatureDefinition)

instance Data.FromJSON FeatureDefinition where
  parseJSON =
    Data.withObject
      "FeatureDefinition"
      ( \x ->
          FeatureDefinition'
            Prelude.<$> (x Data..:? "FeatureName")
            Prelude.<*> (x Data..:? "FeatureType")
      )

instance Prelude.Hashable FeatureDefinition where
  hashWithSalt _salt FeatureDefinition' {..} =
    _salt `Prelude.hashWithSalt` featureName
      `Prelude.hashWithSalt` featureType

instance Prelude.NFData FeatureDefinition where
  rnf FeatureDefinition' {..} =
    Prelude.rnf featureName
      `Prelude.seq` Prelude.rnf featureType

instance Data.ToJSON FeatureDefinition where
  toJSON FeatureDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FeatureName" Data..=) Prelude.<$> featureName,
            ("FeatureType" Data..=) Prelude.<$> featureType
          ]
      )
