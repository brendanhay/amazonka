{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.FeatureDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.FeatureDefinition where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.FeatureType

-- | A list of features. You must include @FeatureName@ and @FeatureType@.
-- Valid feature @FeatureType@s are @Integral@, @Fractional@ and @String@.
--
-- /See:/ 'newFeatureDefinition' smart constructor.
data FeatureDefinition = FeatureDefinition'
  { -- | The value type of a feature. Valid values are Integral, Fractional, or
    -- String.
    featureType :: Prelude.Maybe FeatureType,
    -- | The name of a feature. The type must be a string. @FeatureName@ cannot
    -- be any of the following: @is_deleted@, @write_time@,
    -- @api_invocation_time@.
    featureName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FeatureDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featureType', 'featureDefinition_featureType' - The value type of a feature. Valid values are Integral, Fractional, or
-- String.
--
-- 'featureName', 'featureDefinition_featureName' - The name of a feature. The type must be a string. @FeatureName@ cannot
-- be any of the following: @is_deleted@, @write_time@,
-- @api_invocation_time@.
newFeatureDefinition ::
  FeatureDefinition
newFeatureDefinition =
  FeatureDefinition'
    { featureType = Prelude.Nothing,
      featureName = Prelude.Nothing
    }

-- | The value type of a feature. Valid values are Integral, Fractional, or
-- String.
featureDefinition_featureType :: Lens.Lens' FeatureDefinition (Prelude.Maybe FeatureType)
featureDefinition_featureType = Lens.lens (\FeatureDefinition' {featureType} -> featureType) (\s@FeatureDefinition' {} a -> s {featureType = a} :: FeatureDefinition)

-- | The name of a feature. The type must be a string. @FeatureName@ cannot
-- be any of the following: @is_deleted@, @write_time@,
-- @api_invocation_time@.
featureDefinition_featureName :: Lens.Lens' FeatureDefinition (Prelude.Maybe Prelude.Text)
featureDefinition_featureName = Lens.lens (\FeatureDefinition' {featureName} -> featureName) (\s@FeatureDefinition' {} a -> s {featureName = a} :: FeatureDefinition)

instance Prelude.FromJSON FeatureDefinition where
  parseJSON =
    Prelude.withObject
      "FeatureDefinition"
      ( \x ->
          FeatureDefinition'
            Prelude.<$> (x Prelude..:? "FeatureType")
            Prelude.<*> (x Prelude..:? "FeatureName")
      )

instance Prelude.Hashable FeatureDefinition

instance Prelude.NFData FeatureDefinition

instance Prelude.ToJSON FeatureDefinition where
  toJSON FeatureDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("FeatureType" Prelude..=) Prelude.<$> featureType,
            ("FeatureName" Prelude..=) Prelude.<$> featureName
          ]
      )
