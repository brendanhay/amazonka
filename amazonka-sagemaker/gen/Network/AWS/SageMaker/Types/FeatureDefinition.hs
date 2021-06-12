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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.FeatureType

-- | A list of features. You must include @FeatureName@ and @FeatureType@.
-- Valid feature @FeatureType@s are @Integral@, @Fractional@ and @String@.
--
-- /See:/ 'newFeatureDefinition' smart constructor.
data FeatureDefinition = FeatureDefinition'
  { -- | The value type of a feature. Valid values are Integral, Fractional, or
    -- String.
    featureType :: Core.Maybe FeatureType,
    -- | The name of a feature. The type must be a string. @FeatureName@ cannot
    -- be any of the following: @is_deleted@, @write_time@,
    -- @api_invocation_time@.
    featureName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { featureType = Core.Nothing,
      featureName = Core.Nothing
    }

-- | The value type of a feature. Valid values are Integral, Fractional, or
-- String.
featureDefinition_featureType :: Lens.Lens' FeatureDefinition (Core.Maybe FeatureType)
featureDefinition_featureType = Lens.lens (\FeatureDefinition' {featureType} -> featureType) (\s@FeatureDefinition' {} a -> s {featureType = a} :: FeatureDefinition)

-- | The name of a feature. The type must be a string. @FeatureName@ cannot
-- be any of the following: @is_deleted@, @write_time@,
-- @api_invocation_time@.
featureDefinition_featureName :: Lens.Lens' FeatureDefinition (Core.Maybe Core.Text)
featureDefinition_featureName = Lens.lens (\FeatureDefinition' {featureName} -> featureName) (\s@FeatureDefinition' {} a -> s {featureName = a} :: FeatureDefinition)

instance Core.FromJSON FeatureDefinition where
  parseJSON =
    Core.withObject
      "FeatureDefinition"
      ( \x ->
          FeatureDefinition'
            Core.<$> (x Core..:? "FeatureType")
            Core.<*> (x Core..:? "FeatureName")
      )

instance Core.Hashable FeatureDefinition

instance Core.NFData FeatureDefinition

instance Core.ToJSON FeatureDefinition where
  toJSON FeatureDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("FeatureType" Core..=) Core.<$> featureType,
            ("FeatureName" Core..=) Core.<$> featureName
          ]
      )
