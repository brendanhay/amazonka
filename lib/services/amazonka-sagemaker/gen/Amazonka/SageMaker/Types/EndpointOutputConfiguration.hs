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
-- Module      : Amazonka.SageMaker.Types.EndpointOutputConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.EndpointOutputConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ProductionVariantInstanceType

-- | The endpoint configuration made by Inference Recommender during a
-- recommendation job.
--
-- /See:/ 'newEndpointOutputConfiguration' smart constructor.
data EndpointOutputConfiguration = EndpointOutputConfiguration'
  { -- | The name of the endpoint made during a recommendation job.
    endpointName :: Prelude.Text,
    -- | The name of the production variant (deployed model) made during a
    -- recommendation job.
    variantName :: Prelude.Text,
    -- | The instance type recommended by Amazon SageMaker Inference Recommender.
    instanceType :: ProductionVariantInstanceType,
    -- | The number of instances recommended to launch initially.
    initialInstanceCount :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointOutputConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointName', 'endpointOutputConfiguration_endpointName' - The name of the endpoint made during a recommendation job.
--
-- 'variantName', 'endpointOutputConfiguration_variantName' - The name of the production variant (deployed model) made during a
-- recommendation job.
--
-- 'instanceType', 'endpointOutputConfiguration_instanceType' - The instance type recommended by Amazon SageMaker Inference Recommender.
--
-- 'initialInstanceCount', 'endpointOutputConfiguration_initialInstanceCount' - The number of instances recommended to launch initially.
newEndpointOutputConfiguration ::
  -- | 'endpointName'
  Prelude.Text ->
  -- | 'variantName'
  Prelude.Text ->
  -- | 'instanceType'
  ProductionVariantInstanceType ->
  -- | 'initialInstanceCount'
  Prelude.Int ->
  EndpointOutputConfiguration
newEndpointOutputConfiguration
  pEndpointName_
  pVariantName_
  pInstanceType_
  pInitialInstanceCount_ =
    EndpointOutputConfiguration'
      { endpointName =
          pEndpointName_,
        variantName = pVariantName_,
        instanceType = pInstanceType_,
        initialInstanceCount = pInitialInstanceCount_
      }

-- | The name of the endpoint made during a recommendation job.
endpointOutputConfiguration_endpointName :: Lens.Lens' EndpointOutputConfiguration Prelude.Text
endpointOutputConfiguration_endpointName = Lens.lens (\EndpointOutputConfiguration' {endpointName} -> endpointName) (\s@EndpointOutputConfiguration' {} a -> s {endpointName = a} :: EndpointOutputConfiguration)

-- | The name of the production variant (deployed model) made during a
-- recommendation job.
endpointOutputConfiguration_variantName :: Lens.Lens' EndpointOutputConfiguration Prelude.Text
endpointOutputConfiguration_variantName = Lens.lens (\EndpointOutputConfiguration' {variantName} -> variantName) (\s@EndpointOutputConfiguration' {} a -> s {variantName = a} :: EndpointOutputConfiguration)

-- | The instance type recommended by Amazon SageMaker Inference Recommender.
endpointOutputConfiguration_instanceType :: Lens.Lens' EndpointOutputConfiguration ProductionVariantInstanceType
endpointOutputConfiguration_instanceType = Lens.lens (\EndpointOutputConfiguration' {instanceType} -> instanceType) (\s@EndpointOutputConfiguration' {} a -> s {instanceType = a} :: EndpointOutputConfiguration)

-- | The number of instances recommended to launch initially.
endpointOutputConfiguration_initialInstanceCount :: Lens.Lens' EndpointOutputConfiguration Prelude.Int
endpointOutputConfiguration_initialInstanceCount = Lens.lens (\EndpointOutputConfiguration' {initialInstanceCount} -> initialInstanceCount) (\s@EndpointOutputConfiguration' {} a -> s {initialInstanceCount = a} :: EndpointOutputConfiguration)

instance Core.FromJSON EndpointOutputConfiguration where
  parseJSON =
    Core.withObject
      "EndpointOutputConfiguration"
      ( \x ->
          EndpointOutputConfiguration'
            Prelude.<$> (x Core..: "EndpointName")
            Prelude.<*> (x Core..: "VariantName")
            Prelude.<*> (x Core..: "InstanceType")
            Prelude.<*> (x Core..: "InitialInstanceCount")
      )

instance Prelude.Hashable EndpointOutputConfiguration where
  hashWithSalt _salt EndpointOutputConfiguration' {..} =
    _salt `Prelude.hashWithSalt` endpointName
      `Prelude.hashWithSalt` variantName
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` initialInstanceCount

instance Prelude.NFData EndpointOutputConfiguration where
  rnf EndpointOutputConfiguration' {..} =
    Prelude.rnf endpointName
      `Prelude.seq` Prelude.rnf variantName
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf initialInstanceCount
