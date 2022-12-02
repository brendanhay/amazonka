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
-- Module      : Amazonka.SageMaker.Types.EndpointInputConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.EndpointInputConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.EnvironmentParameterRanges
import Amazonka.SageMaker.Types.ProductionVariantInstanceType

-- | The endpoint configuration for the load test.
--
-- /See:/ 'newEndpointInputConfiguration' smart constructor.
data EndpointInputConfiguration = EndpointInputConfiguration'
  { -- | The inference specification name in the model package version.
    inferenceSpecificationName :: Prelude.Maybe Prelude.Text,
    -- | The parameter you want to benchmark against.
    environmentParameterRanges :: Prelude.Maybe EnvironmentParameterRanges,
    -- | The instance types to use for the load test.
    instanceType :: ProductionVariantInstanceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointInputConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inferenceSpecificationName', 'endpointInputConfiguration_inferenceSpecificationName' - The inference specification name in the model package version.
--
-- 'environmentParameterRanges', 'endpointInputConfiguration_environmentParameterRanges' - The parameter you want to benchmark against.
--
-- 'instanceType', 'endpointInputConfiguration_instanceType' - The instance types to use for the load test.
newEndpointInputConfiguration ::
  -- | 'instanceType'
  ProductionVariantInstanceType ->
  EndpointInputConfiguration
newEndpointInputConfiguration pInstanceType_ =
  EndpointInputConfiguration'
    { inferenceSpecificationName =
        Prelude.Nothing,
      environmentParameterRanges = Prelude.Nothing,
      instanceType = pInstanceType_
    }

-- | The inference specification name in the model package version.
endpointInputConfiguration_inferenceSpecificationName :: Lens.Lens' EndpointInputConfiguration (Prelude.Maybe Prelude.Text)
endpointInputConfiguration_inferenceSpecificationName = Lens.lens (\EndpointInputConfiguration' {inferenceSpecificationName} -> inferenceSpecificationName) (\s@EndpointInputConfiguration' {} a -> s {inferenceSpecificationName = a} :: EndpointInputConfiguration)

-- | The parameter you want to benchmark against.
endpointInputConfiguration_environmentParameterRanges :: Lens.Lens' EndpointInputConfiguration (Prelude.Maybe EnvironmentParameterRanges)
endpointInputConfiguration_environmentParameterRanges = Lens.lens (\EndpointInputConfiguration' {environmentParameterRanges} -> environmentParameterRanges) (\s@EndpointInputConfiguration' {} a -> s {environmentParameterRanges = a} :: EndpointInputConfiguration)

-- | The instance types to use for the load test.
endpointInputConfiguration_instanceType :: Lens.Lens' EndpointInputConfiguration ProductionVariantInstanceType
endpointInputConfiguration_instanceType = Lens.lens (\EndpointInputConfiguration' {instanceType} -> instanceType) (\s@EndpointInputConfiguration' {} a -> s {instanceType = a} :: EndpointInputConfiguration)

instance Data.FromJSON EndpointInputConfiguration where
  parseJSON =
    Data.withObject
      "EndpointInputConfiguration"
      ( \x ->
          EndpointInputConfiguration'
            Prelude.<$> (x Data..:? "InferenceSpecificationName")
            Prelude.<*> (x Data..:? "EnvironmentParameterRanges")
            Prelude.<*> (x Data..: "InstanceType")
      )

instance Prelude.Hashable EndpointInputConfiguration where
  hashWithSalt _salt EndpointInputConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` inferenceSpecificationName
      `Prelude.hashWithSalt` environmentParameterRanges
      `Prelude.hashWithSalt` instanceType

instance Prelude.NFData EndpointInputConfiguration where
  rnf EndpointInputConfiguration' {..} =
    Prelude.rnf inferenceSpecificationName
      `Prelude.seq` Prelude.rnf environmentParameterRanges
      `Prelude.seq` Prelude.rnf instanceType

instance Data.ToJSON EndpointInputConfiguration where
  toJSON EndpointInputConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InferenceSpecificationName" Data..=)
              Prelude.<$> inferenceSpecificationName,
            ("EnvironmentParameterRanges" Data..=)
              Prelude.<$> environmentParameterRanges,
            Prelude.Just ("InstanceType" Data..= instanceType)
          ]
      )
