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
-- Module      : Amazonka.SageMaker.Types.RealTimeInferenceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.RealTimeInferenceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.InstanceType

-- | The infrastructure configuration for deploying the model to a real-time
-- inference endpoint.
--
-- /See:/ 'newRealTimeInferenceConfig' smart constructor.
data RealTimeInferenceConfig = RealTimeInferenceConfig'
  { -- | The instance type the model is deployed to.
    instanceType :: InstanceType,
    -- | The number of instances of the type specified by @InstanceType@.
    instanceCount :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RealTimeInferenceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceType', 'realTimeInferenceConfig_instanceType' - The instance type the model is deployed to.
--
-- 'instanceCount', 'realTimeInferenceConfig_instanceCount' - The number of instances of the type specified by @InstanceType@.
newRealTimeInferenceConfig ::
  -- | 'instanceType'
  InstanceType ->
  -- | 'instanceCount'
  Prelude.Natural ->
  RealTimeInferenceConfig
newRealTimeInferenceConfig
  pInstanceType_
  pInstanceCount_ =
    RealTimeInferenceConfig'
      { instanceType =
          pInstanceType_,
        instanceCount = pInstanceCount_
      }

-- | The instance type the model is deployed to.
realTimeInferenceConfig_instanceType :: Lens.Lens' RealTimeInferenceConfig InstanceType
realTimeInferenceConfig_instanceType = Lens.lens (\RealTimeInferenceConfig' {instanceType} -> instanceType) (\s@RealTimeInferenceConfig' {} a -> s {instanceType = a} :: RealTimeInferenceConfig)

-- | The number of instances of the type specified by @InstanceType@.
realTimeInferenceConfig_instanceCount :: Lens.Lens' RealTimeInferenceConfig Prelude.Natural
realTimeInferenceConfig_instanceCount = Lens.lens (\RealTimeInferenceConfig' {instanceCount} -> instanceCount) (\s@RealTimeInferenceConfig' {} a -> s {instanceCount = a} :: RealTimeInferenceConfig)

instance Data.FromJSON RealTimeInferenceConfig where
  parseJSON =
    Data.withObject
      "RealTimeInferenceConfig"
      ( \x ->
          RealTimeInferenceConfig'
            Prelude.<$> (x Data..: "InstanceType")
            Prelude.<*> (x Data..: "InstanceCount")
      )

instance Prelude.Hashable RealTimeInferenceConfig where
  hashWithSalt _salt RealTimeInferenceConfig' {..} =
    _salt
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` instanceCount

instance Prelude.NFData RealTimeInferenceConfig where
  rnf RealTimeInferenceConfig' {..} =
    Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf instanceCount

instance Data.ToJSON RealTimeInferenceConfig where
  toJSON RealTimeInferenceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("InstanceType" Data..= instanceType),
            Prelude.Just
              ("InstanceCount" Data..= instanceCount)
          ]
      )
