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
-- Module      : Amazonka.SageMaker.Types.ResourceSpec
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ResourceSpec where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AppInstanceType

-- | Specifies the ARN\'s of a SageMaker image and SageMaker image version,
-- and the instance type that the version runs on.
--
-- /See:/ 'newResourceSpec' smart constructor.
data ResourceSpec = ResourceSpec'
  { -- | The Amazon Resource Name (ARN) of the Lifecycle Configuration attached
    -- to the Resource.
    lifecycleConfigArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the image version created on the instance.
    sageMakerImageVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The instance type that the image version runs on.
    --
    -- __JupyterServer apps__ only support the @system@ value.
    --
    -- For __KernelGateway apps__, the @system@ value is translated to
    -- @ml.t3.medium@. KernelGateway apps also support all other values for
    -- available instance types.
    instanceType :: Prelude.Maybe AppInstanceType,
    -- | The ARN of the SageMaker image that the image version belongs to.
    sageMakerImageArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceSpec' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lifecycleConfigArn', 'resourceSpec_lifecycleConfigArn' - The Amazon Resource Name (ARN) of the Lifecycle Configuration attached
-- to the Resource.
--
-- 'sageMakerImageVersionArn', 'resourceSpec_sageMakerImageVersionArn' - The ARN of the image version created on the instance.
--
-- 'instanceType', 'resourceSpec_instanceType' - The instance type that the image version runs on.
--
-- __JupyterServer apps__ only support the @system@ value.
--
-- For __KernelGateway apps__, the @system@ value is translated to
-- @ml.t3.medium@. KernelGateway apps also support all other values for
-- available instance types.
--
-- 'sageMakerImageArn', 'resourceSpec_sageMakerImageArn' - The ARN of the SageMaker image that the image version belongs to.
newResourceSpec ::
  ResourceSpec
newResourceSpec =
  ResourceSpec'
    { lifecycleConfigArn = Prelude.Nothing,
      sageMakerImageVersionArn = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      sageMakerImageArn = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the Lifecycle Configuration attached
-- to the Resource.
resourceSpec_lifecycleConfigArn :: Lens.Lens' ResourceSpec (Prelude.Maybe Prelude.Text)
resourceSpec_lifecycleConfigArn = Lens.lens (\ResourceSpec' {lifecycleConfigArn} -> lifecycleConfigArn) (\s@ResourceSpec' {} a -> s {lifecycleConfigArn = a} :: ResourceSpec)

-- | The ARN of the image version created on the instance.
resourceSpec_sageMakerImageVersionArn :: Lens.Lens' ResourceSpec (Prelude.Maybe Prelude.Text)
resourceSpec_sageMakerImageVersionArn = Lens.lens (\ResourceSpec' {sageMakerImageVersionArn} -> sageMakerImageVersionArn) (\s@ResourceSpec' {} a -> s {sageMakerImageVersionArn = a} :: ResourceSpec)

-- | The instance type that the image version runs on.
--
-- __JupyterServer apps__ only support the @system@ value.
--
-- For __KernelGateway apps__, the @system@ value is translated to
-- @ml.t3.medium@. KernelGateway apps also support all other values for
-- available instance types.
resourceSpec_instanceType :: Lens.Lens' ResourceSpec (Prelude.Maybe AppInstanceType)
resourceSpec_instanceType = Lens.lens (\ResourceSpec' {instanceType} -> instanceType) (\s@ResourceSpec' {} a -> s {instanceType = a} :: ResourceSpec)

-- | The ARN of the SageMaker image that the image version belongs to.
resourceSpec_sageMakerImageArn :: Lens.Lens' ResourceSpec (Prelude.Maybe Prelude.Text)
resourceSpec_sageMakerImageArn = Lens.lens (\ResourceSpec' {sageMakerImageArn} -> sageMakerImageArn) (\s@ResourceSpec' {} a -> s {sageMakerImageArn = a} :: ResourceSpec)

instance Core.FromJSON ResourceSpec where
  parseJSON =
    Core.withObject
      "ResourceSpec"
      ( \x ->
          ResourceSpec'
            Prelude.<$> (x Core..:? "LifecycleConfigArn")
            Prelude.<*> (x Core..:? "SageMakerImageVersionArn")
            Prelude.<*> (x Core..:? "InstanceType")
            Prelude.<*> (x Core..:? "SageMakerImageArn")
      )

instance Prelude.Hashable ResourceSpec where
  hashWithSalt _salt ResourceSpec' {..} =
    _salt `Prelude.hashWithSalt` lifecycleConfigArn
      `Prelude.hashWithSalt` sageMakerImageVersionArn
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` sageMakerImageArn

instance Prelude.NFData ResourceSpec where
  rnf ResourceSpec' {..} =
    Prelude.rnf lifecycleConfigArn
      `Prelude.seq` Prelude.rnf sageMakerImageVersionArn
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf sageMakerImageArn

instance Core.ToJSON ResourceSpec where
  toJSON ResourceSpec' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LifecycleConfigArn" Core..=)
              Prelude.<$> lifecycleConfigArn,
            ("SageMakerImageVersionArn" Core..=)
              Prelude.<$> sageMakerImageVersionArn,
            ("InstanceType" Core..=) Prelude.<$> instanceType,
            ("SageMakerImageArn" Core..=)
              Prelude.<$> sageMakerImageArn
          ]
      )
