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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ResourceSpec where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AppInstanceType

-- | Specifies the ARN\'s of a SageMaker image and SageMaker image version,
-- and the instance type that the version runs on.
--
-- /See:/ 'newResourceSpec' smart constructor.
data ResourceSpec = ResourceSpec'
  { -- | The instance type that the image version runs on.
    instanceType :: Prelude.Maybe AppInstanceType,
    -- | The ARN of the SageMaker image that the image version belongs to.
    sageMakerImageArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the image version created on the instance.
    sageMakerImageVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Lifecycle Configuration attached
    -- to the Resource.
    lifecycleConfigArn :: Prelude.Maybe Prelude.Text
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
-- 'instanceType', 'resourceSpec_instanceType' - The instance type that the image version runs on.
--
-- 'sageMakerImageArn', 'resourceSpec_sageMakerImageArn' - The ARN of the SageMaker image that the image version belongs to.
--
-- 'sageMakerImageVersionArn', 'resourceSpec_sageMakerImageVersionArn' - The ARN of the image version created on the instance.
--
-- 'lifecycleConfigArn', 'resourceSpec_lifecycleConfigArn' - The Amazon Resource Name (ARN) of the Lifecycle Configuration attached
-- to the Resource.
newResourceSpec ::
  ResourceSpec
newResourceSpec =
  ResourceSpec'
    { instanceType = Prelude.Nothing,
      sageMakerImageArn = Prelude.Nothing,
      sageMakerImageVersionArn = Prelude.Nothing,
      lifecycleConfigArn = Prelude.Nothing
    }

-- | The instance type that the image version runs on.
resourceSpec_instanceType :: Lens.Lens' ResourceSpec (Prelude.Maybe AppInstanceType)
resourceSpec_instanceType = Lens.lens (\ResourceSpec' {instanceType} -> instanceType) (\s@ResourceSpec' {} a -> s {instanceType = a} :: ResourceSpec)

-- | The ARN of the SageMaker image that the image version belongs to.
resourceSpec_sageMakerImageArn :: Lens.Lens' ResourceSpec (Prelude.Maybe Prelude.Text)
resourceSpec_sageMakerImageArn = Lens.lens (\ResourceSpec' {sageMakerImageArn} -> sageMakerImageArn) (\s@ResourceSpec' {} a -> s {sageMakerImageArn = a} :: ResourceSpec)

-- | The ARN of the image version created on the instance.
resourceSpec_sageMakerImageVersionArn :: Lens.Lens' ResourceSpec (Prelude.Maybe Prelude.Text)
resourceSpec_sageMakerImageVersionArn = Lens.lens (\ResourceSpec' {sageMakerImageVersionArn} -> sageMakerImageVersionArn) (\s@ResourceSpec' {} a -> s {sageMakerImageVersionArn = a} :: ResourceSpec)

-- | The Amazon Resource Name (ARN) of the Lifecycle Configuration attached
-- to the Resource.
resourceSpec_lifecycleConfigArn :: Lens.Lens' ResourceSpec (Prelude.Maybe Prelude.Text)
resourceSpec_lifecycleConfigArn = Lens.lens (\ResourceSpec' {lifecycleConfigArn} -> lifecycleConfigArn) (\s@ResourceSpec' {} a -> s {lifecycleConfigArn = a} :: ResourceSpec)

instance Core.FromJSON ResourceSpec where
  parseJSON =
    Core.withObject
      "ResourceSpec"
      ( \x ->
          ResourceSpec'
            Prelude.<$> (x Core..:? "InstanceType")
            Prelude.<*> (x Core..:? "SageMakerImageArn")
            Prelude.<*> (x Core..:? "SageMakerImageVersionArn")
            Prelude.<*> (x Core..:? "LifecycleConfigArn")
      )

instance Prelude.Hashable ResourceSpec

instance Prelude.NFData ResourceSpec

instance Core.ToJSON ResourceSpec where
  toJSON ResourceSpec' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("InstanceType" Core..=) Prelude.<$> instanceType,
            ("SageMakerImageArn" Core..=)
              Prelude.<$> sageMakerImageArn,
            ("SageMakerImageVersionArn" Core..=)
              Prelude.<$> sageMakerImageVersionArn,
            ("LifecycleConfigArn" Core..=)
              Prelude.<$> lifecycleConfigArn
          ]
      )
