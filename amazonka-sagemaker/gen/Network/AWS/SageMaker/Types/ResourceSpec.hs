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
-- Module      : Network.AWS.SageMaker.Types.ResourceSpec
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ResourceSpec where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.AppInstanceType

-- | Specifies the ARN\'s of a SageMaker image and SageMaker image version,
-- and the instance type that the version runs on.
--
-- /See:/ 'newResourceSpec' smart constructor.
data ResourceSpec = ResourceSpec'
  { -- | The instance type that the image version runs on.
    instanceType :: Core.Maybe AppInstanceType,
    -- | The ARN of the SageMaker image that the image version belongs to.
    sageMakerImageArn :: Core.Maybe Core.Text,
    -- | The ARN of the image version created on the instance.
    sageMakerImageVersionArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
newResourceSpec ::
  ResourceSpec
newResourceSpec =
  ResourceSpec'
    { instanceType = Core.Nothing,
      sageMakerImageArn = Core.Nothing,
      sageMakerImageVersionArn = Core.Nothing
    }

-- | The instance type that the image version runs on.
resourceSpec_instanceType :: Lens.Lens' ResourceSpec (Core.Maybe AppInstanceType)
resourceSpec_instanceType = Lens.lens (\ResourceSpec' {instanceType} -> instanceType) (\s@ResourceSpec' {} a -> s {instanceType = a} :: ResourceSpec)

-- | The ARN of the SageMaker image that the image version belongs to.
resourceSpec_sageMakerImageArn :: Lens.Lens' ResourceSpec (Core.Maybe Core.Text)
resourceSpec_sageMakerImageArn = Lens.lens (\ResourceSpec' {sageMakerImageArn} -> sageMakerImageArn) (\s@ResourceSpec' {} a -> s {sageMakerImageArn = a} :: ResourceSpec)

-- | The ARN of the image version created on the instance.
resourceSpec_sageMakerImageVersionArn :: Lens.Lens' ResourceSpec (Core.Maybe Core.Text)
resourceSpec_sageMakerImageVersionArn = Lens.lens (\ResourceSpec' {sageMakerImageVersionArn} -> sageMakerImageVersionArn) (\s@ResourceSpec' {} a -> s {sageMakerImageVersionArn = a} :: ResourceSpec)

instance Core.FromJSON ResourceSpec where
  parseJSON =
    Core.withObject
      "ResourceSpec"
      ( \x ->
          ResourceSpec'
            Core.<$> (x Core..:? "InstanceType")
            Core.<*> (x Core..:? "SageMakerImageArn")
            Core.<*> (x Core..:? "SageMakerImageVersionArn")
      )

instance Core.Hashable ResourceSpec

instance Core.NFData ResourceSpec

instance Core.ToJSON ResourceSpec where
  toJSON ResourceSpec' {..} =
    Core.object
      ( Core.catMaybes
          [ ("InstanceType" Core..=) Core.<$> instanceType,
            ("SageMakerImageArn" Core..=)
              Core.<$> sageMakerImageArn,
            ("SageMakerImageVersionArn" Core..=)
              Core.<$> sageMakerImageVersionArn
          ]
      )
