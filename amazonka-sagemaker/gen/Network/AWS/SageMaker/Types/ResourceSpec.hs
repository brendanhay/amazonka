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
-- Module      : Network.AWS.SageMaker.Types.ResourceSpec
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ResourceSpec where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.AppInstanceType

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
    sageMakerImageVersionArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { instanceType = Prelude.Nothing,
      sageMakerImageArn = Prelude.Nothing,
      sageMakerImageVersionArn = Prelude.Nothing
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

instance Prelude.FromJSON ResourceSpec where
  parseJSON =
    Prelude.withObject
      "ResourceSpec"
      ( \x ->
          ResourceSpec'
            Prelude.<$> (x Prelude..:? "InstanceType")
            Prelude.<*> (x Prelude..:? "SageMakerImageArn")
            Prelude.<*> (x Prelude..:? "SageMakerImageVersionArn")
      )

instance Prelude.Hashable ResourceSpec

instance Prelude.NFData ResourceSpec

instance Prelude.ToJSON ResourceSpec where
  toJSON ResourceSpec' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("InstanceType" Prelude..=)
              Prelude.<$> instanceType,
            ("SageMakerImageArn" Prelude..=)
              Prelude.<$> sageMakerImageArn,
            ("SageMakerImageVersionArn" Prelude..=)
              Prelude.<$> sageMakerImageVersionArn
          ]
      )
