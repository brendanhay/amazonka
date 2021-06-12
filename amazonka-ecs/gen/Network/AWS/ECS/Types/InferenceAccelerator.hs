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
-- Module      : Network.AWS.ECS.Types.InferenceAccelerator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.InferenceAccelerator where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Details on a Elastic Inference accelerator. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-eia.html Working with Amazon Elastic Inference on Amazon ECS>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- /See:/ 'newInferenceAccelerator' smart constructor.
data InferenceAccelerator = InferenceAccelerator'
  { -- | The Elastic Inference accelerator device name. The @deviceName@ must
    -- also be referenced in a container definition as a ResourceRequirement.
    deviceName :: Core.Text,
    -- | The Elastic Inference accelerator type to use.
    deviceType :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InferenceAccelerator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceName', 'inferenceAccelerator_deviceName' - The Elastic Inference accelerator device name. The @deviceName@ must
-- also be referenced in a container definition as a ResourceRequirement.
--
-- 'deviceType', 'inferenceAccelerator_deviceType' - The Elastic Inference accelerator type to use.
newInferenceAccelerator ::
  -- | 'deviceName'
  Core.Text ->
  -- | 'deviceType'
  Core.Text ->
  InferenceAccelerator
newInferenceAccelerator pDeviceName_ pDeviceType_ =
  InferenceAccelerator'
    { deviceName = pDeviceName_,
      deviceType = pDeviceType_
    }

-- | The Elastic Inference accelerator device name. The @deviceName@ must
-- also be referenced in a container definition as a ResourceRequirement.
inferenceAccelerator_deviceName :: Lens.Lens' InferenceAccelerator Core.Text
inferenceAccelerator_deviceName = Lens.lens (\InferenceAccelerator' {deviceName} -> deviceName) (\s@InferenceAccelerator' {} a -> s {deviceName = a} :: InferenceAccelerator)

-- | The Elastic Inference accelerator type to use.
inferenceAccelerator_deviceType :: Lens.Lens' InferenceAccelerator Core.Text
inferenceAccelerator_deviceType = Lens.lens (\InferenceAccelerator' {deviceType} -> deviceType) (\s@InferenceAccelerator' {} a -> s {deviceType = a} :: InferenceAccelerator)

instance Core.FromJSON InferenceAccelerator where
  parseJSON =
    Core.withObject
      "InferenceAccelerator"
      ( \x ->
          InferenceAccelerator'
            Core.<$> (x Core..: "deviceName")
            Core.<*> (x Core..: "deviceType")
      )

instance Core.Hashable InferenceAccelerator

instance Core.NFData InferenceAccelerator

instance Core.ToJSON InferenceAccelerator where
  toJSON InferenceAccelerator' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("deviceName" Core..= deviceName),
            Core.Just ("deviceType" Core..= deviceType)
          ]
      )
