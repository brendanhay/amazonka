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
-- Module      : Amazonka.ECS.Types.InferenceAccelerator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.InferenceAccelerator where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details on an Elastic Inference accelerator. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-inference.html Working with Amazon Elastic Inference on Amazon ECS>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- /See:/ 'newInferenceAccelerator' smart constructor.
data InferenceAccelerator = InferenceAccelerator'
  { -- | The Elastic Inference accelerator device name. The @deviceName@ must
    -- also be referenced in a container definition as a ResourceRequirement.
    deviceName :: Prelude.Text,
    -- | The Elastic Inference accelerator type to use.
    deviceType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'deviceType'
  Prelude.Text ->
  InferenceAccelerator
newInferenceAccelerator pDeviceName_ pDeviceType_ =
  InferenceAccelerator'
    { deviceName = pDeviceName_,
      deviceType = pDeviceType_
    }

-- | The Elastic Inference accelerator device name. The @deviceName@ must
-- also be referenced in a container definition as a ResourceRequirement.
inferenceAccelerator_deviceName :: Lens.Lens' InferenceAccelerator Prelude.Text
inferenceAccelerator_deviceName = Lens.lens (\InferenceAccelerator' {deviceName} -> deviceName) (\s@InferenceAccelerator' {} a -> s {deviceName = a} :: InferenceAccelerator)

-- | The Elastic Inference accelerator type to use.
inferenceAccelerator_deviceType :: Lens.Lens' InferenceAccelerator Prelude.Text
inferenceAccelerator_deviceType = Lens.lens (\InferenceAccelerator' {deviceType} -> deviceType) (\s@InferenceAccelerator' {} a -> s {deviceType = a} :: InferenceAccelerator)

instance Data.FromJSON InferenceAccelerator where
  parseJSON =
    Data.withObject
      "InferenceAccelerator"
      ( \x ->
          InferenceAccelerator'
            Prelude.<$> (x Data..: "deviceName")
            Prelude.<*> (x Data..: "deviceType")
      )

instance Prelude.Hashable InferenceAccelerator where
  hashWithSalt _salt InferenceAccelerator' {..} =
    _salt
      `Prelude.hashWithSalt` deviceName
      `Prelude.hashWithSalt` deviceType

instance Prelude.NFData InferenceAccelerator where
  rnf InferenceAccelerator' {..} =
    Prelude.rnf deviceName
      `Prelude.seq` Prelude.rnf deviceType

instance Data.ToJSON InferenceAccelerator where
  toJSON InferenceAccelerator' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("deviceName" Data..= deviceName),
            Prelude.Just ("deviceType" Data..= deviceType)
          ]
      )
