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
-- Module      : Network.AWS.ECS.Types.InferenceAcceleratorOverride
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.InferenceAcceleratorOverride where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details on an Elastic Inference accelerator task override. This
-- parameter is used to override the Elastic Inference accelerator
-- specified in the task definition. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-eia.html Working with Amazon Elastic Inference on Amazon ECS>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- /See:/ 'newInferenceAcceleratorOverride' smart constructor.
data InferenceAcceleratorOverride = InferenceAcceleratorOverride'
  { -- | The Elastic Inference accelerator device name to override for the task.
    -- This parameter must match a @deviceName@ specified in the task
    -- definition.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | The Elastic Inference accelerator type to use.
    deviceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InferenceAcceleratorOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceName', 'inferenceAcceleratorOverride_deviceName' - The Elastic Inference accelerator device name to override for the task.
-- This parameter must match a @deviceName@ specified in the task
-- definition.
--
-- 'deviceType', 'inferenceAcceleratorOverride_deviceType' - The Elastic Inference accelerator type to use.
newInferenceAcceleratorOverride ::
  InferenceAcceleratorOverride
newInferenceAcceleratorOverride =
  InferenceAcceleratorOverride'
    { deviceName =
        Prelude.Nothing,
      deviceType = Prelude.Nothing
    }

-- | The Elastic Inference accelerator device name to override for the task.
-- This parameter must match a @deviceName@ specified in the task
-- definition.
inferenceAcceleratorOverride_deviceName :: Lens.Lens' InferenceAcceleratorOverride (Prelude.Maybe Prelude.Text)
inferenceAcceleratorOverride_deviceName = Lens.lens (\InferenceAcceleratorOverride' {deviceName} -> deviceName) (\s@InferenceAcceleratorOverride' {} a -> s {deviceName = a} :: InferenceAcceleratorOverride)

-- | The Elastic Inference accelerator type to use.
inferenceAcceleratorOverride_deviceType :: Lens.Lens' InferenceAcceleratorOverride (Prelude.Maybe Prelude.Text)
inferenceAcceleratorOverride_deviceType = Lens.lens (\InferenceAcceleratorOverride' {deviceType} -> deviceType) (\s@InferenceAcceleratorOverride' {} a -> s {deviceType = a} :: InferenceAcceleratorOverride)

instance
  Prelude.FromJSON
    InferenceAcceleratorOverride
  where
  parseJSON =
    Prelude.withObject
      "InferenceAcceleratorOverride"
      ( \x ->
          InferenceAcceleratorOverride'
            Prelude.<$> (x Prelude..:? "deviceName")
            Prelude.<*> (x Prelude..:? "deviceType")
      )

instance
  Prelude.Hashable
    InferenceAcceleratorOverride

instance Prelude.NFData InferenceAcceleratorOverride

instance Prelude.ToJSON InferenceAcceleratorOverride where
  toJSON InferenceAcceleratorOverride' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("deviceName" Prelude..=) Prelude.<$> deviceName,
            ("deviceType" Prelude..=) Prelude.<$> deviceType
          ]
      )
