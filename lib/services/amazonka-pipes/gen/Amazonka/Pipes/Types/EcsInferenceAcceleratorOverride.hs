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
-- Module      : Amazonka.Pipes.Types.EcsInferenceAcceleratorOverride
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.EcsInferenceAcceleratorOverride where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details on an Elastic Inference accelerator task override. This
-- parameter is used to override the Elastic Inference accelerator
-- specified in the task definition. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/userguide/ecs-inference.html Working with Amazon Elastic Inference on Amazon ECS>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- /See:/ 'newEcsInferenceAcceleratorOverride' smart constructor.
data EcsInferenceAcceleratorOverride = EcsInferenceAcceleratorOverride'
  { -- | The Elastic Inference accelerator device name to override for the task.
    -- This parameter must match a @deviceName@ specified in the task
    -- definition.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | The Elastic Inference accelerator type to use.
    deviceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EcsInferenceAcceleratorOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceName', 'ecsInferenceAcceleratorOverride_deviceName' - The Elastic Inference accelerator device name to override for the task.
-- This parameter must match a @deviceName@ specified in the task
-- definition.
--
-- 'deviceType', 'ecsInferenceAcceleratorOverride_deviceType' - The Elastic Inference accelerator type to use.
newEcsInferenceAcceleratorOverride ::
  EcsInferenceAcceleratorOverride
newEcsInferenceAcceleratorOverride =
  EcsInferenceAcceleratorOverride'
    { deviceName =
        Prelude.Nothing,
      deviceType = Prelude.Nothing
    }

-- | The Elastic Inference accelerator device name to override for the task.
-- This parameter must match a @deviceName@ specified in the task
-- definition.
ecsInferenceAcceleratorOverride_deviceName :: Lens.Lens' EcsInferenceAcceleratorOverride (Prelude.Maybe Prelude.Text)
ecsInferenceAcceleratorOverride_deviceName = Lens.lens (\EcsInferenceAcceleratorOverride' {deviceName} -> deviceName) (\s@EcsInferenceAcceleratorOverride' {} a -> s {deviceName = a} :: EcsInferenceAcceleratorOverride)

-- | The Elastic Inference accelerator type to use.
ecsInferenceAcceleratorOverride_deviceType :: Lens.Lens' EcsInferenceAcceleratorOverride (Prelude.Maybe Prelude.Text)
ecsInferenceAcceleratorOverride_deviceType = Lens.lens (\EcsInferenceAcceleratorOverride' {deviceType} -> deviceType) (\s@EcsInferenceAcceleratorOverride' {} a -> s {deviceType = a} :: EcsInferenceAcceleratorOverride)

instance
  Data.FromJSON
    EcsInferenceAcceleratorOverride
  where
  parseJSON =
    Data.withObject
      "EcsInferenceAcceleratorOverride"
      ( \x ->
          EcsInferenceAcceleratorOverride'
            Prelude.<$> (x Data..:? "deviceName")
            Prelude.<*> (x Data..:? "deviceType")
      )

instance
  Prelude.Hashable
    EcsInferenceAcceleratorOverride
  where
  hashWithSalt
    _salt
    EcsInferenceAcceleratorOverride' {..} =
      _salt
        `Prelude.hashWithSalt` deviceName
        `Prelude.hashWithSalt` deviceType

instance
  Prelude.NFData
    EcsInferenceAcceleratorOverride
  where
  rnf EcsInferenceAcceleratorOverride' {..} =
    Prelude.rnf deviceName `Prelude.seq`
      Prelude.rnf deviceType

instance Data.ToJSON EcsInferenceAcceleratorOverride where
  toJSON EcsInferenceAcceleratorOverride' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("deviceName" Data..=) Prelude.<$> deviceName,
            ("deviceType" Data..=) Prelude.<$> deviceType
          ]
      )
