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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionInferenceAcceleratorsDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionInferenceAcceleratorsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An Elastic Inference accelerator to use for the containers in the task.
--
-- /See:/ 'newAwsEcsTaskDefinitionInferenceAcceleratorsDetails' smart constructor.
data AwsEcsTaskDefinitionInferenceAcceleratorsDetails = AwsEcsTaskDefinitionInferenceAcceleratorsDetails'
  { -- | The Elastic Inference accelerator device name.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | The Elastic Inference accelerator type to use.
    deviceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionInferenceAcceleratorsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceName', 'awsEcsTaskDefinitionInferenceAcceleratorsDetails_deviceName' - The Elastic Inference accelerator device name.
--
-- 'deviceType', 'awsEcsTaskDefinitionInferenceAcceleratorsDetails_deviceType' - The Elastic Inference accelerator type to use.
newAwsEcsTaskDefinitionInferenceAcceleratorsDetails ::
  AwsEcsTaskDefinitionInferenceAcceleratorsDetails
newAwsEcsTaskDefinitionInferenceAcceleratorsDetails =
  AwsEcsTaskDefinitionInferenceAcceleratorsDetails'
    { deviceName =
        Prelude.Nothing,
      deviceType =
        Prelude.Nothing
    }

-- | The Elastic Inference accelerator device name.
awsEcsTaskDefinitionInferenceAcceleratorsDetails_deviceName :: Lens.Lens' AwsEcsTaskDefinitionInferenceAcceleratorsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionInferenceAcceleratorsDetails_deviceName = Lens.lens (\AwsEcsTaskDefinitionInferenceAcceleratorsDetails' {deviceName} -> deviceName) (\s@AwsEcsTaskDefinitionInferenceAcceleratorsDetails' {} a -> s {deviceName = a} :: AwsEcsTaskDefinitionInferenceAcceleratorsDetails)

-- | The Elastic Inference accelerator type to use.
awsEcsTaskDefinitionInferenceAcceleratorsDetails_deviceType :: Lens.Lens' AwsEcsTaskDefinitionInferenceAcceleratorsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionInferenceAcceleratorsDetails_deviceType = Lens.lens (\AwsEcsTaskDefinitionInferenceAcceleratorsDetails' {deviceType} -> deviceType) (\s@AwsEcsTaskDefinitionInferenceAcceleratorsDetails' {} a -> s {deviceType = a} :: AwsEcsTaskDefinitionInferenceAcceleratorsDetails)

instance
  Data.FromJSON
    AwsEcsTaskDefinitionInferenceAcceleratorsDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcsTaskDefinitionInferenceAcceleratorsDetails"
      ( \x ->
          AwsEcsTaskDefinitionInferenceAcceleratorsDetails'
            Prelude.<$> (x Data..:? "DeviceName")
              Prelude.<*> (x Data..:? "DeviceType")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionInferenceAcceleratorsDetails
  where
  hashWithSalt
    _salt
    AwsEcsTaskDefinitionInferenceAcceleratorsDetails' {..} =
      _salt `Prelude.hashWithSalt` deviceName
        `Prelude.hashWithSalt` deviceType

instance
  Prelude.NFData
    AwsEcsTaskDefinitionInferenceAcceleratorsDetails
  where
  rnf
    AwsEcsTaskDefinitionInferenceAcceleratorsDetails' {..} =
      Prelude.rnf deviceName
        `Prelude.seq` Prelude.rnf deviceType

instance
  Data.ToJSON
    AwsEcsTaskDefinitionInferenceAcceleratorsDetails
  where
  toJSON
    AwsEcsTaskDefinitionInferenceAcceleratorsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("DeviceName" Data..=) Prelude.<$> deviceName,
              ("DeviceType" Data..=) Prelude.<$> deviceType
            ]
        )
