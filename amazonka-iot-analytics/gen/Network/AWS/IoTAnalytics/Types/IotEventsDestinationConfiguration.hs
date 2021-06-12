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
-- Module      : Network.AWS.IoTAnalytics.Types.IotEventsDestinationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.IotEventsDestinationConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Configuration information for delivery of dataset contents to AWS IoT
-- Events.
--
-- /See:/ 'newIotEventsDestinationConfiguration' smart constructor.
data IotEventsDestinationConfiguration = IotEventsDestinationConfiguration'
  { -- | The name of the AWS IoT Events input to which dataset contents are
    -- delivered.
    inputName :: Core.Text,
    -- | The ARN of the role that grants AWS IoT Analytics permission to deliver
    -- dataset contents to an AWS IoT Events input.
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'IotEventsDestinationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputName', 'iotEventsDestinationConfiguration_inputName' - The name of the AWS IoT Events input to which dataset contents are
-- delivered.
--
-- 'roleArn', 'iotEventsDestinationConfiguration_roleArn' - The ARN of the role that grants AWS IoT Analytics permission to deliver
-- dataset contents to an AWS IoT Events input.
newIotEventsDestinationConfiguration ::
  -- | 'inputName'
  Core.Text ->
  -- | 'roleArn'
  Core.Text ->
  IotEventsDestinationConfiguration
newIotEventsDestinationConfiguration
  pInputName_
  pRoleArn_ =
    IotEventsDestinationConfiguration'
      { inputName =
          pInputName_,
        roleArn = pRoleArn_
      }

-- | The name of the AWS IoT Events input to which dataset contents are
-- delivered.
iotEventsDestinationConfiguration_inputName :: Lens.Lens' IotEventsDestinationConfiguration Core.Text
iotEventsDestinationConfiguration_inputName = Lens.lens (\IotEventsDestinationConfiguration' {inputName} -> inputName) (\s@IotEventsDestinationConfiguration' {} a -> s {inputName = a} :: IotEventsDestinationConfiguration)

-- | The ARN of the role that grants AWS IoT Analytics permission to deliver
-- dataset contents to an AWS IoT Events input.
iotEventsDestinationConfiguration_roleArn :: Lens.Lens' IotEventsDestinationConfiguration Core.Text
iotEventsDestinationConfiguration_roleArn = Lens.lens (\IotEventsDestinationConfiguration' {roleArn} -> roleArn) (\s@IotEventsDestinationConfiguration' {} a -> s {roleArn = a} :: IotEventsDestinationConfiguration)

instance
  Core.FromJSON
    IotEventsDestinationConfiguration
  where
  parseJSON =
    Core.withObject
      "IotEventsDestinationConfiguration"
      ( \x ->
          IotEventsDestinationConfiguration'
            Core.<$> (x Core..: "inputName")
            Core.<*> (x Core..: "roleArn")
      )

instance
  Core.Hashable
    IotEventsDestinationConfiguration

instance
  Core.NFData
    IotEventsDestinationConfiguration

instance
  Core.ToJSON
    IotEventsDestinationConfiguration
  where
  toJSON IotEventsDestinationConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("inputName" Core..= inputName),
            Core.Just ("roleArn" Core..= roleArn)
          ]
      )
