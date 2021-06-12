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
-- Module      : Network.AWS.IoTAnalytics.Types.DeviceRegistryEnrichActivity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DeviceRegistryEnrichActivity where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An activity that adds data from the AWS IoT device registry to your
-- message.
--
-- /See:/ 'newDeviceRegistryEnrichActivity' smart constructor.
data DeviceRegistryEnrichActivity = DeviceRegistryEnrichActivity'
  { -- | The next activity in the pipeline.
    next :: Core.Maybe Core.Text,
    -- | The name of the @deviceRegistryEnrich@ activity.
    name :: Core.Text,
    -- | The name of the attribute that is added to the message.
    attribute :: Core.Text,
    -- | The name of the IoT device whose registry information is added to the
    -- message.
    thingName :: Core.Text,
    -- | The ARN of the role that allows access to the device\'s registry
    -- information.
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeviceRegistryEnrichActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'next', 'deviceRegistryEnrichActivity_next' - The next activity in the pipeline.
--
-- 'name', 'deviceRegistryEnrichActivity_name' - The name of the @deviceRegistryEnrich@ activity.
--
-- 'attribute', 'deviceRegistryEnrichActivity_attribute' - The name of the attribute that is added to the message.
--
-- 'thingName', 'deviceRegistryEnrichActivity_thingName' - The name of the IoT device whose registry information is added to the
-- message.
--
-- 'roleArn', 'deviceRegistryEnrichActivity_roleArn' - The ARN of the role that allows access to the device\'s registry
-- information.
newDeviceRegistryEnrichActivity ::
  -- | 'name'
  Core.Text ->
  -- | 'attribute'
  Core.Text ->
  -- | 'thingName'
  Core.Text ->
  -- | 'roleArn'
  Core.Text ->
  DeviceRegistryEnrichActivity
newDeviceRegistryEnrichActivity
  pName_
  pAttribute_
  pThingName_
  pRoleArn_ =
    DeviceRegistryEnrichActivity'
      { next = Core.Nothing,
        name = pName_,
        attribute = pAttribute_,
        thingName = pThingName_,
        roleArn = pRoleArn_
      }

-- | The next activity in the pipeline.
deviceRegistryEnrichActivity_next :: Lens.Lens' DeviceRegistryEnrichActivity (Core.Maybe Core.Text)
deviceRegistryEnrichActivity_next = Lens.lens (\DeviceRegistryEnrichActivity' {next} -> next) (\s@DeviceRegistryEnrichActivity' {} a -> s {next = a} :: DeviceRegistryEnrichActivity)

-- | The name of the @deviceRegistryEnrich@ activity.
deviceRegistryEnrichActivity_name :: Lens.Lens' DeviceRegistryEnrichActivity Core.Text
deviceRegistryEnrichActivity_name = Lens.lens (\DeviceRegistryEnrichActivity' {name} -> name) (\s@DeviceRegistryEnrichActivity' {} a -> s {name = a} :: DeviceRegistryEnrichActivity)

-- | The name of the attribute that is added to the message.
deviceRegistryEnrichActivity_attribute :: Lens.Lens' DeviceRegistryEnrichActivity Core.Text
deviceRegistryEnrichActivity_attribute = Lens.lens (\DeviceRegistryEnrichActivity' {attribute} -> attribute) (\s@DeviceRegistryEnrichActivity' {} a -> s {attribute = a} :: DeviceRegistryEnrichActivity)

-- | The name of the IoT device whose registry information is added to the
-- message.
deviceRegistryEnrichActivity_thingName :: Lens.Lens' DeviceRegistryEnrichActivity Core.Text
deviceRegistryEnrichActivity_thingName = Lens.lens (\DeviceRegistryEnrichActivity' {thingName} -> thingName) (\s@DeviceRegistryEnrichActivity' {} a -> s {thingName = a} :: DeviceRegistryEnrichActivity)

-- | The ARN of the role that allows access to the device\'s registry
-- information.
deviceRegistryEnrichActivity_roleArn :: Lens.Lens' DeviceRegistryEnrichActivity Core.Text
deviceRegistryEnrichActivity_roleArn = Lens.lens (\DeviceRegistryEnrichActivity' {roleArn} -> roleArn) (\s@DeviceRegistryEnrichActivity' {} a -> s {roleArn = a} :: DeviceRegistryEnrichActivity)

instance Core.FromJSON DeviceRegistryEnrichActivity where
  parseJSON =
    Core.withObject
      "DeviceRegistryEnrichActivity"
      ( \x ->
          DeviceRegistryEnrichActivity'
            Core.<$> (x Core..:? "next")
            Core.<*> (x Core..: "name")
            Core.<*> (x Core..: "attribute")
            Core.<*> (x Core..: "thingName")
            Core.<*> (x Core..: "roleArn")
      )

instance Core.Hashable DeviceRegistryEnrichActivity

instance Core.NFData DeviceRegistryEnrichActivity

instance Core.ToJSON DeviceRegistryEnrichActivity where
  toJSON DeviceRegistryEnrichActivity' {..} =
    Core.object
      ( Core.catMaybes
          [ ("next" Core..=) Core.<$> next,
            Core.Just ("name" Core..= name),
            Core.Just ("attribute" Core..= attribute),
            Core.Just ("thingName" Core..= thingName),
            Core.Just ("roleArn" Core..= roleArn)
          ]
      )
