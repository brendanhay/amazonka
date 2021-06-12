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
-- Module      : Network.AWS.IoTAnalytics.Types.DeviceShadowEnrichActivity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DeviceShadowEnrichActivity where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An activity that adds information from the AWS IoT Device Shadow service
-- to a message.
--
-- /See:/ 'newDeviceShadowEnrichActivity' smart constructor.
data DeviceShadowEnrichActivity = DeviceShadowEnrichActivity'
  { -- | The next activity in the pipeline.
    next :: Core.Maybe Core.Text,
    -- | The name of the @deviceShadowEnrich@ activity.
    name :: Core.Text,
    -- | The name of the attribute that is added to the message.
    attribute :: Core.Text,
    -- | The name of the IoT device whose shadow information is added to the
    -- message.
    thingName :: Core.Text,
    -- | The ARN of the role that allows access to the device\'s shadow.
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeviceShadowEnrichActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'next', 'deviceShadowEnrichActivity_next' - The next activity in the pipeline.
--
-- 'name', 'deviceShadowEnrichActivity_name' - The name of the @deviceShadowEnrich@ activity.
--
-- 'attribute', 'deviceShadowEnrichActivity_attribute' - The name of the attribute that is added to the message.
--
-- 'thingName', 'deviceShadowEnrichActivity_thingName' - The name of the IoT device whose shadow information is added to the
-- message.
--
-- 'roleArn', 'deviceShadowEnrichActivity_roleArn' - The ARN of the role that allows access to the device\'s shadow.
newDeviceShadowEnrichActivity ::
  -- | 'name'
  Core.Text ->
  -- | 'attribute'
  Core.Text ->
  -- | 'thingName'
  Core.Text ->
  -- | 'roleArn'
  Core.Text ->
  DeviceShadowEnrichActivity
newDeviceShadowEnrichActivity
  pName_
  pAttribute_
  pThingName_
  pRoleArn_ =
    DeviceShadowEnrichActivity'
      { next = Core.Nothing,
        name = pName_,
        attribute = pAttribute_,
        thingName = pThingName_,
        roleArn = pRoleArn_
      }

-- | The next activity in the pipeline.
deviceShadowEnrichActivity_next :: Lens.Lens' DeviceShadowEnrichActivity (Core.Maybe Core.Text)
deviceShadowEnrichActivity_next = Lens.lens (\DeviceShadowEnrichActivity' {next} -> next) (\s@DeviceShadowEnrichActivity' {} a -> s {next = a} :: DeviceShadowEnrichActivity)

-- | The name of the @deviceShadowEnrich@ activity.
deviceShadowEnrichActivity_name :: Lens.Lens' DeviceShadowEnrichActivity Core.Text
deviceShadowEnrichActivity_name = Lens.lens (\DeviceShadowEnrichActivity' {name} -> name) (\s@DeviceShadowEnrichActivity' {} a -> s {name = a} :: DeviceShadowEnrichActivity)

-- | The name of the attribute that is added to the message.
deviceShadowEnrichActivity_attribute :: Lens.Lens' DeviceShadowEnrichActivity Core.Text
deviceShadowEnrichActivity_attribute = Lens.lens (\DeviceShadowEnrichActivity' {attribute} -> attribute) (\s@DeviceShadowEnrichActivity' {} a -> s {attribute = a} :: DeviceShadowEnrichActivity)

-- | The name of the IoT device whose shadow information is added to the
-- message.
deviceShadowEnrichActivity_thingName :: Lens.Lens' DeviceShadowEnrichActivity Core.Text
deviceShadowEnrichActivity_thingName = Lens.lens (\DeviceShadowEnrichActivity' {thingName} -> thingName) (\s@DeviceShadowEnrichActivity' {} a -> s {thingName = a} :: DeviceShadowEnrichActivity)

-- | The ARN of the role that allows access to the device\'s shadow.
deviceShadowEnrichActivity_roleArn :: Lens.Lens' DeviceShadowEnrichActivity Core.Text
deviceShadowEnrichActivity_roleArn = Lens.lens (\DeviceShadowEnrichActivity' {roleArn} -> roleArn) (\s@DeviceShadowEnrichActivity' {} a -> s {roleArn = a} :: DeviceShadowEnrichActivity)

instance Core.FromJSON DeviceShadowEnrichActivity where
  parseJSON =
    Core.withObject
      "DeviceShadowEnrichActivity"
      ( \x ->
          DeviceShadowEnrichActivity'
            Core.<$> (x Core..:? "next")
            Core.<*> (x Core..: "name")
            Core.<*> (x Core..: "attribute")
            Core.<*> (x Core..: "thingName")
            Core.<*> (x Core..: "roleArn")
      )

instance Core.Hashable DeviceShadowEnrichActivity

instance Core.NFData DeviceShadowEnrichActivity

instance Core.ToJSON DeviceShadowEnrichActivity where
  toJSON DeviceShadowEnrichActivity' {..} =
    Core.object
      ( Core.catMaybes
          [ ("next" Core..=) Core.<$> next,
            Core.Just ("name" Core..= name),
            Core.Just ("attribute" Core..= attribute),
            Core.Just ("thingName" Core..= thingName),
            Core.Just ("roleArn" Core..= roleArn)
          ]
      )
