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
-- Module      : Network.AWS.IoTAnalytics.Types.DeviceRegistryEnrichActivity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DeviceRegistryEnrichActivity where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An activity that adds data from the AWS IoT device registry to your
-- message.
--
-- /See:/ 'newDeviceRegistryEnrichActivity' smart constructor.
data DeviceRegistryEnrichActivity = DeviceRegistryEnrichActivity'
  { -- | The next activity in the pipeline.
    next :: Prelude.Maybe Prelude.Text,
    -- | The name of the @deviceRegistryEnrich@ activity.
    name :: Prelude.Text,
    -- | The name of the attribute that is added to the message.
    attribute :: Prelude.Text,
    -- | The name of the IoT device whose registry information is added to the
    -- message.
    thingName :: Prelude.Text,
    -- | The ARN of the role that allows access to the device\'s registry
    -- information.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'attribute'
  Prelude.Text ->
  -- | 'thingName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  DeviceRegistryEnrichActivity
newDeviceRegistryEnrichActivity
  pName_
  pAttribute_
  pThingName_
  pRoleArn_ =
    DeviceRegistryEnrichActivity'
      { next =
          Prelude.Nothing,
        name = pName_,
        attribute = pAttribute_,
        thingName = pThingName_,
        roleArn = pRoleArn_
      }

-- | The next activity in the pipeline.
deviceRegistryEnrichActivity_next :: Lens.Lens' DeviceRegistryEnrichActivity (Prelude.Maybe Prelude.Text)
deviceRegistryEnrichActivity_next = Lens.lens (\DeviceRegistryEnrichActivity' {next} -> next) (\s@DeviceRegistryEnrichActivity' {} a -> s {next = a} :: DeviceRegistryEnrichActivity)

-- | The name of the @deviceRegistryEnrich@ activity.
deviceRegistryEnrichActivity_name :: Lens.Lens' DeviceRegistryEnrichActivity Prelude.Text
deviceRegistryEnrichActivity_name = Lens.lens (\DeviceRegistryEnrichActivity' {name} -> name) (\s@DeviceRegistryEnrichActivity' {} a -> s {name = a} :: DeviceRegistryEnrichActivity)

-- | The name of the attribute that is added to the message.
deviceRegistryEnrichActivity_attribute :: Lens.Lens' DeviceRegistryEnrichActivity Prelude.Text
deviceRegistryEnrichActivity_attribute = Lens.lens (\DeviceRegistryEnrichActivity' {attribute} -> attribute) (\s@DeviceRegistryEnrichActivity' {} a -> s {attribute = a} :: DeviceRegistryEnrichActivity)

-- | The name of the IoT device whose registry information is added to the
-- message.
deviceRegistryEnrichActivity_thingName :: Lens.Lens' DeviceRegistryEnrichActivity Prelude.Text
deviceRegistryEnrichActivity_thingName = Lens.lens (\DeviceRegistryEnrichActivity' {thingName} -> thingName) (\s@DeviceRegistryEnrichActivity' {} a -> s {thingName = a} :: DeviceRegistryEnrichActivity)

-- | The ARN of the role that allows access to the device\'s registry
-- information.
deviceRegistryEnrichActivity_roleArn :: Lens.Lens' DeviceRegistryEnrichActivity Prelude.Text
deviceRegistryEnrichActivity_roleArn = Lens.lens (\DeviceRegistryEnrichActivity' {roleArn} -> roleArn) (\s@DeviceRegistryEnrichActivity' {} a -> s {roleArn = a} :: DeviceRegistryEnrichActivity)

instance
  Prelude.FromJSON
    DeviceRegistryEnrichActivity
  where
  parseJSON =
    Prelude.withObject
      "DeviceRegistryEnrichActivity"
      ( \x ->
          DeviceRegistryEnrichActivity'
            Prelude.<$> (x Prelude..:? "next")
            Prelude.<*> (x Prelude..: "name")
            Prelude.<*> (x Prelude..: "attribute")
            Prelude.<*> (x Prelude..: "thingName")
            Prelude.<*> (x Prelude..: "roleArn")
      )

instance
  Prelude.Hashable
    DeviceRegistryEnrichActivity

instance Prelude.NFData DeviceRegistryEnrichActivity

instance Prelude.ToJSON DeviceRegistryEnrichActivity where
  toJSON DeviceRegistryEnrichActivity' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("next" Prelude..=) Prelude.<$> next,
            Prelude.Just ("name" Prelude..= name),
            Prelude.Just ("attribute" Prelude..= attribute),
            Prelude.Just ("thingName" Prelude..= thingName),
            Prelude.Just ("roleArn" Prelude..= roleArn)
          ]
      )
