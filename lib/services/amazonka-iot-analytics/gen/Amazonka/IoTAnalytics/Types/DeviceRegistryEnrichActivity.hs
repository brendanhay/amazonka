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
-- Module      : Amazonka.IoTAnalytics.Types.DeviceRegistryEnrichActivity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.DeviceRegistryEnrichActivity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An activity that adds data from the IoT device registry to your message.
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON DeviceRegistryEnrichActivity where
  parseJSON =
    Data.withObject
      "DeviceRegistryEnrichActivity"
      ( \x ->
          DeviceRegistryEnrichActivity'
            Prelude.<$> (x Data..:? "next")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "attribute")
            Prelude.<*> (x Data..: "thingName")
            Prelude.<*> (x Data..: "roleArn")
      )

instance
  Prelude.Hashable
    DeviceRegistryEnrichActivity
  where
  hashWithSalt _salt DeviceRegistryEnrichActivity' {..} =
    _salt
      `Prelude.hashWithSalt` next
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` thingName
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData DeviceRegistryEnrichActivity where
  rnf DeviceRegistryEnrichActivity' {..} =
    Prelude.rnf next
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf thingName
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToJSON DeviceRegistryEnrichActivity where
  toJSON DeviceRegistryEnrichActivity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("next" Data..=) Prelude.<$> next,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("attribute" Data..= attribute),
            Prelude.Just ("thingName" Data..= thingName),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )
