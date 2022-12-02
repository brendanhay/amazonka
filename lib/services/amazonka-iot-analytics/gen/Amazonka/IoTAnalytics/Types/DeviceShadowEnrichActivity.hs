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
-- Module      : Amazonka.IoTAnalytics.Types.DeviceShadowEnrichActivity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.DeviceShadowEnrichActivity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An activity that adds information from the IoT Device Shadow service to
-- a message.
--
-- /See:/ 'newDeviceShadowEnrichActivity' smart constructor.
data DeviceShadowEnrichActivity = DeviceShadowEnrichActivity'
  { -- | The next activity in the pipeline.
    next :: Prelude.Maybe Prelude.Text,
    -- | The name of the @deviceShadowEnrich@ activity.
    name :: Prelude.Text,
    -- | The name of the attribute that is added to the message.
    attribute :: Prelude.Text,
    -- | The name of the IoT device whose shadow information is added to the
    -- message.
    thingName :: Prelude.Text,
    -- | The ARN of the role that allows access to the device\'s shadow.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'attribute'
  Prelude.Text ->
  -- | 'thingName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  DeviceShadowEnrichActivity
newDeviceShadowEnrichActivity
  pName_
  pAttribute_
  pThingName_
  pRoleArn_ =
    DeviceShadowEnrichActivity'
      { next = Prelude.Nothing,
        name = pName_,
        attribute = pAttribute_,
        thingName = pThingName_,
        roleArn = pRoleArn_
      }

-- | The next activity in the pipeline.
deviceShadowEnrichActivity_next :: Lens.Lens' DeviceShadowEnrichActivity (Prelude.Maybe Prelude.Text)
deviceShadowEnrichActivity_next = Lens.lens (\DeviceShadowEnrichActivity' {next} -> next) (\s@DeviceShadowEnrichActivity' {} a -> s {next = a} :: DeviceShadowEnrichActivity)

-- | The name of the @deviceShadowEnrich@ activity.
deviceShadowEnrichActivity_name :: Lens.Lens' DeviceShadowEnrichActivity Prelude.Text
deviceShadowEnrichActivity_name = Lens.lens (\DeviceShadowEnrichActivity' {name} -> name) (\s@DeviceShadowEnrichActivity' {} a -> s {name = a} :: DeviceShadowEnrichActivity)

-- | The name of the attribute that is added to the message.
deviceShadowEnrichActivity_attribute :: Lens.Lens' DeviceShadowEnrichActivity Prelude.Text
deviceShadowEnrichActivity_attribute = Lens.lens (\DeviceShadowEnrichActivity' {attribute} -> attribute) (\s@DeviceShadowEnrichActivity' {} a -> s {attribute = a} :: DeviceShadowEnrichActivity)

-- | The name of the IoT device whose shadow information is added to the
-- message.
deviceShadowEnrichActivity_thingName :: Lens.Lens' DeviceShadowEnrichActivity Prelude.Text
deviceShadowEnrichActivity_thingName = Lens.lens (\DeviceShadowEnrichActivity' {thingName} -> thingName) (\s@DeviceShadowEnrichActivity' {} a -> s {thingName = a} :: DeviceShadowEnrichActivity)

-- | The ARN of the role that allows access to the device\'s shadow.
deviceShadowEnrichActivity_roleArn :: Lens.Lens' DeviceShadowEnrichActivity Prelude.Text
deviceShadowEnrichActivity_roleArn = Lens.lens (\DeviceShadowEnrichActivity' {roleArn} -> roleArn) (\s@DeviceShadowEnrichActivity' {} a -> s {roleArn = a} :: DeviceShadowEnrichActivity)

instance Data.FromJSON DeviceShadowEnrichActivity where
  parseJSON =
    Data.withObject
      "DeviceShadowEnrichActivity"
      ( \x ->
          DeviceShadowEnrichActivity'
            Prelude.<$> (x Data..:? "next")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "attribute")
            Prelude.<*> (x Data..: "thingName")
            Prelude.<*> (x Data..: "roleArn")
      )

instance Prelude.Hashable DeviceShadowEnrichActivity where
  hashWithSalt _salt DeviceShadowEnrichActivity' {..} =
    _salt `Prelude.hashWithSalt` next
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` thingName
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData DeviceShadowEnrichActivity where
  rnf DeviceShadowEnrichActivity' {..} =
    Prelude.rnf next
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf thingName
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToJSON DeviceShadowEnrichActivity where
  toJSON DeviceShadowEnrichActivity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("next" Data..=) Prelude.<$> next,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("attribute" Data..= attribute),
            Prelude.Just ("thingName" Data..= thingName),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )
