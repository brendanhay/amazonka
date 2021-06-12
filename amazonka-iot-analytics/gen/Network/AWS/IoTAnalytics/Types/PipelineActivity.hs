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
-- Module      : Network.AWS.IoTAnalytics.Types.PipelineActivity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.PipelineActivity where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types.AddAttributesActivity
import Network.AWS.IoTAnalytics.Types.ChannelActivity
import Network.AWS.IoTAnalytics.Types.DatastoreActivity
import Network.AWS.IoTAnalytics.Types.DeviceRegistryEnrichActivity
import Network.AWS.IoTAnalytics.Types.DeviceShadowEnrichActivity
import Network.AWS.IoTAnalytics.Types.FilterActivity
import Network.AWS.IoTAnalytics.Types.LambdaActivity
import Network.AWS.IoTAnalytics.Types.MathActivity
import Network.AWS.IoTAnalytics.Types.RemoveAttributesActivity
import Network.AWS.IoTAnalytics.Types.SelectAttributesActivity
import qualified Network.AWS.Lens as Lens

-- | An activity that performs a transformation on a message.
--
-- /See:/ 'newPipelineActivity' smart constructor.
data PipelineActivity = PipelineActivity'
  { -- | Creates a new message using only the specified attributes from the
    -- original message.
    selectAttributes :: Core.Maybe SelectAttributesActivity,
    -- | Specifies where to store the processed message data.
    datastore :: Core.Maybe DatastoreActivity,
    -- | Removes attributes from a message.
    removeAttributes :: Core.Maybe RemoveAttributesActivity,
    -- | Adds other attributes based on existing attributes in the message.
    addAttributes :: Core.Maybe AddAttributesActivity,
    -- | Adds information from the AWS IoT Device Shadow service to a message.
    deviceShadowEnrich :: Core.Maybe DeviceShadowEnrichActivity,
    -- | Runs a Lambda function to modify the message.
    lambda :: Core.Maybe LambdaActivity,
    -- | Adds data from the AWS IoT device registry to your message.
    deviceRegistryEnrich :: Core.Maybe DeviceRegistryEnrichActivity,
    -- | Determines the source of the messages to be processed.
    channel :: Core.Maybe ChannelActivity,
    -- | Filters a message based on its attributes.
    filter' :: Core.Maybe FilterActivity,
    -- | Computes an arithmetic expression using the message\'s attributes and
    -- adds it to the message.
    math :: Core.Maybe MathActivity
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PipelineActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'selectAttributes', 'pipelineActivity_selectAttributes' - Creates a new message using only the specified attributes from the
-- original message.
--
-- 'datastore', 'pipelineActivity_datastore' - Specifies where to store the processed message data.
--
-- 'removeAttributes', 'pipelineActivity_removeAttributes' - Removes attributes from a message.
--
-- 'addAttributes', 'pipelineActivity_addAttributes' - Adds other attributes based on existing attributes in the message.
--
-- 'deviceShadowEnrich', 'pipelineActivity_deviceShadowEnrich' - Adds information from the AWS IoT Device Shadow service to a message.
--
-- 'lambda', 'pipelineActivity_lambda' - Runs a Lambda function to modify the message.
--
-- 'deviceRegistryEnrich', 'pipelineActivity_deviceRegistryEnrich' - Adds data from the AWS IoT device registry to your message.
--
-- 'channel', 'pipelineActivity_channel' - Determines the source of the messages to be processed.
--
-- 'filter'', 'pipelineActivity_filter' - Filters a message based on its attributes.
--
-- 'math', 'pipelineActivity_math' - Computes an arithmetic expression using the message\'s attributes and
-- adds it to the message.
newPipelineActivity ::
  PipelineActivity
newPipelineActivity =
  PipelineActivity'
    { selectAttributes = Core.Nothing,
      datastore = Core.Nothing,
      removeAttributes = Core.Nothing,
      addAttributes = Core.Nothing,
      deviceShadowEnrich = Core.Nothing,
      lambda = Core.Nothing,
      deviceRegistryEnrich = Core.Nothing,
      channel = Core.Nothing,
      filter' = Core.Nothing,
      math = Core.Nothing
    }

-- | Creates a new message using only the specified attributes from the
-- original message.
pipelineActivity_selectAttributes :: Lens.Lens' PipelineActivity (Core.Maybe SelectAttributesActivity)
pipelineActivity_selectAttributes = Lens.lens (\PipelineActivity' {selectAttributes} -> selectAttributes) (\s@PipelineActivity' {} a -> s {selectAttributes = a} :: PipelineActivity)

-- | Specifies where to store the processed message data.
pipelineActivity_datastore :: Lens.Lens' PipelineActivity (Core.Maybe DatastoreActivity)
pipelineActivity_datastore = Lens.lens (\PipelineActivity' {datastore} -> datastore) (\s@PipelineActivity' {} a -> s {datastore = a} :: PipelineActivity)

-- | Removes attributes from a message.
pipelineActivity_removeAttributes :: Lens.Lens' PipelineActivity (Core.Maybe RemoveAttributesActivity)
pipelineActivity_removeAttributes = Lens.lens (\PipelineActivity' {removeAttributes} -> removeAttributes) (\s@PipelineActivity' {} a -> s {removeAttributes = a} :: PipelineActivity)

-- | Adds other attributes based on existing attributes in the message.
pipelineActivity_addAttributes :: Lens.Lens' PipelineActivity (Core.Maybe AddAttributesActivity)
pipelineActivity_addAttributes = Lens.lens (\PipelineActivity' {addAttributes} -> addAttributes) (\s@PipelineActivity' {} a -> s {addAttributes = a} :: PipelineActivity)

-- | Adds information from the AWS IoT Device Shadow service to a message.
pipelineActivity_deviceShadowEnrich :: Lens.Lens' PipelineActivity (Core.Maybe DeviceShadowEnrichActivity)
pipelineActivity_deviceShadowEnrich = Lens.lens (\PipelineActivity' {deviceShadowEnrich} -> deviceShadowEnrich) (\s@PipelineActivity' {} a -> s {deviceShadowEnrich = a} :: PipelineActivity)

-- | Runs a Lambda function to modify the message.
pipelineActivity_lambda :: Lens.Lens' PipelineActivity (Core.Maybe LambdaActivity)
pipelineActivity_lambda = Lens.lens (\PipelineActivity' {lambda} -> lambda) (\s@PipelineActivity' {} a -> s {lambda = a} :: PipelineActivity)

-- | Adds data from the AWS IoT device registry to your message.
pipelineActivity_deviceRegistryEnrich :: Lens.Lens' PipelineActivity (Core.Maybe DeviceRegistryEnrichActivity)
pipelineActivity_deviceRegistryEnrich = Lens.lens (\PipelineActivity' {deviceRegistryEnrich} -> deviceRegistryEnrich) (\s@PipelineActivity' {} a -> s {deviceRegistryEnrich = a} :: PipelineActivity)

-- | Determines the source of the messages to be processed.
pipelineActivity_channel :: Lens.Lens' PipelineActivity (Core.Maybe ChannelActivity)
pipelineActivity_channel = Lens.lens (\PipelineActivity' {channel} -> channel) (\s@PipelineActivity' {} a -> s {channel = a} :: PipelineActivity)

-- | Filters a message based on its attributes.
pipelineActivity_filter :: Lens.Lens' PipelineActivity (Core.Maybe FilterActivity)
pipelineActivity_filter = Lens.lens (\PipelineActivity' {filter'} -> filter') (\s@PipelineActivity' {} a -> s {filter' = a} :: PipelineActivity)

-- | Computes an arithmetic expression using the message\'s attributes and
-- adds it to the message.
pipelineActivity_math :: Lens.Lens' PipelineActivity (Core.Maybe MathActivity)
pipelineActivity_math = Lens.lens (\PipelineActivity' {math} -> math) (\s@PipelineActivity' {} a -> s {math = a} :: PipelineActivity)

instance Core.FromJSON PipelineActivity where
  parseJSON =
    Core.withObject
      "PipelineActivity"
      ( \x ->
          PipelineActivity'
            Core.<$> (x Core..:? "selectAttributes")
            Core.<*> (x Core..:? "datastore")
            Core.<*> (x Core..:? "removeAttributes")
            Core.<*> (x Core..:? "addAttributes")
            Core.<*> (x Core..:? "deviceShadowEnrich")
            Core.<*> (x Core..:? "lambda")
            Core.<*> (x Core..:? "deviceRegistryEnrich")
            Core.<*> (x Core..:? "channel")
            Core.<*> (x Core..:? "filter")
            Core.<*> (x Core..:? "math")
      )

instance Core.Hashable PipelineActivity

instance Core.NFData PipelineActivity

instance Core.ToJSON PipelineActivity where
  toJSON PipelineActivity' {..} =
    Core.object
      ( Core.catMaybes
          [ ("selectAttributes" Core..=)
              Core.<$> selectAttributes,
            ("datastore" Core..=) Core.<$> datastore,
            ("removeAttributes" Core..=)
              Core.<$> removeAttributes,
            ("addAttributes" Core..=) Core.<$> addAttributes,
            ("deviceShadowEnrich" Core..=)
              Core.<$> deviceShadowEnrich,
            ("lambda" Core..=) Core.<$> lambda,
            ("deviceRegistryEnrich" Core..=)
              Core.<$> deviceRegistryEnrich,
            ("channel" Core..=) Core.<$> channel,
            ("filter" Core..=) Core.<$> filter',
            ("math" Core..=) Core.<$> math
          ]
      )
