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
import qualified Network.AWS.Prelude as Prelude

-- | An activity that performs a transformation on a message.
--
-- /See:/ 'newPipelineActivity' smart constructor.
data PipelineActivity = PipelineActivity'
  { -- | Used to create a new message using only the specified attributes from
    -- the original message.
    selectAttributes :: Prelude.Maybe SelectAttributesActivity,
    -- | Determines the source of the messages to be processed.
    channel :: Prelude.Maybe ChannelActivity,
    -- | Adds other attributes based on existing attributes in the message.
    addAttributes :: Prelude.Maybe AddAttributesActivity,
    -- | Adds data from the IoT device registry to your message.
    deviceRegistryEnrich :: Prelude.Maybe DeviceRegistryEnrichActivity,
    -- | Removes attributes from a message.
    removeAttributes :: Prelude.Maybe RemoveAttributesActivity,
    -- | Runs a Lambda function to modify the message.
    lambda :: Prelude.Maybe LambdaActivity,
    -- | Specifies where to store the processed message data.
    datastore :: Prelude.Maybe DatastoreActivity,
    -- | Adds information from the IoT Device Shadow service to a message.
    deviceShadowEnrich :: Prelude.Maybe DeviceShadowEnrichActivity,
    -- | Filters a message based on its attributes.
    filter' :: Prelude.Maybe FilterActivity,
    -- | Computes an arithmetic expression using the message\'s attributes and
    -- adds it to the message.
    math :: Prelude.Maybe MathActivity
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipelineActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'selectAttributes', 'pipelineActivity_selectAttributes' - Used to create a new message using only the specified attributes from
-- the original message.
--
-- 'channel', 'pipelineActivity_channel' - Determines the source of the messages to be processed.
--
-- 'addAttributes', 'pipelineActivity_addAttributes' - Adds other attributes based on existing attributes in the message.
--
-- 'deviceRegistryEnrich', 'pipelineActivity_deviceRegistryEnrich' - Adds data from the IoT device registry to your message.
--
-- 'removeAttributes', 'pipelineActivity_removeAttributes' - Removes attributes from a message.
--
-- 'lambda', 'pipelineActivity_lambda' - Runs a Lambda function to modify the message.
--
-- 'datastore', 'pipelineActivity_datastore' - Specifies where to store the processed message data.
--
-- 'deviceShadowEnrich', 'pipelineActivity_deviceShadowEnrich' - Adds information from the IoT Device Shadow service to a message.
--
-- 'filter'', 'pipelineActivity_filter' - Filters a message based on its attributes.
--
-- 'math', 'pipelineActivity_math' - Computes an arithmetic expression using the message\'s attributes and
-- adds it to the message.
newPipelineActivity ::
  PipelineActivity
newPipelineActivity =
  PipelineActivity'
    { selectAttributes =
        Prelude.Nothing,
      channel = Prelude.Nothing,
      addAttributes = Prelude.Nothing,
      deviceRegistryEnrich = Prelude.Nothing,
      removeAttributes = Prelude.Nothing,
      lambda = Prelude.Nothing,
      datastore = Prelude.Nothing,
      deviceShadowEnrich = Prelude.Nothing,
      filter' = Prelude.Nothing,
      math = Prelude.Nothing
    }

-- | Used to create a new message using only the specified attributes from
-- the original message.
pipelineActivity_selectAttributes :: Lens.Lens' PipelineActivity (Prelude.Maybe SelectAttributesActivity)
pipelineActivity_selectAttributes = Lens.lens (\PipelineActivity' {selectAttributes} -> selectAttributes) (\s@PipelineActivity' {} a -> s {selectAttributes = a} :: PipelineActivity)

-- | Determines the source of the messages to be processed.
pipelineActivity_channel :: Lens.Lens' PipelineActivity (Prelude.Maybe ChannelActivity)
pipelineActivity_channel = Lens.lens (\PipelineActivity' {channel} -> channel) (\s@PipelineActivity' {} a -> s {channel = a} :: PipelineActivity)

-- | Adds other attributes based on existing attributes in the message.
pipelineActivity_addAttributes :: Lens.Lens' PipelineActivity (Prelude.Maybe AddAttributesActivity)
pipelineActivity_addAttributes = Lens.lens (\PipelineActivity' {addAttributes} -> addAttributes) (\s@PipelineActivity' {} a -> s {addAttributes = a} :: PipelineActivity)

-- | Adds data from the IoT device registry to your message.
pipelineActivity_deviceRegistryEnrich :: Lens.Lens' PipelineActivity (Prelude.Maybe DeviceRegistryEnrichActivity)
pipelineActivity_deviceRegistryEnrich = Lens.lens (\PipelineActivity' {deviceRegistryEnrich} -> deviceRegistryEnrich) (\s@PipelineActivity' {} a -> s {deviceRegistryEnrich = a} :: PipelineActivity)

-- | Removes attributes from a message.
pipelineActivity_removeAttributes :: Lens.Lens' PipelineActivity (Prelude.Maybe RemoveAttributesActivity)
pipelineActivity_removeAttributes = Lens.lens (\PipelineActivity' {removeAttributes} -> removeAttributes) (\s@PipelineActivity' {} a -> s {removeAttributes = a} :: PipelineActivity)

-- | Runs a Lambda function to modify the message.
pipelineActivity_lambda :: Lens.Lens' PipelineActivity (Prelude.Maybe LambdaActivity)
pipelineActivity_lambda = Lens.lens (\PipelineActivity' {lambda} -> lambda) (\s@PipelineActivity' {} a -> s {lambda = a} :: PipelineActivity)

-- | Specifies where to store the processed message data.
pipelineActivity_datastore :: Lens.Lens' PipelineActivity (Prelude.Maybe DatastoreActivity)
pipelineActivity_datastore = Lens.lens (\PipelineActivity' {datastore} -> datastore) (\s@PipelineActivity' {} a -> s {datastore = a} :: PipelineActivity)

-- | Adds information from the IoT Device Shadow service to a message.
pipelineActivity_deviceShadowEnrich :: Lens.Lens' PipelineActivity (Prelude.Maybe DeviceShadowEnrichActivity)
pipelineActivity_deviceShadowEnrich = Lens.lens (\PipelineActivity' {deviceShadowEnrich} -> deviceShadowEnrich) (\s@PipelineActivity' {} a -> s {deviceShadowEnrich = a} :: PipelineActivity)

-- | Filters a message based on its attributes.
pipelineActivity_filter :: Lens.Lens' PipelineActivity (Prelude.Maybe FilterActivity)
pipelineActivity_filter = Lens.lens (\PipelineActivity' {filter'} -> filter') (\s@PipelineActivity' {} a -> s {filter' = a} :: PipelineActivity)

-- | Computes an arithmetic expression using the message\'s attributes and
-- adds it to the message.
pipelineActivity_math :: Lens.Lens' PipelineActivity (Prelude.Maybe MathActivity)
pipelineActivity_math = Lens.lens (\PipelineActivity' {math} -> math) (\s@PipelineActivity' {} a -> s {math = a} :: PipelineActivity)

instance Core.FromJSON PipelineActivity where
  parseJSON =
    Core.withObject
      "PipelineActivity"
      ( \x ->
          PipelineActivity'
            Prelude.<$> (x Core..:? "selectAttributes")
            Prelude.<*> (x Core..:? "channel")
            Prelude.<*> (x Core..:? "addAttributes")
            Prelude.<*> (x Core..:? "deviceRegistryEnrich")
            Prelude.<*> (x Core..:? "removeAttributes")
            Prelude.<*> (x Core..:? "lambda")
            Prelude.<*> (x Core..:? "datastore")
            Prelude.<*> (x Core..:? "deviceShadowEnrich")
            Prelude.<*> (x Core..:? "filter")
            Prelude.<*> (x Core..:? "math")
      )

instance Prelude.Hashable PipelineActivity

instance Prelude.NFData PipelineActivity

instance Core.ToJSON PipelineActivity where
  toJSON PipelineActivity' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("selectAttributes" Core..=)
              Prelude.<$> selectAttributes,
            ("channel" Core..=) Prelude.<$> channel,
            ("addAttributes" Core..=) Prelude.<$> addAttributes,
            ("deviceRegistryEnrich" Core..=)
              Prelude.<$> deviceRegistryEnrich,
            ("removeAttributes" Core..=)
              Prelude.<$> removeAttributes,
            ("lambda" Core..=) Prelude.<$> lambda,
            ("datastore" Core..=) Prelude.<$> datastore,
            ("deviceShadowEnrich" Core..=)
              Prelude.<$> deviceShadowEnrich,
            ("filter" Core..=) Prelude.<$> filter',
            ("math" Core..=) Prelude.<$> math
          ]
      )
