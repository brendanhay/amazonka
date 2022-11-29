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
-- Module      : Amazonka.IoTAnalytics.Types.PipelineActivity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.PipelineActivity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTAnalytics.Types.AddAttributesActivity
import Amazonka.IoTAnalytics.Types.ChannelActivity
import Amazonka.IoTAnalytics.Types.DatastoreActivity
import Amazonka.IoTAnalytics.Types.DeviceRegistryEnrichActivity
import Amazonka.IoTAnalytics.Types.DeviceShadowEnrichActivity
import Amazonka.IoTAnalytics.Types.FilterActivity
import Amazonka.IoTAnalytics.Types.LambdaActivity
import Amazonka.IoTAnalytics.Types.MathActivity
import Amazonka.IoTAnalytics.Types.RemoveAttributesActivity
import Amazonka.IoTAnalytics.Types.SelectAttributesActivity
import qualified Amazonka.Prelude as Prelude

-- | An activity that performs a transformation on a message.
--
-- /See:/ 'newPipelineActivity' smart constructor.
data PipelineActivity = PipelineActivity'
  { -- | Adds data from the IoT device registry to your message.
    deviceRegistryEnrich :: Prelude.Maybe DeviceRegistryEnrichActivity,
    -- | Used to create a new message using only the specified attributes from
    -- the original message.
    selectAttributes :: Prelude.Maybe SelectAttributesActivity,
    -- | Specifies where to store the processed message data.
    datastore :: Prelude.Maybe DatastoreActivity,
    -- | Adds other attributes based on existing attributes in the message.
    addAttributes :: Prelude.Maybe AddAttributesActivity,
    -- | Determines the source of the messages to be processed.
    channel :: Prelude.Maybe ChannelActivity,
    -- | Removes attributes from a message.
    removeAttributes :: Prelude.Maybe RemoveAttributesActivity,
    -- | Adds information from the IoT Device Shadow service to a message.
    deviceShadowEnrich :: Prelude.Maybe DeviceShadowEnrichActivity,
    -- | Filters a message based on its attributes.
    filter' :: Prelude.Maybe FilterActivity,
    -- | Computes an arithmetic expression using the message\'s attributes and
    -- adds it to the message.
    math :: Prelude.Maybe MathActivity,
    -- | Runs a Lambda function to modify the message.
    lambda :: Prelude.Maybe LambdaActivity
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
-- 'deviceRegistryEnrich', 'pipelineActivity_deviceRegistryEnrich' - Adds data from the IoT device registry to your message.
--
-- 'selectAttributes', 'pipelineActivity_selectAttributes' - Used to create a new message using only the specified attributes from
-- the original message.
--
-- 'datastore', 'pipelineActivity_datastore' - Specifies where to store the processed message data.
--
-- 'addAttributes', 'pipelineActivity_addAttributes' - Adds other attributes based on existing attributes in the message.
--
-- 'channel', 'pipelineActivity_channel' - Determines the source of the messages to be processed.
--
-- 'removeAttributes', 'pipelineActivity_removeAttributes' - Removes attributes from a message.
--
-- 'deviceShadowEnrich', 'pipelineActivity_deviceShadowEnrich' - Adds information from the IoT Device Shadow service to a message.
--
-- 'filter'', 'pipelineActivity_filter' - Filters a message based on its attributes.
--
-- 'math', 'pipelineActivity_math' - Computes an arithmetic expression using the message\'s attributes and
-- adds it to the message.
--
-- 'lambda', 'pipelineActivity_lambda' - Runs a Lambda function to modify the message.
newPipelineActivity ::
  PipelineActivity
newPipelineActivity =
  PipelineActivity'
    { deviceRegistryEnrich =
        Prelude.Nothing,
      selectAttributes = Prelude.Nothing,
      datastore = Prelude.Nothing,
      addAttributes = Prelude.Nothing,
      channel = Prelude.Nothing,
      removeAttributes = Prelude.Nothing,
      deviceShadowEnrich = Prelude.Nothing,
      filter' = Prelude.Nothing,
      math = Prelude.Nothing,
      lambda = Prelude.Nothing
    }

-- | Adds data from the IoT device registry to your message.
pipelineActivity_deviceRegistryEnrich :: Lens.Lens' PipelineActivity (Prelude.Maybe DeviceRegistryEnrichActivity)
pipelineActivity_deviceRegistryEnrich = Lens.lens (\PipelineActivity' {deviceRegistryEnrich} -> deviceRegistryEnrich) (\s@PipelineActivity' {} a -> s {deviceRegistryEnrich = a} :: PipelineActivity)

-- | Used to create a new message using only the specified attributes from
-- the original message.
pipelineActivity_selectAttributes :: Lens.Lens' PipelineActivity (Prelude.Maybe SelectAttributesActivity)
pipelineActivity_selectAttributes = Lens.lens (\PipelineActivity' {selectAttributes} -> selectAttributes) (\s@PipelineActivity' {} a -> s {selectAttributes = a} :: PipelineActivity)

-- | Specifies where to store the processed message data.
pipelineActivity_datastore :: Lens.Lens' PipelineActivity (Prelude.Maybe DatastoreActivity)
pipelineActivity_datastore = Lens.lens (\PipelineActivity' {datastore} -> datastore) (\s@PipelineActivity' {} a -> s {datastore = a} :: PipelineActivity)

-- | Adds other attributes based on existing attributes in the message.
pipelineActivity_addAttributes :: Lens.Lens' PipelineActivity (Prelude.Maybe AddAttributesActivity)
pipelineActivity_addAttributes = Lens.lens (\PipelineActivity' {addAttributes} -> addAttributes) (\s@PipelineActivity' {} a -> s {addAttributes = a} :: PipelineActivity)

-- | Determines the source of the messages to be processed.
pipelineActivity_channel :: Lens.Lens' PipelineActivity (Prelude.Maybe ChannelActivity)
pipelineActivity_channel = Lens.lens (\PipelineActivity' {channel} -> channel) (\s@PipelineActivity' {} a -> s {channel = a} :: PipelineActivity)

-- | Removes attributes from a message.
pipelineActivity_removeAttributes :: Lens.Lens' PipelineActivity (Prelude.Maybe RemoveAttributesActivity)
pipelineActivity_removeAttributes = Lens.lens (\PipelineActivity' {removeAttributes} -> removeAttributes) (\s@PipelineActivity' {} a -> s {removeAttributes = a} :: PipelineActivity)

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

-- | Runs a Lambda function to modify the message.
pipelineActivity_lambda :: Lens.Lens' PipelineActivity (Prelude.Maybe LambdaActivity)
pipelineActivity_lambda = Lens.lens (\PipelineActivity' {lambda} -> lambda) (\s@PipelineActivity' {} a -> s {lambda = a} :: PipelineActivity)

instance Core.FromJSON PipelineActivity where
  parseJSON =
    Core.withObject
      "PipelineActivity"
      ( \x ->
          PipelineActivity'
            Prelude.<$> (x Core..:? "deviceRegistryEnrich")
            Prelude.<*> (x Core..:? "selectAttributes")
            Prelude.<*> (x Core..:? "datastore")
            Prelude.<*> (x Core..:? "addAttributes")
            Prelude.<*> (x Core..:? "channel")
            Prelude.<*> (x Core..:? "removeAttributes")
            Prelude.<*> (x Core..:? "deviceShadowEnrich")
            Prelude.<*> (x Core..:? "filter")
            Prelude.<*> (x Core..:? "math")
            Prelude.<*> (x Core..:? "lambda")
      )

instance Prelude.Hashable PipelineActivity where
  hashWithSalt _salt PipelineActivity' {..} =
    _salt `Prelude.hashWithSalt` deviceRegistryEnrich
      `Prelude.hashWithSalt` selectAttributes
      `Prelude.hashWithSalt` datastore
      `Prelude.hashWithSalt` addAttributes
      `Prelude.hashWithSalt` channel
      `Prelude.hashWithSalt` removeAttributes
      `Prelude.hashWithSalt` deviceShadowEnrich
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` math
      `Prelude.hashWithSalt` lambda

instance Prelude.NFData PipelineActivity where
  rnf PipelineActivity' {..} =
    Prelude.rnf deviceRegistryEnrich
      `Prelude.seq` Prelude.rnf selectAttributes
      `Prelude.seq` Prelude.rnf datastore
      `Prelude.seq` Prelude.rnf addAttributes
      `Prelude.seq` Prelude.rnf channel
      `Prelude.seq` Prelude.rnf removeAttributes
      `Prelude.seq` Prelude.rnf deviceShadowEnrich
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf math
      `Prelude.seq` Prelude.rnf lambda

instance Core.ToJSON PipelineActivity where
  toJSON PipelineActivity' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("deviceRegistryEnrich" Core..=)
              Prelude.<$> deviceRegistryEnrich,
            ("selectAttributes" Core..=)
              Prelude.<$> selectAttributes,
            ("datastore" Core..=) Prelude.<$> datastore,
            ("addAttributes" Core..=) Prelude.<$> addAttributes,
            ("channel" Core..=) Prelude.<$> channel,
            ("removeAttributes" Core..=)
              Prelude.<$> removeAttributes,
            ("deviceShadowEnrich" Core..=)
              Prelude.<$> deviceShadowEnrich,
            ("filter" Core..=) Prelude.<$> filter',
            ("math" Core..=) Prelude.<$> math,
            ("lambda" Core..=) Prelude.<$> lambda
          ]
      )
