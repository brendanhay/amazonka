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
import qualified Amazonka.Data as Data
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
  { -- | Adds other attributes based on existing attributes in the message.
    addAttributes :: Prelude.Maybe AddAttributesActivity,
    -- | Determines the source of the messages to be processed.
    channel :: Prelude.Maybe ChannelActivity,
    -- | Specifies where to store the processed message data.
    datastore :: Prelude.Maybe DatastoreActivity,
    -- | Adds data from the IoT device registry to your message.
    deviceRegistryEnrich :: Prelude.Maybe DeviceRegistryEnrichActivity,
    -- | Adds information from the IoT Device Shadow service to a message.
    deviceShadowEnrich :: Prelude.Maybe DeviceShadowEnrichActivity,
    -- | Filters a message based on its attributes.
    filter' :: Prelude.Maybe FilterActivity,
    -- | Runs a Lambda function to modify the message.
    lambda :: Prelude.Maybe LambdaActivity,
    -- | Computes an arithmetic expression using the message\'s attributes and
    -- adds it to the message.
    math :: Prelude.Maybe MathActivity,
    -- | Removes attributes from a message.
    removeAttributes :: Prelude.Maybe RemoveAttributesActivity,
    -- | Used to create a new message using only the specified attributes from
    -- the original message.
    selectAttributes :: Prelude.Maybe SelectAttributesActivity
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
-- 'addAttributes', 'pipelineActivity_addAttributes' - Adds other attributes based on existing attributes in the message.
--
-- 'channel', 'pipelineActivity_channel' - Determines the source of the messages to be processed.
--
-- 'datastore', 'pipelineActivity_datastore' - Specifies where to store the processed message data.
--
-- 'deviceRegistryEnrich', 'pipelineActivity_deviceRegistryEnrich' - Adds data from the IoT device registry to your message.
--
-- 'deviceShadowEnrich', 'pipelineActivity_deviceShadowEnrich' - Adds information from the IoT Device Shadow service to a message.
--
-- 'filter'', 'pipelineActivity_filter' - Filters a message based on its attributes.
--
-- 'lambda', 'pipelineActivity_lambda' - Runs a Lambda function to modify the message.
--
-- 'math', 'pipelineActivity_math' - Computes an arithmetic expression using the message\'s attributes and
-- adds it to the message.
--
-- 'removeAttributes', 'pipelineActivity_removeAttributes' - Removes attributes from a message.
--
-- 'selectAttributes', 'pipelineActivity_selectAttributes' - Used to create a new message using only the specified attributes from
-- the original message.
newPipelineActivity ::
  PipelineActivity
newPipelineActivity =
  PipelineActivity'
    { addAttributes = Prelude.Nothing,
      channel = Prelude.Nothing,
      datastore = Prelude.Nothing,
      deviceRegistryEnrich = Prelude.Nothing,
      deviceShadowEnrich = Prelude.Nothing,
      filter' = Prelude.Nothing,
      lambda = Prelude.Nothing,
      math = Prelude.Nothing,
      removeAttributes = Prelude.Nothing,
      selectAttributes = Prelude.Nothing
    }

-- | Adds other attributes based on existing attributes in the message.
pipelineActivity_addAttributes :: Lens.Lens' PipelineActivity (Prelude.Maybe AddAttributesActivity)
pipelineActivity_addAttributes = Lens.lens (\PipelineActivity' {addAttributes} -> addAttributes) (\s@PipelineActivity' {} a -> s {addAttributes = a} :: PipelineActivity)

-- | Determines the source of the messages to be processed.
pipelineActivity_channel :: Lens.Lens' PipelineActivity (Prelude.Maybe ChannelActivity)
pipelineActivity_channel = Lens.lens (\PipelineActivity' {channel} -> channel) (\s@PipelineActivity' {} a -> s {channel = a} :: PipelineActivity)

-- | Specifies where to store the processed message data.
pipelineActivity_datastore :: Lens.Lens' PipelineActivity (Prelude.Maybe DatastoreActivity)
pipelineActivity_datastore = Lens.lens (\PipelineActivity' {datastore} -> datastore) (\s@PipelineActivity' {} a -> s {datastore = a} :: PipelineActivity)

-- | Adds data from the IoT device registry to your message.
pipelineActivity_deviceRegistryEnrich :: Lens.Lens' PipelineActivity (Prelude.Maybe DeviceRegistryEnrichActivity)
pipelineActivity_deviceRegistryEnrich = Lens.lens (\PipelineActivity' {deviceRegistryEnrich} -> deviceRegistryEnrich) (\s@PipelineActivity' {} a -> s {deviceRegistryEnrich = a} :: PipelineActivity)

-- | Adds information from the IoT Device Shadow service to a message.
pipelineActivity_deviceShadowEnrich :: Lens.Lens' PipelineActivity (Prelude.Maybe DeviceShadowEnrichActivity)
pipelineActivity_deviceShadowEnrich = Lens.lens (\PipelineActivity' {deviceShadowEnrich} -> deviceShadowEnrich) (\s@PipelineActivity' {} a -> s {deviceShadowEnrich = a} :: PipelineActivity)

-- | Filters a message based on its attributes.
pipelineActivity_filter :: Lens.Lens' PipelineActivity (Prelude.Maybe FilterActivity)
pipelineActivity_filter = Lens.lens (\PipelineActivity' {filter'} -> filter') (\s@PipelineActivity' {} a -> s {filter' = a} :: PipelineActivity)

-- | Runs a Lambda function to modify the message.
pipelineActivity_lambda :: Lens.Lens' PipelineActivity (Prelude.Maybe LambdaActivity)
pipelineActivity_lambda = Lens.lens (\PipelineActivity' {lambda} -> lambda) (\s@PipelineActivity' {} a -> s {lambda = a} :: PipelineActivity)

-- | Computes an arithmetic expression using the message\'s attributes and
-- adds it to the message.
pipelineActivity_math :: Lens.Lens' PipelineActivity (Prelude.Maybe MathActivity)
pipelineActivity_math = Lens.lens (\PipelineActivity' {math} -> math) (\s@PipelineActivity' {} a -> s {math = a} :: PipelineActivity)

-- | Removes attributes from a message.
pipelineActivity_removeAttributes :: Lens.Lens' PipelineActivity (Prelude.Maybe RemoveAttributesActivity)
pipelineActivity_removeAttributes = Lens.lens (\PipelineActivity' {removeAttributes} -> removeAttributes) (\s@PipelineActivity' {} a -> s {removeAttributes = a} :: PipelineActivity)

-- | Used to create a new message using only the specified attributes from
-- the original message.
pipelineActivity_selectAttributes :: Lens.Lens' PipelineActivity (Prelude.Maybe SelectAttributesActivity)
pipelineActivity_selectAttributes = Lens.lens (\PipelineActivity' {selectAttributes} -> selectAttributes) (\s@PipelineActivity' {} a -> s {selectAttributes = a} :: PipelineActivity)

instance Data.FromJSON PipelineActivity where
  parseJSON =
    Data.withObject
      "PipelineActivity"
      ( \x ->
          PipelineActivity'
            Prelude.<$> (x Data..:? "addAttributes")
            Prelude.<*> (x Data..:? "channel")
            Prelude.<*> (x Data..:? "datastore")
            Prelude.<*> (x Data..:? "deviceRegistryEnrich")
            Prelude.<*> (x Data..:? "deviceShadowEnrich")
            Prelude.<*> (x Data..:? "filter")
            Prelude.<*> (x Data..:? "lambda")
            Prelude.<*> (x Data..:? "math")
            Prelude.<*> (x Data..:? "removeAttributes")
            Prelude.<*> (x Data..:? "selectAttributes")
      )

instance Prelude.Hashable PipelineActivity where
  hashWithSalt _salt PipelineActivity' {..} =
    _salt `Prelude.hashWithSalt` addAttributes
      `Prelude.hashWithSalt` channel
      `Prelude.hashWithSalt` datastore
      `Prelude.hashWithSalt` deviceRegistryEnrich
      `Prelude.hashWithSalt` deviceShadowEnrich
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` lambda
      `Prelude.hashWithSalt` math
      `Prelude.hashWithSalt` removeAttributes
      `Prelude.hashWithSalt` selectAttributes

instance Prelude.NFData PipelineActivity where
  rnf PipelineActivity' {..} =
    Prelude.rnf addAttributes
      `Prelude.seq` Prelude.rnf channel
      `Prelude.seq` Prelude.rnf datastore
      `Prelude.seq` Prelude.rnf deviceRegistryEnrich
      `Prelude.seq` Prelude.rnf deviceShadowEnrich
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf lambda
      `Prelude.seq` Prelude.rnf math
      `Prelude.seq` Prelude.rnf removeAttributes
      `Prelude.seq` Prelude.rnf selectAttributes

instance Data.ToJSON PipelineActivity where
  toJSON PipelineActivity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("addAttributes" Data..=) Prelude.<$> addAttributes,
            ("channel" Data..=) Prelude.<$> channel,
            ("datastore" Data..=) Prelude.<$> datastore,
            ("deviceRegistryEnrich" Data..=)
              Prelude.<$> deviceRegistryEnrich,
            ("deviceShadowEnrich" Data..=)
              Prelude.<$> deviceShadowEnrich,
            ("filter" Data..=) Prelude.<$> filter',
            ("lambda" Data..=) Prelude.<$> lambda,
            ("math" Data..=) Prelude.<$> math,
            ("removeAttributes" Data..=)
              Prelude.<$> removeAttributes,
            ("selectAttributes" Data..=)
              Prelude.<$> selectAttributes
          ]
      )
