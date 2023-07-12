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
-- Module      : Amazonka.GreengrassV2.Types.LambdaEventSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.LambdaEventSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types.LambdaEventSourceType
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an event source for an Lambda function. The
-- event source defines the topics on which this Lambda function subscribes
-- to receive messages that run the function.
--
-- /See:/ 'newLambdaEventSource' smart constructor.
data LambdaEventSource = LambdaEventSource'
  { -- | The topic to which to subscribe to receive event messages.
    topic :: Prelude.Text,
    -- | The type of event source. Choose from the following options:
    --
    -- -   @PUB_SUB@ – Subscribe to local publish\/subscribe messages. This
    --     event source type doesn\'t support MQTT wildcards (@+@ and @#@) in
    --     the event source topic.
    --
    -- -   @IOT_CORE@ – Subscribe to Amazon Web Services IoT Core MQTT
    --     messages. This event source type supports MQTT wildcards (@+@ and
    --     @#@) in the event source topic.
    type' :: LambdaEventSourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaEventSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topic', 'lambdaEventSource_topic' - The topic to which to subscribe to receive event messages.
--
-- 'type'', 'lambdaEventSource_type' - The type of event source. Choose from the following options:
--
-- -   @PUB_SUB@ – Subscribe to local publish\/subscribe messages. This
--     event source type doesn\'t support MQTT wildcards (@+@ and @#@) in
--     the event source topic.
--
-- -   @IOT_CORE@ – Subscribe to Amazon Web Services IoT Core MQTT
--     messages. This event source type supports MQTT wildcards (@+@ and
--     @#@) in the event source topic.
newLambdaEventSource ::
  -- | 'topic'
  Prelude.Text ->
  -- | 'type''
  LambdaEventSourceType ->
  LambdaEventSource
newLambdaEventSource pTopic_ pType_ =
  LambdaEventSource' {topic = pTopic_, type' = pType_}

-- | The topic to which to subscribe to receive event messages.
lambdaEventSource_topic :: Lens.Lens' LambdaEventSource Prelude.Text
lambdaEventSource_topic = Lens.lens (\LambdaEventSource' {topic} -> topic) (\s@LambdaEventSource' {} a -> s {topic = a} :: LambdaEventSource)

-- | The type of event source. Choose from the following options:
--
-- -   @PUB_SUB@ – Subscribe to local publish\/subscribe messages. This
--     event source type doesn\'t support MQTT wildcards (@+@ and @#@) in
--     the event source topic.
--
-- -   @IOT_CORE@ – Subscribe to Amazon Web Services IoT Core MQTT
--     messages. This event source type supports MQTT wildcards (@+@ and
--     @#@) in the event source topic.
lambdaEventSource_type :: Lens.Lens' LambdaEventSource LambdaEventSourceType
lambdaEventSource_type = Lens.lens (\LambdaEventSource' {type'} -> type') (\s@LambdaEventSource' {} a -> s {type' = a} :: LambdaEventSource)

instance Prelude.Hashable LambdaEventSource where
  hashWithSalt _salt LambdaEventSource' {..} =
    _salt
      `Prelude.hashWithSalt` topic
      `Prelude.hashWithSalt` type'

instance Prelude.NFData LambdaEventSource where
  rnf LambdaEventSource' {..} =
    Prelude.rnf topic `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON LambdaEventSource where
  toJSON LambdaEventSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("topic" Data..= topic),
            Prelude.Just ("type" Data..= type')
          ]
      )
