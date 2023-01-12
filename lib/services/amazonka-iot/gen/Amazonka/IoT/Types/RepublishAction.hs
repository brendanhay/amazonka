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
-- Module      : Amazonka.IoT.Types.RepublishAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.RepublishAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.MqttHeaders
import qualified Amazonka.Prelude as Prelude

-- | Describes an action to republish to another topic.
--
-- /See:/ 'newRepublishAction' smart constructor.
data RepublishAction = RepublishAction'
  { -- | MQTT Version 5.0 headers information. For more information, see
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/mqtt.html MQTT>
    -- from the Amazon Web Services IoT Core Developer Guide.
    headers :: Prelude.Maybe MqttHeaders,
    -- | The Quality of Service (QoS) level to use when republishing messages.
    -- The default value is 0.
    qos :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the IAM role that grants access.
    roleArn :: Prelude.Text,
    -- | The name of the MQTT topic.
    topic :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RepublishAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'headers', 'republishAction_headers' - MQTT Version 5.0 headers information. For more information, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/mqtt.html MQTT>
-- from the Amazon Web Services IoT Core Developer Guide.
--
-- 'qos', 'republishAction_qos' - The Quality of Service (QoS) level to use when republishing messages.
-- The default value is 0.
--
-- 'roleArn', 'republishAction_roleArn' - The ARN of the IAM role that grants access.
--
-- 'topic', 'republishAction_topic' - The name of the MQTT topic.
newRepublishAction ::
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'topic'
  Prelude.Text ->
  RepublishAction
newRepublishAction pRoleArn_ pTopic_ =
  RepublishAction'
    { headers = Prelude.Nothing,
      qos = Prelude.Nothing,
      roleArn = pRoleArn_,
      topic = pTopic_
    }

-- | MQTT Version 5.0 headers information. For more information, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/mqtt.html MQTT>
-- from the Amazon Web Services IoT Core Developer Guide.
republishAction_headers :: Lens.Lens' RepublishAction (Prelude.Maybe MqttHeaders)
republishAction_headers = Lens.lens (\RepublishAction' {headers} -> headers) (\s@RepublishAction' {} a -> s {headers = a} :: RepublishAction)

-- | The Quality of Service (QoS) level to use when republishing messages.
-- The default value is 0.
republishAction_qos :: Lens.Lens' RepublishAction (Prelude.Maybe Prelude.Natural)
republishAction_qos = Lens.lens (\RepublishAction' {qos} -> qos) (\s@RepublishAction' {} a -> s {qos = a} :: RepublishAction)

-- | The ARN of the IAM role that grants access.
republishAction_roleArn :: Lens.Lens' RepublishAction Prelude.Text
republishAction_roleArn = Lens.lens (\RepublishAction' {roleArn} -> roleArn) (\s@RepublishAction' {} a -> s {roleArn = a} :: RepublishAction)

-- | The name of the MQTT topic.
republishAction_topic :: Lens.Lens' RepublishAction Prelude.Text
republishAction_topic = Lens.lens (\RepublishAction' {topic} -> topic) (\s@RepublishAction' {} a -> s {topic = a} :: RepublishAction)

instance Data.FromJSON RepublishAction where
  parseJSON =
    Data.withObject
      "RepublishAction"
      ( \x ->
          RepublishAction'
            Prelude.<$> (x Data..:? "headers")
            Prelude.<*> (x Data..:? "qos")
            Prelude.<*> (x Data..: "roleArn")
            Prelude.<*> (x Data..: "topic")
      )

instance Prelude.Hashable RepublishAction where
  hashWithSalt _salt RepublishAction' {..} =
    _salt `Prelude.hashWithSalt` headers
      `Prelude.hashWithSalt` qos
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` topic

instance Prelude.NFData RepublishAction where
  rnf RepublishAction' {..} =
    Prelude.rnf headers
      `Prelude.seq` Prelude.rnf qos
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf topic

instance Data.ToJSON RepublishAction where
  toJSON RepublishAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("headers" Data..=) Prelude.<$> headers,
            ("qos" Data..=) Prelude.<$> qos,
            Prelude.Just ("roleArn" Data..= roleArn),
            Prelude.Just ("topic" Data..= topic)
          ]
      )
