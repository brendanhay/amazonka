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
-- Module      : Network.AWS.IoT.Types.RepublishAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.RepublishAction where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes an action to republish to another topic.
--
-- /See:/ 'newRepublishAction' smart constructor.
data RepublishAction = RepublishAction'
  { -- | The Quality of Service (QoS) level to use when republishing messages.
    -- The default value is 0.
    qos :: Core.Maybe Core.Natural,
    -- | The ARN of the IAM role that grants access.
    roleArn :: Core.Text,
    -- | The name of the MQTT topic.
    topic :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RepublishAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qos', 'republishAction_qos' - The Quality of Service (QoS) level to use when republishing messages.
-- The default value is 0.
--
-- 'roleArn', 'republishAction_roleArn' - The ARN of the IAM role that grants access.
--
-- 'topic', 'republishAction_topic' - The name of the MQTT topic.
newRepublishAction ::
  -- | 'roleArn'
  Core.Text ->
  -- | 'topic'
  Core.Text ->
  RepublishAction
newRepublishAction pRoleArn_ pTopic_ =
  RepublishAction'
    { qos = Core.Nothing,
      roleArn = pRoleArn_,
      topic = pTopic_
    }

-- | The Quality of Service (QoS) level to use when republishing messages.
-- The default value is 0.
republishAction_qos :: Lens.Lens' RepublishAction (Core.Maybe Core.Natural)
republishAction_qos = Lens.lens (\RepublishAction' {qos} -> qos) (\s@RepublishAction' {} a -> s {qos = a} :: RepublishAction)

-- | The ARN of the IAM role that grants access.
republishAction_roleArn :: Lens.Lens' RepublishAction Core.Text
republishAction_roleArn = Lens.lens (\RepublishAction' {roleArn} -> roleArn) (\s@RepublishAction' {} a -> s {roleArn = a} :: RepublishAction)

-- | The name of the MQTT topic.
republishAction_topic :: Lens.Lens' RepublishAction Core.Text
republishAction_topic = Lens.lens (\RepublishAction' {topic} -> topic) (\s@RepublishAction' {} a -> s {topic = a} :: RepublishAction)

instance Core.FromJSON RepublishAction where
  parseJSON =
    Core.withObject
      "RepublishAction"
      ( \x ->
          RepublishAction'
            Core.<$> (x Core..:? "qos")
            Core.<*> (x Core..: "roleArn")
            Core.<*> (x Core..: "topic")
      )

instance Core.Hashable RepublishAction

instance Core.NFData RepublishAction

instance Core.ToJSON RepublishAction where
  toJSON RepublishAction' {..} =
    Core.object
      ( Core.catMaybes
          [ ("qos" Core..=) Core.<$> qos,
            Core.Just ("roleArn" Core..= roleArn),
            Core.Just ("topic" Core..= topic)
          ]
      )
