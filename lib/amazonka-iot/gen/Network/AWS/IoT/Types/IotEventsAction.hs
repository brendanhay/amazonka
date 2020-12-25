{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.IotEventsAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.IotEventsAction
  ( IotEventsAction (..),

    -- * Smart constructor
    mkIotEventsAction,

    -- * Lenses
    ieaInputName,
    ieaRoleArn,
    ieaBatchMode,
    ieaMessageId,
  )
where

import qualified Network.AWS.IoT.Types.AwsArn as Types
import qualified Network.AWS.IoT.Types.InputName as Types
import qualified Network.AWS.IoT.Types.MessageId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Sends an input to an AWS IoT Events detector.
--
-- /See:/ 'mkIotEventsAction' smart constructor.
data IotEventsAction = IotEventsAction'
  { -- | The name of the AWS IoT Events input.
    inputName :: Types.InputName,
    -- | The ARN of the role that grants AWS IoT permission to send an input to an AWS IoT Events detector. ("Action":"iotevents:BatchPutMessage").
    roleArn :: Types.AwsArn,
    -- | Whether to process the event actions as a batch. The default value is @false@ .
    --
    -- When @batchMode@ is @true@ , you can't specify a @messageId@ .
    -- When @batchMode@ is @true@ and the rule SQL statement evaluates to an Array, each Array element is treated as a separate message when it's sent to AWS IoT Events by calling <https://docs.aws.amazon.com/iotevents/latest/apireference/API_iotevents-data_BatchPutMessage.html @BatchPutMessage@ > . The resulting array can't have more than 10 messages.
    batchMode :: Core.Maybe Core.Bool,
    -- | The ID of the message. The default @messageId@ is a new UUID value.
    --
    -- When @batchMode@ is @true@ , you can't specify a @messageId@ --a new UUID value will be assigned.
    -- Assign a value to this property to ensure that only one input (message) with a given @messageId@ will be processed by an AWS IoT Events detector.
    messageId :: Core.Maybe Types.MessageId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IotEventsAction' value with any optional fields omitted.
mkIotEventsAction ::
  -- | 'inputName'
  Types.InputName ->
  -- | 'roleArn'
  Types.AwsArn ->
  IotEventsAction
mkIotEventsAction inputName roleArn =
  IotEventsAction'
    { inputName,
      roleArn,
      batchMode = Core.Nothing,
      messageId = Core.Nothing
    }

-- | The name of the AWS IoT Events input.
--
-- /Note:/ Consider using 'inputName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieaInputName :: Lens.Lens' IotEventsAction Types.InputName
ieaInputName = Lens.field @"inputName"
{-# DEPRECATED ieaInputName "Use generic-lens or generic-optics with 'inputName' instead." #-}

-- | The ARN of the role that grants AWS IoT permission to send an input to an AWS IoT Events detector. ("Action":"iotevents:BatchPutMessage").
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieaRoleArn :: Lens.Lens' IotEventsAction Types.AwsArn
ieaRoleArn = Lens.field @"roleArn"
{-# DEPRECATED ieaRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | Whether to process the event actions as a batch. The default value is @false@ .
--
-- When @batchMode@ is @true@ , you can't specify a @messageId@ .
-- When @batchMode@ is @true@ and the rule SQL statement evaluates to an Array, each Array element is treated as a separate message when it's sent to AWS IoT Events by calling <https://docs.aws.amazon.com/iotevents/latest/apireference/API_iotevents-data_BatchPutMessage.html @BatchPutMessage@ > . The resulting array can't have more than 10 messages.
--
-- /Note:/ Consider using 'batchMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieaBatchMode :: Lens.Lens' IotEventsAction (Core.Maybe Core.Bool)
ieaBatchMode = Lens.field @"batchMode"
{-# DEPRECATED ieaBatchMode "Use generic-lens or generic-optics with 'batchMode' instead." #-}

-- | The ID of the message. The default @messageId@ is a new UUID value.
--
-- When @batchMode@ is @true@ , you can't specify a @messageId@ --a new UUID value will be assigned.
-- Assign a value to this property to ensure that only one input (message) with a given @messageId@ will be processed by an AWS IoT Events detector.
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieaMessageId :: Lens.Lens' IotEventsAction (Core.Maybe Types.MessageId)
ieaMessageId = Lens.field @"messageId"
{-# DEPRECATED ieaMessageId "Use generic-lens or generic-optics with 'messageId' instead." #-}

instance Core.FromJSON IotEventsAction where
  toJSON IotEventsAction {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("inputName" Core..= inputName),
            Core.Just ("roleArn" Core..= roleArn),
            ("batchMode" Core..=) Core.<$> batchMode,
            ("messageId" Core..=) Core.<$> messageId
          ]
      )

instance Core.FromJSON IotEventsAction where
  parseJSON =
    Core.withObject "IotEventsAction" Core.$
      \x ->
        IotEventsAction'
          Core.<$> (x Core..: "inputName")
          Core.<*> (x Core..: "roleArn")
          Core.<*> (x Core..:? "batchMode")
          Core.<*> (x Core..:? "messageId")
