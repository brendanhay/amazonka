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
    ieaBatchMode,
    ieaMessageId,
    ieaInputName,
    ieaRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Sends an input to an AWS IoT Events detector.
--
-- /See:/ 'mkIotEventsAction' smart constructor.
data IotEventsAction = IotEventsAction'
  { batchMode ::
      Lude.Maybe Lude.Bool,
    messageId :: Lude.Maybe Lude.Text,
    inputName :: Lude.Text,
    roleARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IotEventsAction' with the minimum fields required to make a request.
--
-- * 'batchMode' - Whether to process the event actions as a batch. The default value is @false@ .
--
-- When @batchMode@ is @true@ , you can't specify a @messageId@ .
-- When @batchMode@ is @true@ and the rule SQL statement evaluates to an Array, each Array element is treated as a separate message when it's sent to AWS IoT Events by calling <https://docs.aws.amazon.com/iotevents/latest/apireference/API_iotevents-data_BatchPutMessage.html @BatchPutMessage@ > . The resulting array can't have more than 10 messages.
-- * 'inputName' - The name of the AWS IoT Events input.
-- * 'messageId' - The ID of the message. The default @messageId@ is a new UUID value.
--
-- When @batchMode@ is @true@ , you can't specify a @messageId@ --a new UUID value will be assigned.
-- Assign a value to this property to ensure that only one input (message) with a given @messageId@ will be processed by an AWS IoT Events detector.
-- * 'roleARN' - The ARN of the role that grants AWS IoT permission to send an input to an AWS IoT Events detector. ("Action":"iotevents:BatchPutMessage").
mkIotEventsAction ::
  -- | 'inputName'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  IotEventsAction
mkIotEventsAction pInputName_ pRoleARN_ =
  IotEventsAction'
    { batchMode = Lude.Nothing,
      messageId = Lude.Nothing,
      inputName = pInputName_,
      roleARN = pRoleARN_
    }

-- | Whether to process the event actions as a batch. The default value is @false@ .
--
-- When @batchMode@ is @true@ , you can't specify a @messageId@ .
-- When @batchMode@ is @true@ and the rule SQL statement evaluates to an Array, each Array element is treated as a separate message when it's sent to AWS IoT Events by calling <https://docs.aws.amazon.com/iotevents/latest/apireference/API_iotevents-data_BatchPutMessage.html @BatchPutMessage@ > . The resulting array can't have more than 10 messages.
--
-- /Note:/ Consider using 'batchMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieaBatchMode :: Lens.Lens' IotEventsAction (Lude.Maybe Lude.Bool)
ieaBatchMode = Lens.lens (batchMode :: IotEventsAction -> Lude.Maybe Lude.Bool) (\s a -> s {batchMode = a} :: IotEventsAction)
{-# DEPRECATED ieaBatchMode "Use generic-lens or generic-optics with 'batchMode' instead." #-}

-- | The ID of the message. The default @messageId@ is a new UUID value.
--
-- When @batchMode@ is @true@ , you can't specify a @messageId@ --a new UUID value will be assigned.
-- Assign a value to this property to ensure that only one input (message) with a given @messageId@ will be processed by an AWS IoT Events detector.
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieaMessageId :: Lens.Lens' IotEventsAction (Lude.Maybe Lude.Text)
ieaMessageId = Lens.lens (messageId :: IotEventsAction -> Lude.Maybe Lude.Text) (\s a -> s {messageId = a} :: IotEventsAction)
{-# DEPRECATED ieaMessageId "Use generic-lens or generic-optics with 'messageId' instead." #-}

-- | The name of the AWS IoT Events input.
--
-- /Note:/ Consider using 'inputName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieaInputName :: Lens.Lens' IotEventsAction Lude.Text
ieaInputName = Lens.lens (inputName :: IotEventsAction -> Lude.Text) (\s a -> s {inputName = a} :: IotEventsAction)
{-# DEPRECATED ieaInputName "Use generic-lens or generic-optics with 'inputName' instead." #-}

-- | The ARN of the role that grants AWS IoT permission to send an input to an AWS IoT Events detector. ("Action":"iotevents:BatchPutMessage").
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieaRoleARN :: Lens.Lens' IotEventsAction Lude.Text
ieaRoleARN = Lens.lens (roleARN :: IotEventsAction -> Lude.Text) (\s a -> s {roleARN = a} :: IotEventsAction)
{-# DEPRECATED ieaRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON IotEventsAction where
  parseJSON =
    Lude.withObject
      "IotEventsAction"
      ( \x ->
          IotEventsAction'
            Lude.<$> (x Lude..:? "batchMode")
            Lude.<*> (x Lude..:? "messageId")
            Lude.<*> (x Lude..: "inputName")
            Lude.<*> (x Lude..: "roleArn")
      )

instance Lude.ToJSON IotEventsAction where
  toJSON IotEventsAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("batchMode" Lude..=) Lude.<$> batchMode,
            ("messageId" Lude..=) Lude.<$> messageId,
            Lude.Just ("inputName" Lude..= inputName),
            Lude.Just ("roleArn" Lude..= roleARN)
          ]
      )
