{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.IotAnalyticsAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.IotAnalyticsAction
  ( IotAnalyticsAction (..),

    -- * Smart constructor
    mkIotAnalyticsAction,

    -- * Lenses
    iaaBatchMode,
    iaaChannelARN,
    iaaChannelName,
    iaaRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Sends message data to an AWS IoT Analytics channel.
--
-- /See:/ 'mkIotAnalyticsAction' smart constructor.
data IotAnalyticsAction = IotAnalyticsAction'
  { -- | Whether to process the action as a batch. The default value is @false@ .
    --
    -- When @batchMode@ is @true@ and the rule SQL statement evaluates to an Array, each Array element is delivered as a separate message when passed by <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_BatchPutMessage.html @BatchPutMessage@ > to the AWS IoT Analytics channel. The resulting array can't have more than 100 messages.
    batchMode :: Lude.Maybe Lude.Bool,
    -- | (deprecated) The ARN of the IoT Analytics channel to which message data will be sent.
    channelARN :: Lude.Maybe Lude.Text,
    -- | The name of the IoT Analytics channel to which message data will be sent.
    channelName :: Lude.Maybe Lude.Text,
    -- | The ARN of the role which has a policy that grants IoT Analytics permission to send message data via IoT Analytics (iotanalytics:BatchPutMessage).
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IotAnalyticsAction' with the minimum fields required to make a request.
--
-- * 'batchMode' - Whether to process the action as a batch. The default value is @false@ .
--
-- When @batchMode@ is @true@ and the rule SQL statement evaluates to an Array, each Array element is delivered as a separate message when passed by <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_BatchPutMessage.html @BatchPutMessage@ > to the AWS IoT Analytics channel. The resulting array can't have more than 100 messages.
-- * 'channelARN' - (deprecated) The ARN of the IoT Analytics channel to which message data will be sent.
-- * 'channelName' - The name of the IoT Analytics channel to which message data will be sent.
-- * 'roleARN' - The ARN of the role which has a policy that grants IoT Analytics permission to send message data via IoT Analytics (iotanalytics:BatchPutMessage).
mkIotAnalyticsAction ::
  IotAnalyticsAction
mkIotAnalyticsAction =
  IotAnalyticsAction'
    { batchMode = Lude.Nothing,
      channelARN = Lude.Nothing,
      channelName = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | Whether to process the action as a batch. The default value is @false@ .
--
-- When @batchMode@ is @true@ and the rule SQL statement evaluates to an Array, each Array element is delivered as a separate message when passed by <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_BatchPutMessage.html @BatchPutMessage@ > to the AWS IoT Analytics channel. The resulting array can't have more than 100 messages.
--
-- /Note:/ Consider using 'batchMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaaBatchMode :: Lens.Lens' IotAnalyticsAction (Lude.Maybe Lude.Bool)
iaaBatchMode = Lens.lens (batchMode :: IotAnalyticsAction -> Lude.Maybe Lude.Bool) (\s a -> s {batchMode = a} :: IotAnalyticsAction)
{-# DEPRECATED iaaBatchMode "Use generic-lens or generic-optics with 'batchMode' instead." #-}

-- | (deprecated) The ARN of the IoT Analytics channel to which message data will be sent.
--
-- /Note:/ Consider using 'channelARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaaChannelARN :: Lens.Lens' IotAnalyticsAction (Lude.Maybe Lude.Text)
iaaChannelARN = Lens.lens (channelARN :: IotAnalyticsAction -> Lude.Maybe Lude.Text) (\s a -> s {channelARN = a} :: IotAnalyticsAction)
{-# DEPRECATED iaaChannelARN "Use generic-lens or generic-optics with 'channelARN' instead." #-}

-- | The name of the IoT Analytics channel to which message data will be sent.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaaChannelName :: Lens.Lens' IotAnalyticsAction (Lude.Maybe Lude.Text)
iaaChannelName = Lens.lens (channelName :: IotAnalyticsAction -> Lude.Maybe Lude.Text) (\s a -> s {channelName = a} :: IotAnalyticsAction)
{-# DEPRECATED iaaChannelName "Use generic-lens or generic-optics with 'channelName' instead." #-}

-- | The ARN of the role which has a policy that grants IoT Analytics permission to send message data via IoT Analytics (iotanalytics:BatchPutMessage).
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaaRoleARN :: Lens.Lens' IotAnalyticsAction (Lude.Maybe Lude.Text)
iaaRoleARN = Lens.lens (roleARN :: IotAnalyticsAction -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: IotAnalyticsAction)
{-# DEPRECATED iaaRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON IotAnalyticsAction where
  parseJSON =
    Lude.withObject
      "IotAnalyticsAction"
      ( \x ->
          IotAnalyticsAction'
            Lude.<$> (x Lude..:? "batchMode")
            Lude.<*> (x Lude..:? "channelArn")
            Lude.<*> (x Lude..:? "channelName")
            Lude.<*> (x Lude..:? "roleArn")
      )

instance Lude.ToJSON IotAnalyticsAction where
  toJSON IotAnalyticsAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("batchMode" Lude..=) Lude.<$> batchMode,
            ("channelArn" Lude..=) Lude.<$> channelARN,
            ("channelName" Lude..=) Lude.<$> channelName,
            ("roleArn" Lude..=) Lude.<$> roleARN
          ]
      )
