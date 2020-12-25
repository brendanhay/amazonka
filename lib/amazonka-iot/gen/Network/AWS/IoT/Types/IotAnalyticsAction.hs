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
    iaaChannelArn,
    iaaChannelName,
    iaaRoleArn,
  )
where

import qualified Network.AWS.IoT.Types.AwsArn as Types
import qualified Network.AWS.IoT.Types.ChannelArn as Types
import qualified Network.AWS.IoT.Types.ChannelName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Sends message data to an AWS IoT Analytics channel.
--
-- /See:/ 'mkIotAnalyticsAction' smart constructor.
data IotAnalyticsAction = IotAnalyticsAction'
  { -- | Whether to process the action as a batch. The default value is @false@ .
    --
    -- When @batchMode@ is @true@ and the rule SQL statement evaluates to an Array, each Array element is delivered as a separate message when passed by <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_BatchPutMessage.html @BatchPutMessage@ > to the AWS IoT Analytics channel. The resulting array can't have more than 100 messages.
    batchMode :: Core.Maybe Core.Bool,
    -- | (deprecated) The ARN of the IoT Analytics channel to which message data will be sent.
    channelArn :: Core.Maybe Types.ChannelArn,
    -- | The name of the IoT Analytics channel to which message data will be sent.
    channelName :: Core.Maybe Types.ChannelName,
    -- | The ARN of the role which has a policy that grants IoT Analytics permission to send message data via IoT Analytics (iotanalytics:BatchPutMessage).
    roleArn :: Core.Maybe Types.AwsArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IotAnalyticsAction' value with any optional fields omitted.
mkIotAnalyticsAction ::
  IotAnalyticsAction
mkIotAnalyticsAction =
  IotAnalyticsAction'
    { batchMode = Core.Nothing,
      channelArn = Core.Nothing,
      channelName = Core.Nothing,
      roleArn = Core.Nothing
    }

-- | Whether to process the action as a batch. The default value is @false@ .
--
-- When @batchMode@ is @true@ and the rule SQL statement evaluates to an Array, each Array element is delivered as a separate message when passed by <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_BatchPutMessage.html @BatchPutMessage@ > to the AWS IoT Analytics channel. The resulting array can't have more than 100 messages.
--
-- /Note:/ Consider using 'batchMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaaBatchMode :: Lens.Lens' IotAnalyticsAction (Core.Maybe Core.Bool)
iaaBatchMode = Lens.field @"batchMode"
{-# DEPRECATED iaaBatchMode "Use generic-lens or generic-optics with 'batchMode' instead." #-}

-- | (deprecated) The ARN of the IoT Analytics channel to which message data will be sent.
--
-- /Note:/ Consider using 'channelArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaaChannelArn :: Lens.Lens' IotAnalyticsAction (Core.Maybe Types.ChannelArn)
iaaChannelArn = Lens.field @"channelArn"
{-# DEPRECATED iaaChannelArn "Use generic-lens or generic-optics with 'channelArn' instead." #-}

-- | The name of the IoT Analytics channel to which message data will be sent.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaaChannelName :: Lens.Lens' IotAnalyticsAction (Core.Maybe Types.ChannelName)
iaaChannelName = Lens.field @"channelName"
{-# DEPRECATED iaaChannelName "Use generic-lens or generic-optics with 'channelName' instead." #-}

-- | The ARN of the role which has a policy that grants IoT Analytics permission to send message data via IoT Analytics (iotanalytics:BatchPutMessage).
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaaRoleArn :: Lens.Lens' IotAnalyticsAction (Core.Maybe Types.AwsArn)
iaaRoleArn = Lens.field @"roleArn"
{-# DEPRECATED iaaRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

instance Core.FromJSON IotAnalyticsAction where
  toJSON IotAnalyticsAction {..} =
    Core.object
      ( Core.catMaybes
          [ ("batchMode" Core..=) Core.<$> batchMode,
            ("channelArn" Core..=) Core.<$> channelArn,
            ("channelName" Core..=) Core.<$> channelName,
            ("roleArn" Core..=) Core.<$> roleArn
          ]
      )

instance Core.FromJSON IotAnalyticsAction where
  parseJSON =
    Core.withObject "IotAnalyticsAction" Core.$
      \x ->
        IotAnalyticsAction'
          Core.<$> (x Core..:? "batchMode")
          Core.<*> (x Core..:? "channelArn")
          Core.<*> (x Core..:? "channelName")
          Core.<*> (x Core..:? "roleArn")
