{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.WriteApplicationSettingsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.WriteApplicationSettingsRequest
  ( WriteApplicationSettingsRequest (..),

    -- * Smart constructor
    mkWriteApplicationSettingsRequest,

    -- * Lenses
    wasrCampaignHook,
    wasrCloudWatchMetricsEnabled,
    wasrEventTaggingEnabled,
    wasrLimits,
    wasrQuietTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.CampaignHook as Types
import qualified Network.AWS.Pinpoint.Types.CampaignLimits as Types
import qualified Network.AWS.Pinpoint.Types.QuietTime as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the default settings for an application.
--
-- /See:/ 'mkWriteApplicationSettingsRequest' smart constructor.
data WriteApplicationSettingsRequest = WriteApplicationSettingsRequest'
  { -- | The settings for the AWS Lambda function to invoke by default as a code hook for campaigns in the application. You can use this hook to customize segments that are used by campaigns in the application.
    --
    -- To override these settings and define custom settings for a specific campaign, use the CampaignHook object of the <link>Campaign resource.
    campaignHook :: Core.Maybe Types.CampaignHook,
    -- | Specifies whether to enable application-related alarms in Amazon CloudWatch.
    cloudWatchMetricsEnabled :: Core.Maybe Core.Bool,
    eventTaggingEnabled :: Core.Maybe Core.Bool,
    -- | The default sending limits for campaigns in the application. To override these limits and define custom limits for a specific campaign or journey, use the <link>Campaign resource or the <link>Journey resource, respectively.
    limits :: Core.Maybe Types.CampaignLimits,
    -- | The default quiet time for campaigns in the application. Quiet time is a specific time range when messages aren't sent to endpoints, if all the following conditions are met:
    --
    --
    --     * The EndpointDemographic.Timezone property of the endpoint is set to a valid value.
    --
    --
    --     * The current time in the endpoint's time zone is later than or equal to the time specified by the QuietTime.Start property for the application (or a campaign or journey that has custom quiet time settings).
    --
    --
    --     * The current time in the endpoint's time zone is earlier than or equal to the time specified by the QuietTime.End property for the application (or a campaign or journey that has custom quiet time settings).
    --
    --
    -- If any of the preceding conditions isn't met, the endpoint will receive messages from a campaign or journey, even if quiet time is enabled.
    -- To override the default quiet time settings for a specific campaign or journey, use the <link>Campaign resource or the <link>Journey resource to define a custom quiet time for the campaign or journey.
    quietTime :: Core.Maybe Types.QuietTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WriteApplicationSettingsRequest' value with any optional fields omitted.
mkWriteApplicationSettingsRequest ::
  WriteApplicationSettingsRequest
mkWriteApplicationSettingsRequest =
  WriteApplicationSettingsRequest'
    { campaignHook = Core.Nothing,
      cloudWatchMetricsEnabled = Core.Nothing,
      eventTaggingEnabled = Core.Nothing,
      limits = Core.Nothing,
      quietTime = Core.Nothing
    }

-- | The settings for the AWS Lambda function to invoke by default as a code hook for campaigns in the application. You can use this hook to customize segments that are used by campaigns in the application.
--
-- To override these settings and define custom settings for a specific campaign, use the CampaignHook object of the <link>Campaign resource.
--
-- /Note:/ Consider using 'campaignHook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wasrCampaignHook :: Lens.Lens' WriteApplicationSettingsRequest (Core.Maybe Types.CampaignHook)
wasrCampaignHook = Lens.field @"campaignHook"
{-# DEPRECATED wasrCampaignHook "Use generic-lens or generic-optics with 'campaignHook' instead." #-}

-- | Specifies whether to enable application-related alarms in Amazon CloudWatch.
--
-- /Note:/ Consider using 'cloudWatchMetricsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wasrCloudWatchMetricsEnabled :: Lens.Lens' WriteApplicationSettingsRequest (Core.Maybe Core.Bool)
wasrCloudWatchMetricsEnabled = Lens.field @"cloudWatchMetricsEnabled"
{-# DEPRECATED wasrCloudWatchMetricsEnabled "Use generic-lens or generic-optics with 'cloudWatchMetricsEnabled' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'eventTaggingEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wasrEventTaggingEnabled :: Lens.Lens' WriteApplicationSettingsRequest (Core.Maybe Core.Bool)
wasrEventTaggingEnabled = Lens.field @"eventTaggingEnabled"
{-# DEPRECATED wasrEventTaggingEnabled "Use generic-lens or generic-optics with 'eventTaggingEnabled' instead." #-}

-- | The default sending limits for campaigns in the application. To override these limits and define custom limits for a specific campaign or journey, use the <link>Campaign resource or the <link>Journey resource, respectively.
--
-- /Note:/ Consider using 'limits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wasrLimits :: Lens.Lens' WriteApplicationSettingsRequest (Core.Maybe Types.CampaignLimits)
wasrLimits = Lens.field @"limits"
{-# DEPRECATED wasrLimits "Use generic-lens or generic-optics with 'limits' instead." #-}

-- | The default quiet time for campaigns in the application. Quiet time is a specific time range when messages aren't sent to endpoints, if all the following conditions are met:
--
--
--     * The EndpointDemographic.Timezone property of the endpoint is set to a valid value.
--
--
--     * The current time in the endpoint's time zone is later than or equal to the time specified by the QuietTime.Start property for the application (or a campaign or journey that has custom quiet time settings).
--
--
--     * The current time in the endpoint's time zone is earlier than or equal to the time specified by the QuietTime.End property for the application (or a campaign or journey that has custom quiet time settings).
--
--
-- If any of the preceding conditions isn't met, the endpoint will receive messages from a campaign or journey, even if quiet time is enabled.
-- To override the default quiet time settings for a specific campaign or journey, use the <link>Campaign resource or the <link>Journey resource to define a custom quiet time for the campaign or journey.
--
-- /Note:/ Consider using 'quietTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wasrQuietTime :: Lens.Lens' WriteApplicationSettingsRequest (Core.Maybe Types.QuietTime)
wasrQuietTime = Lens.field @"quietTime"
{-# DEPRECATED wasrQuietTime "Use generic-lens or generic-optics with 'quietTime' instead." #-}

instance Core.FromJSON WriteApplicationSettingsRequest where
  toJSON WriteApplicationSettingsRequest {..} =
    Core.object
      ( Core.catMaybes
          [ ("CampaignHook" Core..=) Core.<$> campaignHook,
            ("CloudWatchMetricsEnabled" Core..=)
              Core.<$> cloudWatchMetricsEnabled,
            ("EventTaggingEnabled" Core..=) Core.<$> eventTaggingEnabled,
            ("Limits" Core..=) Core.<$> limits,
            ("QuietTime" Core..=) Core.<$> quietTime
          ]
      )
