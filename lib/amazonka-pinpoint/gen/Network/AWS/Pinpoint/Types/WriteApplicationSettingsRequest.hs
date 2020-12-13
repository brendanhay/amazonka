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
    wasrEventTaggingEnabled,
    wasrCloudWatchMetricsEnabled,
    wasrLimits,
    wasrQuietTime,
    wasrCampaignHook,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.CampaignHook
import Network.AWS.Pinpoint.Types.CampaignLimits
import Network.AWS.Pinpoint.Types.QuietTime
import qualified Network.AWS.Prelude as Lude

-- | Specifies the default settings for an application.
--
-- /See:/ 'mkWriteApplicationSettingsRequest' smart constructor.
data WriteApplicationSettingsRequest = WriteApplicationSettingsRequest'
  { eventTaggingEnabled :: Lude.Maybe Lude.Bool,
    -- | Specifies whether to enable application-related alarms in Amazon CloudWatch.
    cloudWatchMetricsEnabled :: Lude.Maybe Lude.Bool,
    -- | The default sending limits for campaigns in the application. To override these limits and define custom limits for a specific campaign or journey, use the <link>Campaign resource or the <link>Journey resource, respectively.
    limits :: Lude.Maybe CampaignLimits,
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
    quietTime :: Lude.Maybe QuietTime,
    -- | The settings for the AWS Lambda function to invoke by default as a code hook for campaigns in the application. You can use this hook to customize segments that are used by campaigns in the application.
    --
    -- To override these settings and define custom settings for a specific campaign, use the CampaignHook object of the <link>Campaign resource.
    campaignHook :: Lude.Maybe CampaignHook
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WriteApplicationSettingsRequest' with the minimum fields required to make a request.
--
-- * 'eventTaggingEnabled' -
-- * 'cloudWatchMetricsEnabled' - Specifies whether to enable application-related alarms in Amazon CloudWatch.
-- * 'limits' - The default sending limits for campaigns in the application. To override these limits and define custom limits for a specific campaign or journey, use the <link>Campaign resource or the <link>Journey resource, respectively.
-- * 'quietTime' - The default quiet time for campaigns in the application. Quiet time is a specific time range when messages aren't sent to endpoints, if all the following conditions are met:
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
-- * 'campaignHook' - The settings for the AWS Lambda function to invoke by default as a code hook for campaigns in the application. You can use this hook to customize segments that are used by campaigns in the application.
--
-- To override these settings and define custom settings for a specific campaign, use the CampaignHook object of the <link>Campaign resource.
mkWriteApplicationSettingsRequest ::
  WriteApplicationSettingsRequest
mkWriteApplicationSettingsRequest =
  WriteApplicationSettingsRequest'
    { eventTaggingEnabled =
        Lude.Nothing,
      cloudWatchMetricsEnabled = Lude.Nothing,
      limits = Lude.Nothing,
      quietTime = Lude.Nothing,
      campaignHook = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'eventTaggingEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wasrEventTaggingEnabled :: Lens.Lens' WriteApplicationSettingsRequest (Lude.Maybe Lude.Bool)
wasrEventTaggingEnabled = Lens.lens (eventTaggingEnabled :: WriteApplicationSettingsRequest -> Lude.Maybe Lude.Bool) (\s a -> s {eventTaggingEnabled = a} :: WriteApplicationSettingsRequest)
{-# DEPRECATED wasrEventTaggingEnabled "Use generic-lens or generic-optics with 'eventTaggingEnabled' instead." #-}

-- | Specifies whether to enable application-related alarms in Amazon CloudWatch.
--
-- /Note:/ Consider using 'cloudWatchMetricsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wasrCloudWatchMetricsEnabled :: Lens.Lens' WriteApplicationSettingsRequest (Lude.Maybe Lude.Bool)
wasrCloudWatchMetricsEnabled = Lens.lens (cloudWatchMetricsEnabled :: WriteApplicationSettingsRequest -> Lude.Maybe Lude.Bool) (\s a -> s {cloudWatchMetricsEnabled = a} :: WriteApplicationSettingsRequest)
{-# DEPRECATED wasrCloudWatchMetricsEnabled "Use generic-lens or generic-optics with 'cloudWatchMetricsEnabled' instead." #-}

-- | The default sending limits for campaigns in the application. To override these limits and define custom limits for a specific campaign or journey, use the <link>Campaign resource or the <link>Journey resource, respectively.
--
-- /Note:/ Consider using 'limits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wasrLimits :: Lens.Lens' WriteApplicationSettingsRequest (Lude.Maybe CampaignLimits)
wasrLimits = Lens.lens (limits :: WriteApplicationSettingsRequest -> Lude.Maybe CampaignLimits) (\s a -> s {limits = a} :: WriteApplicationSettingsRequest)
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
wasrQuietTime :: Lens.Lens' WriteApplicationSettingsRequest (Lude.Maybe QuietTime)
wasrQuietTime = Lens.lens (quietTime :: WriteApplicationSettingsRequest -> Lude.Maybe QuietTime) (\s a -> s {quietTime = a} :: WriteApplicationSettingsRequest)
{-# DEPRECATED wasrQuietTime "Use generic-lens or generic-optics with 'quietTime' instead." #-}

-- | The settings for the AWS Lambda function to invoke by default as a code hook for campaigns in the application. You can use this hook to customize segments that are used by campaigns in the application.
--
-- To override these settings and define custom settings for a specific campaign, use the CampaignHook object of the <link>Campaign resource.
--
-- /Note:/ Consider using 'campaignHook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wasrCampaignHook :: Lens.Lens' WriteApplicationSettingsRequest (Lude.Maybe CampaignHook)
wasrCampaignHook = Lens.lens (campaignHook :: WriteApplicationSettingsRequest -> Lude.Maybe CampaignHook) (\s a -> s {campaignHook = a} :: WriteApplicationSettingsRequest)
{-# DEPRECATED wasrCampaignHook "Use generic-lens or generic-optics with 'campaignHook' instead." #-}

instance Lude.ToJSON WriteApplicationSettingsRequest where
  toJSON WriteApplicationSettingsRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EventTaggingEnabled" Lude..=) Lude.<$> eventTaggingEnabled,
            ("CloudWatchMetricsEnabled" Lude..=)
              Lude.<$> cloudWatchMetricsEnabled,
            ("Limits" Lude..=) Lude.<$> limits,
            ("QuietTime" Lude..=) Lude.<$> quietTime,
            ("CampaignHook" Lude..=) Lude.<$> campaignHook
          ]
      )
