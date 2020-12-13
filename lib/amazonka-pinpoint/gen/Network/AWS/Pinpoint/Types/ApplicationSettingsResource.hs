{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ApplicationSettingsResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ApplicationSettingsResource
  ( ApplicationSettingsResource (..),

    -- * Smart constructor
    mkApplicationSettingsResource,

    -- * Lenses
    asrLastModifiedDate,
    asrLimits,
    asrQuietTime,
    asrApplicationId,
    asrCampaignHook,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.CampaignHook
import Network.AWS.Pinpoint.Types.CampaignLimits
import Network.AWS.Pinpoint.Types.QuietTime
import qualified Network.AWS.Prelude as Lude

-- | Provides information about an application, including the default settings for an application.
--
-- /See:/ 'mkApplicationSettingsResource' smart constructor.
data ApplicationSettingsResource = ApplicationSettingsResource'
  { -- | The date and time, in ISO 8601 format, when the application's settings were last modified.
    lastModifiedDate :: Lude.Maybe Lude.Text,
    -- | The default sending limits for campaigns in the application.
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
    quietTime :: Lude.Maybe QuietTime,
    -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text,
    -- | The settings for the AWS Lambda function to invoke by default as a code hook for campaigns in the application. You can use this hook to customize segments that are used by campaigns in the application.
    campaignHook :: Lude.Maybe CampaignHook
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplicationSettingsResource' with the minimum fields required to make a request.
--
-- * 'lastModifiedDate' - The date and time, in ISO 8601 format, when the application's settings were last modified.
-- * 'limits' - The default sending limits for campaigns in the application.
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
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'campaignHook' - The settings for the AWS Lambda function to invoke by default as a code hook for campaigns in the application. You can use this hook to customize segments that are used by campaigns in the application.
mkApplicationSettingsResource ::
  -- | 'applicationId'
  Lude.Text ->
  ApplicationSettingsResource
mkApplicationSettingsResource pApplicationId_ =
  ApplicationSettingsResource'
    { lastModifiedDate = Lude.Nothing,
      limits = Lude.Nothing,
      quietTime = Lude.Nothing,
      applicationId = pApplicationId_,
      campaignHook = Lude.Nothing
    }

-- | The date and time, in ISO 8601 format, when the application's settings were last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asrLastModifiedDate :: Lens.Lens' ApplicationSettingsResource (Lude.Maybe Lude.Text)
asrLastModifiedDate = Lens.lens (lastModifiedDate :: ApplicationSettingsResource -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedDate = a} :: ApplicationSettingsResource)
{-# DEPRECATED asrLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The default sending limits for campaigns in the application.
--
-- /Note:/ Consider using 'limits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asrLimits :: Lens.Lens' ApplicationSettingsResource (Lude.Maybe CampaignLimits)
asrLimits = Lens.lens (limits :: ApplicationSettingsResource -> Lude.Maybe CampaignLimits) (\s a -> s {limits = a} :: ApplicationSettingsResource)
{-# DEPRECATED asrLimits "Use generic-lens or generic-optics with 'limits' instead." #-}

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
--
-- /Note:/ Consider using 'quietTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asrQuietTime :: Lens.Lens' ApplicationSettingsResource (Lude.Maybe QuietTime)
asrQuietTime = Lens.lens (quietTime :: ApplicationSettingsResource -> Lude.Maybe QuietTime) (\s a -> s {quietTime = a} :: ApplicationSettingsResource)
{-# DEPRECATED asrQuietTime "Use generic-lens or generic-optics with 'quietTime' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asrApplicationId :: Lens.Lens' ApplicationSettingsResource Lude.Text
asrApplicationId = Lens.lens (applicationId :: ApplicationSettingsResource -> Lude.Text) (\s a -> s {applicationId = a} :: ApplicationSettingsResource)
{-# DEPRECATED asrApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The settings for the AWS Lambda function to invoke by default as a code hook for campaigns in the application. You can use this hook to customize segments that are used by campaigns in the application.
--
-- /Note:/ Consider using 'campaignHook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asrCampaignHook :: Lens.Lens' ApplicationSettingsResource (Lude.Maybe CampaignHook)
asrCampaignHook = Lens.lens (campaignHook :: ApplicationSettingsResource -> Lude.Maybe CampaignHook) (\s a -> s {campaignHook = a} :: ApplicationSettingsResource)
{-# DEPRECATED asrCampaignHook "Use generic-lens or generic-optics with 'campaignHook' instead." #-}

instance Lude.FromJSON ApplicationSettingsResource where
  parseJSON =
    Lude.withObject
      "ApplicationSettingsResource"
      ( \x ->
          ApplicationSettingsResource'
            Lude.<$> (x Lude..:? "LastModifiedDate")
            Lude.<*> (x Lude..:? "Limits")
            Lude.<*> (x Lude..:? "QuietTime")
            Lude.<*> (x Lude..: "ApplicationId")
            Lude.<*> (x Lude..:? "CampaignHook")
      )
