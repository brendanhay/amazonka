{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ApplicationSettingsResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.ApplicationSettingsResource
  ( ApplicationSettingsResource (..)
  -- * Smart constructor
  , mkApplicationSettingsResource
  -- * Lenses
  , asrApplicationId
  , asrCampaignHook
  , asrLastModifiedDate
  , asrLimits
  , asrQuietTime
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.CampaignHook as Types
import qualified Network.AWS.Pinpoint.Types.CampaignLimits as Types
import qualified Network.AWS.Pinpoint.Types.QuietTime as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about an application, including the default settings for an application.
--
-- /See:/ 'mkApplicationSettingsResource' smart constructor.
data ApplicationSettingsResource = ApplicationSettingsResource'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , campaignHook :: Core.Maybe Types.CampaignHook
    -- ^ The settings for the AWS Lambda function to invoke by default as a code hook for campaigns in the application. You can use this hook to customize segments that are used by campaigns in the application.
  , lastModifiedDate :: Core.Maybe Core.Text
    -- ^ The date and time, in ISO 8601 format, when the application's settings were last modified.
  , limits :: Core.Maybe Types.CampaignLimits
    -- ^ The default sending limits for campaigns in the application.
  , quietTime :: Core.Maybe Types.QuietTime
    -- ^ The default quiet time for campaigns in the application. Quiet time is a specific time range when messages aren't sent to endpoints, if all the following conditions are met:
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApplicationSettingsResource' value with any optional fields omitted.
mkApplicationSettingsResource
    :: Core.Text -- ^ 'applicationId'
    -> ApplicationSettingsResource
mkApplicationSettingsResource applicationId
  = ApplicationSettingsResource'{applicationId,
                                 campaignHook = Core.Nothing, lastModifiedDate = Core.Nothing,
                                 limits = Core.Nothing, quietTime = Core.Nothing}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asrApplicationId :: Lens.Lens' ApplicationSettingsResource Core.Text
asrApplicationId = Lens.field @"applicationId"
{-# INLINEABLE asrApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The settings for the AWS Lambda function to invoke by default as a code hook for campaigns in the application. You can use this hook to customize segments that are used by campaigns in the application.
--
-- /Note:/ Consider using 'campaignHook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asrCampaignHook :: Lens.Lens' ApplicationSettingsResource (Core.Maybe Types.CampaignHook)
asrCampaignHook = Lens.field @"campaignHook"
{-# INLINEABLE asrCampaignHook #-}
{-# DEPRECATED campaignHook "Use generic-lens or generic-optics with 'campaignHook' instead"  #-}

-- | The date and time, in ISO 8601 format, when the application's settings were last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asrLastModifiedDate :: Lens.Lens' ApplicationSettingsResource (Core.Maybe Core.Text)
asrLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE asrLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | The default sending limits for campaigns in the application.
--
-- /Note:/ Consider using 'limits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asrLimits :: Lens.Lens' ApplicationSettingsResource (Core.Maybe Types.CampaignLimits)
asrLimits = Lens.field @"limits"
{-# INLINEABLE asrLimits #-}
{-# DEPRECATED limits "Use generic-lens or generic-optics with 'limits' instead"  #-}

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
asrQuietTime :: Lens.Lens' ApplicationSettingsResource (Core.Maybe Types.QuietTime)
asrQuietTime = Lens.field @"quietTime"
{-# INLINEABLE asrQuietTime #-}
{-# DEPRECATED quietTime "Use generic-lens or generic-optics with 'quietTime' instead"  #-}

instance Core.FromJSON ApplicationSettingsResource where
        parseJSON
          = Core.withObject "ApplicationSettingsResource" Core.$
              \ x ->
                ApplicationSettingsResource' Core.<$>
                  (x Core..: "ApplicationId") Core.<*> x Core..:? "CampaignHook"
                    Core.<*> x Core..:? "LastModifiedDate"
                    Core.<*> x Core..:? "Limits"
                    Core.<*> x Core..:? "QuietTime"
