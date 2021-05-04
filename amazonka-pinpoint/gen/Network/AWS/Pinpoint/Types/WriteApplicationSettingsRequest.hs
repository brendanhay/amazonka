{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Pinpoint.Types.WriteApplicationSettingsRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.WriteApplicationSettingsRequest where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.CampaignHook
import Network.AWS.Pinpoint.Types.CampaignLimits
import Network.AWS.Pinpoint.Types.QuietTime
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the default settings for an application.
--
-- /See:/ 'newWriteApplicationSettingsRequest' smart constructor.
data WriteApplicationSettingsRequest = WriteApplicationSettingsRequest'
  { -- | The settings for the AWS Lambda function to invoke by default as a code
    -- hook for campaigns in the application. You can use this hook to
    -- customize segments that are used by campaigns in the application.
    --
    -- To override these settings and define custom settings for a specific
    -- campaign, use the CampaignHook object of the Campaign resource.
    campaignHook :: Prelude.Maybe CampaignHook,
    -- | Specifies whether to enable application-related alarms in Amazon
    -- CloudWatch.
    cloudWatchMetricsEnabled :: Prelude.Maybe Prelude.Bool,
    eventTaggingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The default quiet time for campaigns in the application. Quiet time is a
    -- specific time range when messages aren\'t sent to endpoints, if all the
    -- following conditions are met:
    --
    -- -   The EndpointDemographic.Timezone property of the endpoint is set to
    --     a valid value.
    --
    -- -   The current time in the endpoint\'s time zone is later than or equal
    --     to the time specified by the QuietTime.Start property for the
    --     application (or a campaign or journey that has custom quiet time
    --     settings).
    --
    -- -   The current time in the endpoint\'s time zone is earlier than or
    --     equal to the time specified by the QuietTime.End property for the
    --     application (or a campaign or journey that has custom quiet time
    --     settings).
    --
    -- If any of the preceding conditions isn\'t met, the endpoint will receive
    -- messages from a campaign or journey, even if quiet time is enabled.
    --
    -- To override the default quiet time settings for a specific campaign or
    -- journey, use the Campaign resource or the Journey resource to define a
    -- custom quiet time for the campaign or journey.
    quietTime :: Prelude.Maybe QuietTime,
    -- | The default sending limits for campaigns in the application. To override
    -- these limits and define custom limits for a specific campaign or
    -- journey, use the Campaign resource or the Journey resource,
    -- respectively.
    limits :: Prelude.Maybe CampaignLimits
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WriteApplicationSettingsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'campaignHook', 'writeApplicationSettingsRequest_campaignHook' - The settings for the AWS Lambda function to invoke by default as a code
-- hook for campaigns in the application. You can use this hook to
-- customize segments that are used by campaigns in the application.
--
-- To override these settings and define custom settings for a specific
-- campaign, use the CampaignHook object of the Campaign resource.
--
-- 'cloudWatchMetricsEnabled', 'writeApplicationSettingsRequest_cloudWatchMetricsEnabled' - Specifies whether to enable application-related alarms in Amazon
-- CloudWatch.
--
-- 'eventTaggingEnabled', 'writeApplicationSettingsRequest_eventTaggingEnabled' - Undocumented member.
--
-- 'quietTime', 'writeApplicationSettingsRequest_quietTime' - The default quiet time for campaigns in the application. Quiet time is a
-- specific time range when messages aren\'t sent to endpoints, if all the
-- following conditions are met:
--
-- -   The EndpointDemographic.Timezone property of the endpoint is set to
--     a valid value.
--
-- -   The current time in the endpoint\'s time zone is later than or equal
--     to the time specified by the QuietTime.Start property for the
--     application (or a campaign or journey that has custom quiet time
--     settings).
--
-- -   The current time in the endpoint\'s time zone is earlier than or
--     equal to the time specified by the QuietTime.End property for the
--     application (or a campaign or journey that has custom quiet time
--     settings).
--
-- If any of the preceding conditions isn\'t met, the endpoint will receive
-- messages from a campaign or journey, even if quiet time is enabled.
--
-- To override the default quiet time settings for a specific campaign or
-- journey, use the Campaign resource or the Journey resource to define a
-- custom quiet time for the campaign or journey.
--
-- 'limits', 'writeApplicationSettingsRequest_limits' - The default sending limits for campaigns in the application. To override
-- these limits and define custom limits for a specific campaign or
-- journey, use the Campaign resource or the Journey resource,
-- respectively.
newWriteApplicationSettingsRequest ::
  WriteApplicationSettingsRequest
newWriteApplicationSettingsRequest =
  WriteApplicationSettingsRequest'
    { campaignHook =
        Prelude.Nothing,
      cloudWatchMetricsEnabled = Prelude.Nothing,
      eventTaggingEnabled = Prelude.Nothing,
      quietTime = Prelude.Nothing,
      limits = Prelude.Nothing
    }

-- | The settings for the AWS Lambda function to invoke by default as a code
-- hook for campaigns in the application. You can use this hook to
-- customize segments that are used by campaigns in the application.
--
-- To override these settings and define custom settings for a specific
-- campaign, use the CampaignHook object of the Campaign resource.
writeApplicationSettingsRequest_campaignHook :: Lens.Lens' WriteApplicationSettingsRequest (Prelude.Maybe CampaignHook)
writeApplicationSettingsRequest_campaignHook = Lens.lens (\WriteApplicationSettingsRequest' {campaignHook} -> campaignHook) (\s@WriteApplicationSettingsRequest' {} a -> s {campaignHook = a} :: WriteApplicationSettingsRequest)

-- | Specifies whether to enable application-related alarms in Amazon
-- CloudWatch.
writeApplicationSettingsRequest_cloudWatchMetricsEnabled :: Lens.Lens' WriteApplicationSettingsRequest (Prelude.Maybe Prelude.Bool)
writeApplicationSettingsRequest_cloudWatchMetricsEnabled = Lens.lens (\WriteApplicationSettingsRequest' {cloudWatchMetricsEnabled} -> cloudWatchMetricsEnabled) (\s@WriteApplicationSettingsRequest' {} a -> s {cloudWatchMetricsEnabled = a} :: WriteApplicationSettingsRequest)

-- | Undocumented member.
writeApplicationSettingsRequest_eventTaggingEnabled :: Lens.Lens' WriteApplicationSettingsRequest (Prelude.Maybe Prelude.Bool)
writeApplicationSettingsRequest_eventTaggingEnabled = Lens.lens (\WriteApplicationSettingsRequest' {eventTaggingEnabled} -> eventTaggingEnabled) (\s@WriteApplicationSettingsRequest' {} a -> s {eventTaggingEnabled = a} :: WriteApplicationSettingsRequest)

-- | The default quiet time for campaigns in the application. Quiet time is a
-- specific time range when messages aren\'t sent to endpoints, if all the
-- following conditions are met:
--
-- -   The EndpointDemographic.Timezone property of the endpoint is set to
--     a valid value.
--
-- -   The current time in the endpoint\'s time zone is later than or equal
--     to the time specified by the QuietTime.Start property for the
--     application (or a campaign or journey that has custom quiet time
--     settings).
--
-- -   The current time in the endpoint\'s time zone is earlier than or
--     equal to the time specified by the QuietTime.End property for the
--     application (or a campaign or journey that has custom quiet time
--     settings).
--
-- If any of the preceding conditions isn\'t met, the endpoint will receive
-- messages from a campaign or journey, even if quiet time is enabled.
--
-- To override the default quiet time settings for a specific campaign or
-- journey, use the Campaign resource or the Journey resource to define a
-- custom quiet time for the campaign or journey.
writeApplicationSettingsRequest_quietTime :: Lens.Lens' WriteApplicationSettingsRequest (Prelude.Maybe QuietTime)
writeApplicationSettingsRequest_quietTime = Lens.lens (\WriteApplicationSettingsRequest' {quietTime} -> quietTime) (\s@WriteApplicationSettingsRequest' {} a -> s {quietTime = a} :: WriteApplicationSettingsRequest)

-- | The default sending limits for campaigns in the application. To override
-- these limits and define custom limits for a specific campaign or
-- journey, use the Campaign resource or the Journey resource,
-- respectively.
writeApplicationSettingsRequest_limits :: Lens.Lens' WriteApplicationSettingsRequest (Prelude.Maybe CampaignLimits)
writeApplicationSettingsRequest_limits = Lens.lens (\WriteApplicationSettingsRequest' {limits} -> limits) (\s@WriteApplicationSettingsRequest' {} a -> s {limits = a} :: WriteApplicationSettingsRequest)

instance
  Prelude.Hashable
    WriteApplicationSettingsRequest

instance
  Prelude.NFData
    WriteApplicationSettingsRequest

instance
  Prelude.ToJSON
    WriteApplicationSettingsRequest
  where
  toJSON WriteApplicationSettingsRequest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CampaignHook" Prelude..=)
              Prelude.<$> campaignHook,
            ("CloudWatchMetricsEnabled" Prelude..=)
              Prelude.<$> cloudWatchMetricsEnabled,
            ("EventTaggingEnabled" Prelude..=)
              Prelude.<$> eventTaggingEnabled,
            ("QuietTime" Prelude..=) Prelude.<$> quietTime,
            ("Limits" Prelude..=) Prelude.<$> limits
          ]
      )
