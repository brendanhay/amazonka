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
-- Module      : Amazonka.Pinpoint.Types.WriteApplicationSettingsRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.WriteApplicationSettingsRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.CampaignHook
import Amazonka.Pinpoint.Types.CampaignLimits
import Amazonka.Pinpoint.Types.QuietTime
import qualified Amazonka.Prelude as Prelude

-- | Specifies the default settings for an application.
--
-- /See:/ 'newWriteApplicationSettingsRequest' smart constructor.
data WriteApplicationSettingsRequest = WriteApplicationSettingsRequest'
  { -- | The default sending limits for campaigns in the application. To override
    -- these limits and define custom limits for a specific campaign or
    -- journey, use the Campaign resource or the Journey resource,
    -- respectively.
    limits :: Prelude.Maybe CampaignLimits,
    eventTaggingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether to enable application-related alarms in Amazon
    -- CloudWatch.
    cloudWatchMetricsEnabled :: Prelude.Maybe Prelude.Bool,
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
    -- | The settings for the AWS Lambda function to invoke by default as a code
    -- hook for campaigns in the application. You can use this hook to
    -- customize segments that are used by campaigns in the application.
    --
    -- To override these settings and define custom settings for a specific
    -- campaign, use the CampaignHook object of the Campaign resource.
    campaignHook :: Prelude.Maybe CampaignHook
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WriteApplicationSettingsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limits', 'writeApplicationSettingsRequest_limits' - The default sending limits for campaigns in the application. To override
-- these limits and define custom limits for a specific campaign or
-- journey, use the Campaign resource or the Journey resource,
-- respectively.
--
-- 'eventTaggingEnabled', 'writeApplicationSettingsRequest_eventTaggingEnabled' - Undocumented member.
--
-- 'cloudWatchMetricsEnabled', 'writeApplicationSettingsRequest_cloudWatchMetricsEnabled' - Specifies whether to enable application-related alarms in Amazon
-- CloudWatch.
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
-- 'campaignHook', 'writeApplicationSettingsRequest_campaignHook' - The settings for the AWS Lambda function to invoke by default as a code
-- hook for campaigns in the application. You can use this hook to
-- customize segments that are used by campaigns in the application.
--
-- To override these settings and define custom settings for a specific
-- campaign, use the CampaignHook object of the Campaign resource.
newWriteApplicationSettingsRequest ::
  WriteApplicationSettingsRequest
newWriteApplicationSettingsRequest =
  WriteApplicationSettingsRequest'
    { limits =
        Prelude.Nothing,
      eventTaggingEnabled = Prelude.Nothing,
      cloudWatchMetricsEnabled = Prelude.Nothing,
      quietTime = Prelude.Nothing,
      campaignHook = Prelude.Nothing
    }

-- | The default sending limits for campaigns in the application. To override
-- these limits and define custom limits for a specific campaign or
-- journey, use the Campaign resource or the Journey resource,
-- respectively.
writeApplicationSettingsRequest_limits :: Lens.Lens' WriteApplicationSettingsRequest (Prelude.Maybe CampaignLimits)
writeApplicationSettingsRequest_limits = Lens.lens (\WriteApplicationSettingsRequest' {limits} -> limits) (\s@WriteApplicationSettingsRequest' {} a -> s {limits = a} :: WriteApplicationSettingsRequest)

-- | Undocumented member.
writeApplicationSettingsRequest_eventTaggingEnabled :: Lens.Lens' WriteApplicationSettingsRequest (Prelude.Maybe Prelude.Bool)
writeApplicationSettingsRequest_eventTaggingEnabled = Lens.lens (\WriteApplicationSettingsRequest' {eventTaggingEnabled} -> eventTaggingEnabled) (\s@WriteApplicationSettingsRequest' {} a -> s {eventTaggingEnabled = a} :: WriteApplicationSettingsRequest)

-- | Specifies whether to enable application-related alarms in Amazon
-- CloudWatch.
writeApplicationSettingsRequest_cloudWatchMetricsEnabled :: Lens.Lens' WriteApplicationSettingsRequest (Prelude.Maybe Prelude.Bool)
writeApplicationSettingsRequest_cloudWatchMetricsEnabled = Lens.lens (\WriteApplicationSettingsRequest' {cloudWatchMetricsEnabled} -> cloudWatchMetricsEnabled) (\s@WriteApplicationSettingsRequest' {} a -> s {cloudWatchMetricsEnabled = a} :: WriteApplicationSettingsRequest)

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

-- | The settings for the AWS Lambda function to invoke by default as a code
-- hook for campaigns in the application. You can use this hook to
-- customize segments that are used by campaigns in the application.
--
-- To override these settings and define custom settings for a specific
-- campaign, use the CampaignHook object of the Campaign resource.
writeApplicationSettingsRequest_campaignHook :: Lens.Lens' WriteApplicationSettingsRequest (Prelude.Maybe CampaignHook)
writeApplicationSettingsRequest_campaignHook = Lens.lens (\WriteApplicationSettingsRequest' {campaignHook} -> campaignHook) (\s@WriteApplicationSettingsRequest' {} a -> s {campaignHook = a} :: WriteApplicationSettingsRequest)

instance
  Prelude.Hashable
    WriteApplicationSettingsRequest
  where
  hashWithSalt
    _salt
    WriteApplicationSettingsRequest' {..} =
      _salt `Prelude.hashWithSalt` limits
        `Prelude.hashWithSalt` eventTaggingEnabled
        `Prelude.hashWithSalt` cloudWatchMetricsEnabled
        `Prelude.hashWithSalt` quietTime
        `Prelude.hashWithSalt` campaignHook

instance
  Prelude.NFData
    WriteApplicationSettingsRequest
  where
  rnf WriteApplicationSettingsRequest' {..} =
    Prelude.rnf limits
      `Prelude.seq` Prelude.rnf eventTaggingEnabled
      `Prelude.seq` Prelude.rnf cloudWatchMetricsEnabled
      `Prelude.seq` Prelude.rnf quietTime
      `Prelude.seq` Prelude.rnf campaignHook

instance Core.ToJSON WriteApplicationSettingsRequest where
  toJSON WriteApplicationSettingsRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Limits" Core..=) Prelude.<$> limits,
            ("EventTaggingEnabled" Core..=)
              Prelude.<$> eventTaggingEnabled,
            ("CloudWatchMetricsEnabled" Core..=)
              Prelude.<$> cloudWatchMetricsEnabled,
            ("QuietTime" Core..=) Prelude.<$> quietTime,
            ("CampaignHook" Core..=) Prelude.<$> campaignHook
          ]
      )
