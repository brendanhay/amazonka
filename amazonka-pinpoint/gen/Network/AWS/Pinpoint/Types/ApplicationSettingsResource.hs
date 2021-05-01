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
-- Module      : Network.AWS.Pinpoint.Types.ApplicationSettingsResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ApplicationSettingsResource where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.CampaignHook
import Network.AWS.Pinpoint.Types.CampaignLimits
import Network.AWS.Pinpoint.Types.QuietTime
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about an application, including the default
-- settings for an application.
--
-- /See:/ 'newApplicationSettingsResource' smart constructor.
data ApplicationSettingsResource = ApplicationSettingsResource'
  { -- | The date and time, in ISO 8601 format, when the application\'s settings
    -- were last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Text,
    -- | The settings for the AWS Lambda function to invoke by default as a code
    -- hook for campaigns in the application. You can use this hook to
    -- customize segments that are used by campaigns in the application.
    campaignHook :: Prelude.Maybe CampaignHook,
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
    quietTime :: Prelude.Maybe QuietTime,
    -- | The default sending limits for campaigns in the application.
    limits :: Prelude.Maybe CampaignLimits,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ApplicationSettingsResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'applicationSettingsResource_lastModifiedDate' - The date and time, in ISO 8601 format, when the application\'s settings
-- were last modified.
--
-- 'campaignHook', 'applicationSettingsResource_campaignHook' - The settings for the AWS Lambda function to invoke by default as a code
-- hook for campaigns in the application. You can use this hook to
-- customize segments that are used by campaigns in the application.
--
-- 'quietTime', 'applicationSettingsResource_quietTime' - The default quiet time for campaigns in the application. Quiet time is a
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
-- 'limits', 'applicationSettingsResource_limits' - The default sending limits for campaigns in the application.
--
-- 'applicationId', 'applicationSettingsResource_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newApplicationSettingsResource ::
  -- | 'applicationId'
  Prelude.Text ->
  ApplicationSettingsResource
newApplicationSettingsResource pApplicationId_ =
  ApplicationSettingsResource'
    { lastModifiedDate =
        Prelude.Nothing,
      campaignHook = Prelude.Nothing,
      quietTime = Prelude.Nothing,
      limits = Prelude.Nothing,
      applicationId = pApplicationId_
    }

-- | The date and time, in ISO 8601 format, when the application\'s settings
-- were last modified.
applicationSettingsResource_lastModifiedDate :: Lens.Lens' ApplicationSettingsResource (Prelude.Maybe Prelude.Text)
applicationSettingsResource_lastModifiedDate = Lens.lens (\ApplicationSettingsResource' {lastModifiedDate} -> lastModifiedDate) (\s@ApplicationSettingsResource' {} a -> s {lastModifiedDate = a} :: ApplicationSettingsResource)

-- | The settings for the AWS Lambda function to invoke by default as a code
-- hook for campaigns in the application. You can use this hook to
-- customize segments that are used by campaigns in the application.
applicationSettingsResource_campaignHook :: Lens.Lens' ApplicationSettingsResource (Prelude.Maybe CampaignHook)
applicationSettingsResource_campaignHook = Lens.lens (\ApplicationSettingsResource' {campaignHook} -> campaignHook) (\s@ApplicationSettingsResource' {} a -> s {campaignHook = a} :: ApplicationSettingsResource)

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
applicationSettingsResource_quietTime :: Lens.Lens' ApplicationSettingsResource (Prelude.Maybe QuietTime)
applicationSettingsResource_quietTime = Lens.lens (\ApplicationSettingsResource' {quietTime} -> quietTime) (\s@ApplicationSettingsResource' {} a -> s {quietTime = a} :: ApplicationSettingsResource)

-- | The default sending limits for campaigns in the application.
applicationSettingsResource_limits :: Lens.Lens' ApplicationSettingsResource (Prelude.Maybe CampaignLimits)
applicationSettingsResource_limits = Lens.lens (\ApplicationSettingsResource' {limits} -> limits) (\s@ApplicationSettingsResource' {} a -> s {limits = a} :: ApplicationSettingsResource)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
applicationSettingsResource_applicationId :: Lens.Lens' ApplicationSettingsResource Prelude.Text
applicationSettingsResource_applicationId = Lens.lens (\ApplicationSettingsResource' {applicationId} -> applicationId) (\s@ApplicationSettingsResource' {} a -> s {applicationId = a} :: ApplicationSettingsResource)

instance Prelude.FromJSON ApplicationSettingsResource where
  parseJSON =
    Prelude.withObject
      "ApplicationSettingsResource"
      ( \x ->
          ApplicationSettingsResource'
            Prelude.<$> (x Prelude..:? "LastModifiedDate")
            Prelude.<*> (x Prelude..:? "CampaignHook")
            Prelude.<*> (x Prelude..:? "QuietTime")
            Prelude.<*> (x Prelude..:? "Limits")
            Prelude.<*> (x Prelude..: "ApplicationId")
      )

instance Prelude.Hashable ApplicationSettingsResource

instance Prelude.NFData ApplicationSettingsResource
