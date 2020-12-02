{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ApplicationSettingsResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ApplicationSettingsResource where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.CampaignHook
import Network.AWS.Pinpoint.Types.CampaignLimits
import Network.AWS.Pinpoint.Types.QuietTime
import Network.AWS.Prelude

-- | Provides information about an application, including the default settings for an application.
--
--
--
-- /See:/ 'applicationSettingsResource' smart constructor.
data ApplicationSettingsResource = ApplicationSettingsResource'
  { _asrLastModifiedDate ::
      !(Maybe Text),
    _asrLimits ::
      !(Maybe CampaignLimits),
    _asrQuietTime :: !(Maybe QuietTime),
    _asrCampaignHook ::
      !(Maybe CampaignHook),
    _asrApplicationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ApplicationSettingsResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asrLastModifiedDate' - The date and time, in ISO 8601 format, when the application's settings were last modified.
--
-- * 'asrLimits' - The default sending limits for campaigns in the application.
--
-- * 'asrQuietTime' - The default quiet time for campaigns in the application. Quiet time is a specific time range when messages aren't sent to endpoints, if all the following conditions are met:     * The EndpointDemographic.Timezone property of the endpoint is set to a valid value.     * The current time in the endpoint's time zone is later than or equal to the time specified by the QuietTime.Start property for the application (or a campaign or journey that has custom quiet time settings).     * The current time in the endpoint's time zone is earlier than or equal to the time specified by the QuietTime.End property for the application (or a campaign or journey that has custom quiet time settings). If any of the preceding conditions isn't met, the endpoint will receive messages from a campaign or journey, even if quiet time is enabled.
--
-- * 'asrCampaignHook' - The settings for the AWS Lambda function to invoke by default as a code hook for campaigns in the application. You can use this hook to customize segments that are used by campaigns in the application.
--
-- * 'asrApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
applicationSettingsResource ::
  -- | 'asrApplicationId'
  Text ->
  ApplicationSettingsResource
applicationSettingsResource pApplicationId_ =
  ApplicationSettingsResource'
    { _asrLastModifiedDate = Nothing,
      _asrLimits = Nothing,
      _asrQuietTime = Nothing,
      _asrCampaignHook = Nothing,
      _asrApplicationId = pApplicationId_
    }

-- | The date and time, in ISO 8601 format, when the application's settings were last modified.
asrLastModifiedDate :: Lens' ApplicationSettingsResource (Maybe Text)
asrLastModifiedDate = lens _asrLastModifiedDate (\s a -> s {_asrLastModifiedDate = a})

-- | The default sending limits for campaigns in the application.
asrLimits :: Lens' ApplicationSettingsResource (Maybe CampaignLimits)
asrLimits = lens _asrLimits (\s a -> s {_asrLimits = a})

-- | The default quiet time for campaigns in the application. Quiet time is a specific time range when messages aren't sent to endpoints, if all the following conditions are met:     * The EndpointDemographic.Timezone property of the endpoint is set to a valid value.     * The current time in the endpoint's time zone is later than or equal to the time specified by the QuietTime.Start property for the application (or a campaign or journey that has custom quiet time settings).     * The current time in the endpoint's time zone is earlier than or equal to the time specified by the QuietTime.End property for the application (or a campaign or journey that has custom quiet time settings). If any of the preceding conditions isn't met, the endpoint will receive messages from a campaign or journey, even if quiet time is enabled.
asrQuietTime :: Lens' ApplicationSettingsResource (Maybe QuietTime)
asrQuietTime = lens _asrQuietTime (\s a -> s {_asrQuietTime = a})

-- | The settings for the AWS Lambda function to invoke by default as a code hook for campaigns in the application. You can use this hook to customize segments that are used by campaigns in the application.
asrCampaignHook :: Lens' ApplicationSettingsResource (Maybe CampaignHook)
asrCampaignHook = lens _asrCampaignHook (\s a -> s {_asrCampaignHook = a})

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
asrApplicationId :: Lens' ApplicationSettingsResource Text
asrApplicationId = lens _asrApplicationId (\s a -> s {_asrApplicationId = a})

instance FromJSON ApplicationSettingsResource where
  parseJSON =
    withObject
      "ApplicationSettingsResource"
      ( \x ->
          ApplicationSettingsResource'
            <$> (x .:? "LastModifiedDate")
            <*> (x .:? "Limits")
            <*> (x .:? "QuietTime")
            <*> (x .:? "CampaignHook")
            <*> (x .: "ApplicationId")
      )

instance Hashable ApplicationSettingsResource

instance NFData ApplicationSettingsResource
