{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.WriteCampaignRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.WriteCampaignRequest where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.CampaignHook
import Network.AWS.Pinpoint.Types.CampaignLimits
import Network.AWS.Pinpoint.Types.CustomDeliveryConfiguration
import Network.AWS.Pinpoint.Types.MessageConfiguration
import Network.AWS.Pinpoint.Types.Schedule
import Network.AWS.Pinpoint.Types.TemplateConfiguration
import Network.AWS.Pinpoint.Types.WriteTreatmentResource
import Network.AWS.Prelude

-- | Specifies the configuration and other settings for a campaign.
--
--
--
-- /See:/ 'writeCampaignRequest' smart constructor.
data WriteCampaignRequest = WriteCampaignRequest'
  { _wcrCustomDeliveryConfiguration ::
      !(Maybe CustomDeliveryConfiguration),
    _wcrSchedule :: !(Maybe Schedule),
    _wcrTemplateConfiguration ::
      !(Maybe TemplateConfiguration),
    _wcrHook :: !(Maybe CampaignHook),
    _wcrTreatmentName :: !(Maybe Text),
    _wcrLimits :: !(Maybe CampaignLimits),
    _wcrIsPaused :: !(Maybe Bool),
    _wcrName :: !(Maybe Text),
    _wcrHoldoutPercent :: !(Maybe Int),
    _wcrTreatmentDescription :: !(Maybe Text),
    _wcrMessageConfiguration ::
      !(Maybe MessageConfiguration),
    _wcrDescription :: !(Maybe Text),
    _wcrSegmentId :: !(Maybe Text),
    _wcrAdditionalTreatments ::
      !(Maybe [WriteTreatmentResource]),
    _wcrTags :: !(Maybe (Map Text (Text))),
    _wcrSegmentVersion :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WriteCampaignRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wcrCustomDeliveryConfiguration' - The delivery configuration settings for sending the campaign through a custom channel. This object is required if the MessageConfiguration object for the campaign specifies a CustomMessage object.
--
-- * 'wcrSchedule' - The schedule settings for the campaign.
--
-- * 'wcrTemplateConfiguration' - The message template to use for the campaign.
--
-- * 'wcrHook' - The settings for the AWS Lambda function to invoke as a code hook for the campaign. You can use this hook to customize the segment that's used by the campaign.
--
-- * 'wcrTreatmentName' - A custom name of the default treatment for the campaign, if the campaign has multiple treatments. A /treatment/ is a variation of a campaign that's used for A/B testing.
--
-- * 'wcrLimits' - The messaging limits for the campaign.
--
-- * 'wcrIsPaused' - Specifies whether to pause the campaign. A paused campaign doesn't run unless you resume it by changing this value to false.
--
-- * 'wcrName' - A custom name for the campaign.
--
-- * 'wcrHoldoutPercent' - The allocated percentage of users (segment members) who shouldn't receive messages from the campaign.
--
-- * 'wcrTreatmentDescription' - A custom description of the default treatment for the campaign.
--
-- * 'wcrMessageConfiguration' - The message configuration settings for the campaign.
--
-- * 'wcrDescription' - A custom description of the campaign.
--
-- * 'wcrSegmentId' - The unique identifier for the segment to associate with the campaign.
--
-- * 'wcrAdditionalTreatments' - An array of requests that defines additional treatments for the campaign, in addition to the default treatment for the campaign.
--
-- * 'wcrTags' - A string-to-string map of key-value pairs that defines the tags to associate with the campaign. Each tag consists of a required tag key and an associated tag value.
--
-- * 'wcrSegmentVersion' - The version of the segment to associate with the campaign.
writeCampaignRequest ::
  WriteCampaignRequest
writeCampaignRequest =
  WriteCampaignRequest'
    { _wcrCustomDeliveryConfiguration = Nothing,
      _wcrSchedule = Nothing,
      _wcrTemplateConfiguration = Nothing,
      _wcrHook = Nothing,
      _wcrTreatmentName = Nothing,
      _wcrLimits = Nothing,
      _wcrIsPaused = Nothing,
      _wcrName = Nothing,
      _wcrHoldoutPercent = Nothing,
      _wcrTreatmentDescription = Nothing,
      _wcrMessageConfiguration = Nothing,
      _wcrDescription = Nothing,
      _wcrSegmentId = Nothing,
      _wcrAdditionalTreatments = Nothing,
      _wcrTags = Nothing,
      _wcrSegmentVersion = Nothing
    }

-- | The delivery configuration settings for sending the campaign through a custom channel. This object is required if the MessageConfiguration object for the campaign specifies a CustomMessage object.
wcrCustomDeliveryConfiguration :: Lens' WriteCampaignRequest (Maybe CustomDeliveryConfiguration)
wcrCustomDeliveryConfiguration = lens _wcrCustomDeliveryConfiguration (\s a -> s {_wcrCustomDeliveryConfiguration = a})

-- | The schedule settings for the campaign.
wcrSchedule :: Lens' WriteCampaignRequest (Maybe Schedule)
wcrSchedule = lens _wcrSchedule (\s a -> s {_wcrSchedule = a})

-- | The message template to use for the campaign.
wcrTemplateConfiguration :: Lens' WriteCampaignRequest (Maybe TemplateConfiguration)
wcrTemplateConfiguration = lens _wcrTemplateConfiguration (\s a -> s {_wcrTemplateConfiguration = a})

-- | The settings for the AWS Lambda function to invoke as a code hook for the campaign. You can use this hook to customize the segment that's used by the campaign.
wcrHook :: Lens' WriteCampaignRequest (Maybe CampaignHook)
wcrHook = lens _wcrHook (\s a -> s {_wcrHook = a})

-- | A custom name of the default treatment for the campaign, if the campaign has multiple treatments. A /treatment/ is a variation of a campaign that's used for A/B testing.
wcrTreatmentName :: Lens' WriteCampaignRequest (Maybe Text)
wcrTreatmentName = lens _wcrTreatmentName (\s a -> s {_wcrTreatmentName = a})

-- | The messaging limits for the campaign.
wcrLimits :: Lens' WriteCampaignRequest (Maybe CampaignLimits)
wcrLimits = lens _wcrLimits (\s a -> s {_wcrLimits = a})

-- | Specifies whether to pause the campaign. A paused campaign doesn't run unless you resume it by changing this value to false.
wcrIsPaused :: Lens' WriteCampaignRequest (Maybe Bool)
wcrIsPaused = lens _wcrIsPaused (\s a -> s {_wcrIsPaused = a})

-- | A custom name for the campaign.
wcrName :: Lens' WriteCampaignRequest (Maybe Text)
wcrName = lens _wcrName (\s a -> s {_wcrName = a})

-- | The allocated percentage of users (segment members) who shouldn't receive messages from the campaign.
wcrHoldoutPercent :: Lens' WriteCampaignRequest (Maybe Int)
wcrHoldoutPercent = lens _wcrHoldoutPercent (\s a -> s {_wcrHoldoutPercent = a})

-- | A custom description of the default treatment for the campaign.
wcrTreatmentDescription :: Lens' WriteCampaignRequest (Maybe Text)
wcrTreatmentDescription = lens _wcrTreatmentDescription (\s a -> s {_wcrTreatmentDescription = a})

-- | The message configuration settings for the campaign.
wcrMessageConfiguration :: Lens' WriteCampaignRequest (Maybe MessageConfiguration)
wcrMessageConfiguration = lens _wcrMessageConfiguration (\s a -> s {_wcrMessageConfiguration = a})

-- | A custom description of the campaign.
wcrDescription :: Lens' WriteCampaignRequest (Maybe Text)
wcrDescription = lens _wcrDescription (\s a -> s {_wcrDescription = a})

-- | The unique identifier for the segment to associate with the campaign.
wcrSegmentId :: Lens' WriteCampaignRequest (Maybe Text)
wcrSegmentId = lens _wcrSegmentId (\s a -> s {_wcrSegmentId = a})

-- | An array of requests that defines additional treatments for the campaign, in addition to the default treatment for the campaign.
wcrAdditionalTreatments :: Lens' WriteCampaignRequest [WriteTreatmentResource]
wcrAdditionalTreatments = lens _wcrAdditionalTreatments (\s a -> s {_wcrAdditionalTreatments = a}) . _Default . _Coerce

-- | A string-to-string map of key-value pairs that defines the tags to associate with the campaign. Each tag consists of a required tag key and an associated tag value.
wcrTags :: Lens' WriteCampaignRequest (HashMap Text (Text))
wcrTags = lens _wcrTags (\s a -> s {_wcrTags = a}) . _Default . _Map

-- | The version of the segment to associate with the campaign.
wcrSegmentVersion :: Lens' WriteCampaignRequest (Maybe Int)
wcrSegmentVersion = lens _wcrSegmentVersion (\s a -> s {_wcrSegmentVersion = a})

instance Hashable WriteCampaignRequest

instance NFData WriteCampaignRequest

instance ToJSON WriteCampaignRequest where
  toJSON WriteCampaignRequest' {..} =
    object
      ( catMaybes
          [ ("CustomDeliveryConfiguration" .=)
              <$> _wcrCustomDeliveryConfiguration,
            ("Schedule" .=) <$> _wcrSchedule,
            ("TemplateConfiguration" .=) <$> _wcrTemplateConfiguration,
            ("Hook" .=) <$> _wcrHook,
            ("TreatmentName" .=) <$> _wcrTreatmentName,
            ("Limits" .=) <$> _wcrLimits,
            ("IsPaused" .=) <$> _wcrIsPaused,
            ("Name" .=) <$> _wcrName,
            ("HoldoutPercent" .=) <$> _wcrHoldoutPercent,
            ("TreatmentDescription" .=) <$> _wcrTreatmentDescription,
            ("MessageConfiguration" .=) <$> _wcrMessageConfiguration,
            ("Description" .=) <$> _wcrDescription,
            ("SegmentId" .=) <$> _wcrSegmentId,
            ("AdditionalTreatments" .=) <$> _wcrAdditionalTreatments,
            ("tags" .=) <$> _wcrTags,
            ("SegmentVersion" .=) <$> _wcrSegmentVersion
          ]
      )
