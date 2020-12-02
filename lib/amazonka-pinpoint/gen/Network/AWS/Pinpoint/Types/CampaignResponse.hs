{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CampaignResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.CampaignHook
import Network.AWS.Pinpoint.Types.CampaignLimits
import Network.AWS.Pinpoint.Types.CampaignState
import Network.AWS.Pinpoint.Types.CustomDeliveryConfiguration
import Network.AWS.Pinpoint.Types.MessageConfiguration
import Network.AWS.Pinpoint.Types.Schedule
import Network.AWS.Pinpoint.Types.TemplateConfiguration
import Network.AWS.Pinpoint.Types.TreatmentResource
import Network.AWS.Prelude

-- | Provides information about the status, configuration, and other settings for a campaign.
--
--
--
-- /See:/ 'campaignResponse' smart constructor.
data CampaignResponse = CampaignResponse'
  { _cCustomDeliveryConfiguration ::
      !(Maybe CustomDeliveryConfiguration),
    _cState :: !(Maybe CampaignState),
    _cSchedule :: !(Maybe Schedule),
    _cTemplateConfiguration :: !(Maybe TemplateConfiguration),
    _cHook :: !(Maybe CampaignHook),
    _cTreatmentName :: !(Maybe Text),
    _cLimits :: !(Maybe CampaignLimits),
    _cIsPaused :: !(Maybe Bool),
    _cDefaultState :: !(Maybe CampaignState),
    _cName :: !(Maybe Text),
    _cVersion :: !(Maybe Int),
    _cHoldoutPercent :: !(Maybe Int),
    _cTreatmentDescription :: !(Maybe Text),
    _cMessageConfiguration :: !(Maybe MessageConfiguration),
    _cDescription :: !(Maybe Text),
    _cAdditionalTreatments :: !(Maybe [TreatmentResource]),
    _cTags :: !(Maybe (Map Text (Text))),
    _cLastModifiedDate :: !Text,
    _cCreationDate :: !Text,
    _cSegmentId :: !Text,
    _cSegmentVersion :: !Int,
    _cId :: !Text,
    _cARN :: !Text,
    _cApplicationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CampaignResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCustomDeliveryConfiguration' - The delivery configuration settings for sending the campaign through a custom channel.
--
-- * 'cState' - The current status of the campaign.
--
-- * 'cSchedule' - The schedule settings for the campaign.
--
-- * 'cTemplateConfiguration' - The message template that’s used for the campaign.
--
-- * 'cHook' - The settings for the AWS Lambda function to use as a code hook for the campaign. You can use this hook to customize the segment that's used by the campaign.
--
-- * 'cTreatmentName' - The custom name of the default treatment for the campaign, if the campaign has multiple treatments. A /treatment/ is a variation of a campaign that's used for A/B testing.
--
-- * 'cLimits' - The messaging limits for the campaign.
--
-- * 'cIsPaused' - Specifies whether the campaign is paused. A paused campaign doesn't run unless you resume it by changing this value to false.
--
-- * 'cDefaultState' - The current status of the campaign's default treatment. This value exists only for campaigns that have more than one treatment.
--
-- * 'cName' - The name of the campaign.
--
-- * 'cVersion' - The version number of the campaign.
--
-- * 'cHoldoutPercent' - The allocated percentage of users (segment members) who shouldn't receive messages from the campaign.
--
-- * 'cTreatmentDescription' - The custom description of the default treatment for the campaign.
--
-- * 'cMessageConfiguration' - The message configuration settings for the campaign.
--
-- * 'cDescription' - The custom description of the campaign.
--
-- * 'cAdditionalTreatments' - An array of responses, one for each treatment that you defined for the campaign, in addition to the default treatment.
--
-- * 'cTags' - A string-to-string map of key-value pairs that identifies the tags that are associated with the campaign. Each tag consists of a required tag key and an associated tag value.
--
-- * 'cLastModifiedDate' - The date, in ISO 8601 format, when the campaign was last modified.
--
-- * 'cCreationDate' - The date, in ISO 8601 format, when the campaign was created.
--
-- * 'cSegmentId' - The unique identifier for the segment that's associated with the campaign.
--
-- * 'cSegmentVersion' - The version number of the segment that's associated with the campaign.
--
-- * 'cId' - The unique identifier for the campaign.
--
-- * 'cARN' - The Amazon Resource Name (ARN) of the campaign.
--
-- * 'cApplicationId' - The unique identifier for the application that the campaign applies to.
campaignResponse ::
  -- | 'cLastModifiedDate'
  Text ->
  -- | 'cCreationDate'
  Text ->
  -- | 'cSegmentId'
  Text ->
  -- | 'cSegmentVersion'
  Int ->
  -- | 'cId'
  Text ->
  -- | 'cARN'
  Text ->
  -- | 'cApplicationId'
  Text ->
  CampaignResponse
campaignResponse
  pLastModifiedDate_
  pCreationDate_
  pSegmentId_
  pSegmentVersion_
  pId_
  pARN_
  pApplicationId_ =
    CampaignResponse'
      { _cCustomDeliveryConfiguration = Nothing,
        _cState = Nothing,
        _cSchedule = Nothing,
        _cTemplateConfiguration = Nothing,
        _cHook = Nothing,
        _cTreatmentName = Nothing,
        _cLimits = Nothing,
        _cIsPaused = Nothing,
        _cDefaultState = Nothing,
        _cName = Nothing,
        _cVersion = Nothing,
        _cHoldoutPercent = Nothing,
        _cTreatmentDescription = Nothing,
        _cMessageConfiguration = Nothing,
        _cDescription = Nothing,
        _cAdditionalTreatments = Nothing,
        _cTags = Nothing,
        _cLastModifiedDate = pLastModifiedDate_,
        _cCreationDate = pCreationDate_,
        _cSegmentId = pSegmentId_,
        _cSegmentVersion = pSegmentVersion_,
        _cId = pId_,
        _cARN = pARN_,
        _cApplicationId = pApplicationId_
      }

-- | The delivery configuration settings for sending the campaign through a custom channel.
cCustomDeliveryConfiguration :: Lens' CampaignResponse (Maybe CustomDeliveryConfiguration)
cCustomDeliveryConfiguration = lens _cCustomDeliveryConfiguration (\s a -> s {_cCustomDeliveryConfiguration = a})

-- | The current status of the campaign.
cState :: Lens' CampaignResponse (Maybe CampaignState)
cState = lens _cState (\s a -> s {_cState = a})

-- | The schedule settings for the campaign.
cSchedule :: Lens' CampaignResponse (Maybe Schedule)
cSchedule = lens _cSchedule (\s a -> s {_cSchedule = a})

-- | The message template that’s used for the campaign.
cTemplateConfiguration :: Lens' CampaignResponse (Maybe TemplateConfiguration)
cTemplateConfiguration = lens _cTemplateConfiguration (\s a -> s {_cTemplateConfiguration = a})

-- | The settings for the AWS Lambda function to use as a code hook for the campaign. You can use this hook to customize the segment that's used by the campaign.
cHook :: Lens' CampaignResponse (Maybe CampaignHook)
cHook = lens _cHook (\s a -> s {_cHook = a})

-- | The custom name of the default treatment for the campaign, if the campaign has multiple treatments. A /treatment/ is a variation of a campaign that's used for A/B testing.
cTreatmentName :: Lens' CampaignResponse (Maybe Text)
cTreatmentName = lens _cTreatmentName (\s a -> s {_cTreatmentName = a})

-- | The messaging limits for the campaign.
cLimits :: Lens' CampaignResponse (Maybe CampaignLimits)
cLimits = lens _cLimits (\s a -> s {_cLimits = a})

-- | Specifies whether the campaign is paused. A paused campaign doesn't run unless you resume it by changing this value to false.
cIsPaused :: Lens' CampaignResponse (Maybe Bool)
cIsPaused = lens _cIsPaused (\s a -> s {_cIsPaused = a})

-- | The current status of the campaign's default treatment. This value exists only for campaigns that have more than one treatment.
cDefaultState :: Lens' CampaignResponse (Maybe CampaignState)
cDefaultState = lens _cDefaultState (\s a -> s {_cDefaultState = a})

-- | The name of the campaign.
cName :: Lens' CampaignResponse (Maybe Text)
cName = lens _cName (\s a -> s {_cName = a})

-- | The version number of the campaign.
cVersion :: Lens' CampaignResponse (Maybe Int)
cVersion = lens _cVersion (\s a -> s {_cVersion = a})

-- | The allocated percentage of users (segment members) who shouldn't receive messages from the campaign.
cHoldoutPercent :: Lens' CampaignResponse (Maybe Int)
cHoldoutPercent = lens _cHoldoutPercent (\s a -> s {_cHoldoutPercent = a})

-- | The custom description of the default treatment for the campaign.
cTreatmentDescription :: Lens' CampaignResponse (Maybe Text)
cTreatmentDescription = lens _cTreatmentDescription (\s a -> s {_cTreatmentDescription = a})

-- | The message configuration settings for the campaign.
cMessageConfiguration :: Lens' CampaignResponse (Maybe MessageConfiguration)
cMessageConfiguration = lens _cMessageConfiguration (\s a -> s {_cMessageConfiguration = a})

-- | The custom description of the campaign.
cDescription :: Lens' CampaignResponse (Maybe Text)
cDescription = lens _cDescription (\s a -> s {_cDescription = a})

-- | An array of responses, one for each treatment that you defined for the campaign, in addition to the default treatment.
cAdditionalTreatments :: Lens' CampaignResponse [TreatmentResource]
cAdditionalTreatments = lens _cAdditionalTreatments (\s a -> s {_cAdditionalTreatments = a}) . _Default . _Coerce

-- | A string-to-string map of key-value pairs that identifies the tags that are associated with the campaign. Each tag consists of a required tag key and an associated tag value.
cTags :: Lens' CampaignResponse (HashMap Text (Text))
cTags = lens _cTags (\s a -> s {_cTags = a}) . _Default . _Map

-- | The date, in ISO 8601 format, when the campaign was last modified.
cLastModifiedDate :: Lens' CampaignResponse Text
cLastModifiedDate = lens _cLastModifiedDate (\s a -> s {_cLastModifiedDate = a})

-- | The date, in ISO 8601 format, when the campaign was created.
cCreationDate :: Lens' CampaignResponse Text
cCreationDate = lens _cCreationDate (\s a -> s {_cCreationDate = a})

-- | The unique identifier for the segment that's associated with the campaign.
cSegmentId :: Lens' CampaignResponse Text
cSegmentId = lens _cSegmentId (\s a -> s {_cSegmentId = a})

-- | The version number of the segment that's associated with the campaign.
cSegmentVersion :: Lens' CampaignResponse Int
cSegmentVersion = lens _cSegmentVersion (\s a -> s {_cSegmentVersion = a})

-- | The unique identifier for the campaign.
cId :: Lens' CampaignResponse Text
cId = lens _cId (\s a -> s {_cId = a})

-- | The Amazon Resource Name (ARN) of the campaign.
cARN :: Lens' CampaignResponse Text
cARN = lens _cARN (\s a -> s {_cARN = a})

-- | The unique identifier for the application that the campaign applies to.
cApplicationId :: Lens' CampaignResponse Text
cApplicationId = lens _cApplicationId (\s a -> s {_cApplicationId = a})

instance FromJSON CampaignResponse where
  parseJSON =
    withObject
      "CampaignResponse"
      ( \x ->
          CampaignResponse'
            <$> (x .:? "CustomDeliveryConfiguration")
            <*> (x .:? "State")
            <*> (x .:? "Schedule")
            <*> (x .:? "TemplateConfiguration")
            <*> (x .:? "Hook")
            <*> (x .:? "TreatmentName")
            <*> (x .:? "Limits")
            <*> (x .:? "IsPaused")
            <*> (x .:? "DefaultState")
            <*> (x .:? "Name")
            <*> (x .:? "Version")
            <*> (x .:? "HoldoutPercent")
            <*> (x .:? "TreatmentDescription")
            <*> (x .:? "MessageConfiguration")
            <*> (x .:? "Description")
            <*> (x .:? "AdditionalTreatments" .!= mempty)
            <*> (x .:? "tags" .!= mempty)
            <*> (x .: "LastModifiedDate")
            <*> (x .: "CreationDate")
            <*> (x .: "SegmentId")
            <*> (x .: "SegmentVersion")
            <*> (x .: "Id")
            <*> (x .: "Arn")
            <*> (x .: "ApplicationId")
      )

instance Hashable CampaignResponse

instance NFData CampaignResponse
