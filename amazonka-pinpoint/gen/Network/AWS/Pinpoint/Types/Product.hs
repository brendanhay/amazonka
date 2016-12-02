{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.Product where

import           Network.AWS.Lens
import           Network.AWS.Pinpoint.Types.Sum
import           Network.AWS.Prelude

-- | /See:/ 'apnsChannelRequest' smart constructor.
data APNSChannelRequest = APNSChannelRequest'
    { _acrPrivateKey  :: !(Maybe Text)
    , _acrCertificate :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'APNSChannelRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acrPrivateKey' - The certificate private key.
--
-- * 'acrCertificate' - The distribution certificate from Apple.
apnsChannelRequest
    :: APNSChannelRequest
apnsChannelRequest =
    APNSChannelRequest'
    { _acrPrivateKey = Nothing
    , _acrCertificate = Nothing
    }

-- | The certificate private key.
acrPrivateKey :: Lens' APNSChannelRequest (Maybe Text)
acrPrivateKey = lens _acrPrivateKey (\ s a -> s{_acrPrivateKey = a});

-- | The distribution certificate from Apple.
acrCertificate :: Lens' APNSChannelRequest (Maybe Text)
acrCertificate = lens _acrCertificate (\ s a -> s{_acrCertificate = a});

instance Hashable APNSChannelRequest

instance NFData APNSChannelRequest

instance ToJSON APNSChannelRequest where
        toJSON APNSChannelRequest'{..}
          = object
              (catMaybes
                 [("PrivateKey" .=) <$> _acrPrivateKey,
                  ("Certificate" .=) <$> _acrCertificate])

-- | /See:/ 'apnsChannelResponse' smart constructor.
data APNSChannelResponse = APNSChannelResponse'
    { _acPlatform         :: !(Maybe Text)
    , _acLastModifiedDate :: !(Maybe Text)
    , _acIsArchived       :: !(Maybe Bool)
    , _acApplicationId    :: !(Maybe Text)
    , _acVersion          :: !(Maybe Int)
    , _acId               :: !(Maybe Text)
    , _acCreationDate     :: !(Maybe Text)
    , _acLastModifiedBy   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'APNSChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acPlatform' - The platform type. Will be APNS.
--
-- * 'acLastModifiedDate' - Last date this was updated
--
-- * 'acIsArchived' - Is this channel archived
--
-- * 'acApplicationId' - Undocumented member.
--
-- * 'acVersion' - Version of channel
--
-- * 'acId' - Undocumented member.
--
-- * 'acCreationDate' - When was this segment created
--
-- * 'acLastModifiedBy' - Who last updated this entry
apnsChannelResponse
    :: APNSChannelResponse
apnsChannelResponse =
    APNSChannelResponse'
    { _acPlatform = Nothing
    , _acLastModifiedDate = Nothing
    , _acIsArchived = Nothing
    , _acApplicationId = Nothing
    , _acVersion = Nothing
    , _acId = Nothing
    , _acCreationDate = Nothing
    , _acLastModifiedBy = Nothing
    }

-- | The platform type. Will be APNS.
acPlatform :: Lens' APNSChannelResponse (Maybe Text)
acPlatform = lens _acPlatform (\ s a -> s{_acPlatform = a});

-- | Last date this was updated
acLastModifiedDate :: Lens' APNSChannelResponse (Maybe Text)
acLastModifiedDate = lens _acLastModifiedDate (\ s a -> s{_acLastModifiedDate = a});

-- | Is this channel archived
acIsArchived :: Lens' APNSChannelResponse (Maybe Bool)
acIsArchived = lens _acIsArchived (\ s a -> s{_acIsArchived = a});

-- | Undocumented member.
acApplicationId :: Lens' APNSChannelResponse (Maybe Text)
acApplicationId = lens _acApplicationId (\ s a -> s{_acApplicationId = a});

-- | Version of channel
acVersion :: Lens' APNSChannelResponse (Maybe Int)
acVersion = lens _acVersion (\ s a -> s{_acVersion = a});

-- | Undocumented member.
acId :: Lens' APNSChannelResponse (Maybe Text)
acId = lens _acId (\ s a -> s{_acId = a});

-- | When was this segment created
acCreationDate :: Lens' APNSChannelResponse (Maybe Text)
acCreationDate = lens _acCreationDate (\ s a -> s{_acCreationDate = a});

-- | Who last updated this entry
acLastModifiedBy :: Lens' APNSChannelResponse (Maybe Text)
acLastModifiedBy = lens _acLastModifiedBy (\ s a -> s{_acLastModifiedBy = a});

instance FromJSON APNSChannelResponse where
        parseJSON
          = withObject "APNSChannelResponse"
              (\ x ->
                 APNSChannelResponse' <$>
                   (x .:? "Platform") <*> (x .:? "LastModifiedDate") <*>
                     (x .:? "IsArchived")
                     <*> (x .:? "ApplicationId")
                     <*> (x .:? "Version")
                     <*> (x .:? "Id")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "LastModifiedBy"))

instance Hashable APNSChannelResponse

instance NFData APNSChannelResponse

-- | /See:/ 'activitiesResponse' smart constructor.
newtype ActivitiesResponse = ActivitiesResponse'
    { _aItem :: Maybe [ActivityResponse]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ActivitiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aItem' - List of campaign activities
activitiesResponse
    :: ActivitiesResponse
activitiesResponse =
    ActivitiesResponse'
    { _aItem = Nothing
    }

-- | List of campaign activities
aItem :: Lens' ActivitiesResponse [ActivityResponse]
aItem = lens _aItem (\ s a -> s{_aItem = a}) . _Default . _Coerce;

instance FromJSON ActivitiesResponse where
        parseJSON
          = withObject "ActivitiesResponse"
              (\ x ->
                 ActivitiesResponse' <$> (x .:? "Item" .!= mempty))

instance Hashable ActivitiesResponse

instance NFData ActivitiesResponse

-- | /See:/ 'activityResponse' smart constructor.
data ActivityResponse = ActivityResponse'
    { _aState                   :: !(Maybe Text)
    , _aStart                   :: !(Maybe Text)
    , _aCampaignId              :: !(Maybe Text)
    , _aResult                  :: !(Maybe Text)
    , _aTreatmentId             :: !(Maybe Text)
    , _aSuccessfulEndpointCount :: !(Maybe Int)
    , _aEnd                     :: !(Maybe Text)
    , _aApplicationId           :: !(Maybe Text)
    , _aTotalEndpointCount      :: !(Maybe Int)
    , _aId                      :: !(Maybe Text)
    , _aScheduledStart          :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ActivityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aState' - The state of the activity. Valid values: PENDING, INITIALIZING, RUNNING, PAUSED, CANCELLED, COMPLETED
--
-- * 'aStart' - The actual start time of the activity in ISO 8601 format.
--
-- * 'aCampaignId' - The ID of the campaign to which the activity applies.
--
-- * 'aResult' - Indicates whether the activity succeeded. Valid values: SUCCESS, FAIL
--
-- * 'aTreatmentId' - The ID of a variation of the campaign used for A/B testing.
--
-- * 'aSuccessfulEndpointCount' - The total number of endpoints to which the campaign successfully delivered messages.
--
-- * 'aEnd' - The actual time the activity was marked CANCELLED or COMPLETED. Provided in ISO 8601 format.
--
-- * 'aApplicationId' - The ID of the application to which the campaign applies.
--
-- * 'aTotalEndpointCount' - The total number of endpoints to which the campaign attempts to deliver messages.
--
-- * 'aId' - The unique activity ID.
--
-- * 'aScheduledStart' - The scheduled start time for the activity in ISO 8601 format.
activityResponse
    :: ActivityResponse
activityResponse =
    ActivityResponse'
    { _aState = Nothing
    , _aStart = Nothing
    , _aCampaignId = Nothing
    , _aResult = Nothing
    , _aTreatmentId = Nothing
    , _aSuccessfulEndpointCount = Nothing
    , _aEnd = Nothing
    , _aApplicationId = Nothing
    , _aTotalEndpointCount = Nothing
    , _aId = Nothing
    , _aScheduledStart = Nothing
    }

-- | The state of the activity. Valid values: PENDING, INITIALIZING, RUNNING, PAUSED, CANCELLED, COMPLETED
aState :: Lens' ActivityResponse (Maybe Text)
aState = lens _aState (\ s a -> s{_aState = a});

-- | The actual start time of the activity in ISO 8601 format.
aStart :: Lens' ActivityResponse (Maybe Text)
aStart = lens _aStart (\ s a -> s{_aStart = a});

-- | The ID of the campaign to which the activity applies.
aCampaignId :: Lens' ActivityResponse (Maybe Text)
aCampaignId = lens _aCampaignId (\ s a -> s{_aCampaignId = a});

-- | Indicates whether the activity succeeded. Valid values: SUCCESS, FAIL
aResult :: Lens' ActivityResponse (Maybe Text)
aResult = lens _aResult (\ s a -> s{_aResult = a});

-- | The ID of a variation of the campaign used for A/B testing.
aTreatmentId :: Lens' ActivityResponse (Maybe Text)
aTreatmentId = lens _aTreatmentId (\ s a -> s{_aTreatmentId = a});

-- | The total number of endpoints to which the campaign successfully delivered messages.
aSuccessfulEndpointCount :: Lens' ActivityResponse (Maybe Int)
aSuccessfulEndpointCount = lens _aSuccessfulEndpointCount (\ s a -> s{_aSuccessfulEndpointCount = a});

-- | The actual time the activity was marked CANCELLED or COMPLETED. Provided in ISO 8601 format.
aEnd :: Lens' ActivityResponse (Maybe Text)
aEnd = lens _aEnd (\ s a -> s{_aEnd = a});

-- | The ID of the application to which the campaign applies.
aApplicationId :: Lens' ActivityResponse (Maybe Text)
aApplicationId = lens _aApplicationId (\ s a -> s{_aApplicationId = a});

-- | The total number of endpoints to which the campaign attempts to deliver messages.
aTotalEndpointCount :: Lens' ActivityResponse (Maybe Int)
aTotalEndpointCount = lens _aTotalEndpointCount (\ s a -> s{_aTotalEndpointCount = a});

-- | The unique activity ID.
aId :: Lens' ActivityResponse (Maybe Text)
aId = lens _aId (\ s a -> s{_aId = a});

-- | The scheduled start time for the activity in ISO 8601 format.
aScheduledStart :: Lens' ActivityResponse (Maybe Text)
aScheduledStart = lens _aScheduledStart (\ s a -> s{_aScheduledStart = a});

instance FromJSON ActivityResponse where
        parseJSON
          = withObject "ActivityResponse"
              (\ x ->
                 ActivityResponse' <$>
                   (x .:? "State") <*> (x .:? "Start") <*>
                     (x .:? "CampaignId")
                     <*> (x .:? "Result")
                     <*> (x .:? "TreatmentId")
                     <*> (x .:? "SuccessfulEndpointCount")
                     <*> (x .:? "End")
                     <*> (x .:? "ApplicationId")
                     <*> (x .:? "TotalEndpointCount")
                     <*> (x .:? "Id")
                     <*> (x .:? "ScheduledStart"))

instance Hashable ActivityResponse

instance NFData ActivityResponse

-- | /See:/ 'applicationSettingsResource' smart constructor.
data ApplicationSettingsResource = ApplicationSettingsResource'
    { _asrLastModifiedDate :: !(Maybe Text)
    , _asrLimits           :: !(Maybe CampaignLimits)
    , _asrQuietTime        :: !(Maybe QuietTime)
    , _asrApplicationId    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ApplicationSettingsResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asrLastModifiedDate' - The date that the settings were last updated in ISO 8601 format.
--
-- * 'asrLimits' - The default campaign limits for the app. These limits apply to each campaign for the app, unless the campaign overrides the default with limits of its own.
--
-- * 'asrQuietTime' - The default quiet time for the app. Each campaign for this app sends no messages during this time unless the campaign overrides the default with a quiet time of its own.
--
-- * 'asrApplicationId' - The unique ID for the application.
applicationSettingsResource
    :: ApplicationSettingsResource
applicationSettingsResource =
    ApplicationSettingsResource'
    { _asrLastModifiedDate = Nothing
    , _asrLimits = Nothing
    , _asrQuietTime = Nothing
    , _asrApplicationId = Nothing
    }

-- | The date that the settings were last updated in ISO 8601 format.
asrLastModifiedDate :: Lens' ApplicationSettingsResource (Maybe Text)
asrLastModifiedDate = lens _asrLastModifiedDate (\ s a -> s{_asrLastModifiedDate = a});

-- | The default campaign limits for the app. These limits apply to each campaign for the app, unless the campaign overrides the default with limits of its own.
asrLimits :: Lens' ApplicationSettingsResource (Maybe CampaignLimits)
asrLimits = lens _asrLimits (\ s a -> s{_asrLimits = a});

-- | The default quiet time for the app. Each campaign for this app sends no messages during this time unless the campaign overrides the default with a quiet time of its own.
asrQuietTime :: Lens' ApplicationSettingsResource (Maybe QuietTime)
asrQuietTime = lens _asrQuietTime (\ s a -> s{_asrQuietTime = a});

-- | The unique ID for the application.
asrApplicationId :: Lens' ApplicationSettingsResource (Maybe Text)
asrApplicationId = lens _asrApplicationId (\ s a -> s{_asrApplicationId = a});

instance FromJSON ApplicationSettingsResource where
        parseJSON
          = withObject "ApplicationSettingsResource"
              (\ x ->
                 ApplicationSettingsResource' <$>
                   (x .:? "LastModifiedDate") <*> (x .:? "Limits") <*>
                     (x .:? "QuietTime")
                     <*> (x .:? "ApplicationId"))

instance Hashable ApplicationSettingsResource

instance NFData ApplicationSettingsResource

-- | /See:/ 'attributeDimension' smart constructor.
data AttributeDimension = AttributeDimension'
    { _adValues        :: !(Maybe [Text])
    , _adAttributeType :: !(Maybe AttributeType)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AttributeDimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adValues' - The criteria values for the segment dimension. Endpoints with matching attribute values are included or excluded from the segment, depending on the setting for Type.
--
-- * 'adAttributeType' - The type of dimension: INCLUSIVE – Endpoints that match the criteria are included in the segment. EXCLUSIVE – Endpoints that match the criteria are excluded from the segment.
attributeDimension
    :: AttributeDimension
attributeDimension =
    AttributeDimension'
    { _adValues = Nothing
    , _adAttributeType = Nothing
    }

-- | The criteria values for the segment dimension. Endpoints with matching attribute values are included or excluded from the segment, depending on the setting for Type.
adValues :: Lens' AttributeDimension [Text]
adValues = lens _adValues (\ s a -> s{_adValues = a}) . _Default . _Coerce;

-- | The type of dimension: INCLUSIVE – Endpoints that match the criteria are included in the segment. EXCLUSIVE – Endpoints that match the criteria are excluded from the segment.
adAttributeType :: Lens' AttributeDimension (Maybe AttributeType)
adAttributeType = lens _adAttributeType (\ s a -> s{_adAttributeType = a});

instance FromJSON AttributeDimension where
        parseJSON
          = withObject "AttributeDimension"
              (\ x ->
                 AttributeDimension' <$>
                   (x .:? "Values" .!= mempty) <*>
                     (x .:? "AttributeType"))

instance Hashable AttributeDimension

instance NFData AttributeDimension

instance ToJSON AttributeDimension where
        toJSON AttributeDimension'{..}
          = object
              (catMaybes
                 [("Values" .=) <$> _adValues,
                  ("AttributeType" .=) <$> _adAttributeType])

-- | /See:/ 'campaignLimits' smart constructor.
data CampaignLimits = CampaignLimits'
    { _clDaily :: !(Maybe Int)
    , _clTotal :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CampaignLimits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clDaily' - The maximum number of messages that the campaign can send daily.
--
-- * 'clTotal' - The maximum total number of messages that the campaign can send.
campaignLimits
    :: CampaignLimits
campaignLimits =
    CampaignLimits'
    { _clDaily = Nothing
    , _clTotal = Nothing
    }

-- | The maximum number of messages that the campaign can send daily.
clDaily :: Lens' CampaignLimits (Maybe Int)
clDaily = lens _clDaily (\ s a -> s{_clDaily = a});

-- | The maximum total number of messages that the campaign can send.
clTotal :: Lens' CampaignLimits (Maybe Int)
clTotal = lens _clTotal (\ s a -> s{_clTotal = a});

instance FromJSON CampaignLimits where
        parseJSON
          = withObject "CampaignLimits"
              (\ x ->
                 CampaignLimits' <$>
                   (x .:? "Daily") <*> (x .:? "Total"))

instance Hashable CampaignLimits

instance NFData CampaignLimits

instance ToJSON CampaignLimits where
        toJSON CampaignLimits'{..}
          = object
              (catMaybes
                 [("Daily" .=) <$> _clDaily,
                  ("Total" .=) <$> _clTotal])

-- | /See:/ 'campaignResponse' smart constructor.
data CampaignResponse = CampaignResponse'
    { _cState                :: !(Maybe CampaignState)
    , _cLastModifiedDate     :: !(Maybe Text)
    , _cSchedule             :: !(Maybe Schedule)
    , _cTreatmentName        :: !(Maybe Text)
    , _cLimits               :: !(Maybe CampaignLimits)
    , _cIsPaused             :: !(Maybe Bool)
    , _cDefaultState         :: !(Maybe CampaignState)
    , _cApplicationId        :: !(Maybe Text)
    , _cName                 :: !(Maybe Text)
    , _cVersion              :: !(Maybe Int)
    , _cHoldoutPercent       :: !(Maybe Int)
    , _cTreatmentDescription :: !(Maybe Text)
    , _cId                   :: !(Maybe Text)
    , _cCreationDate         :: !(Maybe Text)
    , _cMessageConfiguration :: !(Maybe MessageConfiguration)
    , _cDescription          :: !(Maybe Text)
    , _cSegmentId            :: !(Maybe Text)
    , _cAdditionalTreatments :: !(Maybe [TreatmentResource])
    , _cSegmentVersion       :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CampaignResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cState' - The campaign status. An A/B test campaign will have a status of COMPLETED only when all treatments have a status of COMPLETED.
--
-- * 'cLastModifiedDate' - The date the campaign was last updated in ISO 8601 format.
--
-- * 'cSchedule' - The campaign schedule.
--
-- * 'cTreatmentName' - The custom name of a variation of the campaign used for A/B testing.
--
-- * 'cLimits' - The campaign limits settings.
--
-- * 'cIsPaused' - Indicates whether the campaign is paused. A paused campaign does not send messages unless you resume it by setting IsPaused to false.
--
-- * 'cDefaultState' - The status of the campaign's default treatment. Only present for A/B test campaigns.
--
-- * 'cApplicationId' - The ID of the application to which the campaign applies.
--
-- * 'cName' - The custom name of the campaign.
--
-- * 'cVersion' - The campaign version number.
--
-- * 'cHoldoutPercent' - The allocated percentage of end users who will not receive messages from this campaign.
--
-- * 'cTreatmentDescription' - A custom description for the treatment.
--
-- * 'cId' - The unique campaign ID.
--
-- * 'cCreationDate' - The date the campaign was created in ISO 8601 format.
--
-- * 'cMessageConfiguration' - The message configuration settings.
--
-- * 'cDescription' - A description of the campaign.
--
-- * 'cSegmentId' - The ID of the segment to which the campaign sends messages.
--
-- * 'cAdditionalTreatments' - Treatments that are defined in addition to the default treatment.
--
-- * 'cSegmentVersion' - The version of the segment to which the campaign sends messages.
campaignResponse
    :: CampaignResponse
campaignResponse =
    CampaignResponse'
    { _cState = Nothing
    , _cLastModifiedDate = Nothing
    , _cSchedule = Nothing
    , _cTreatmentName = Nothing
    , _cLimits = Nothing
    , _cIsPaused = Nothing
    , _cDefaultState = Nothing
    , _cApplicationId = Nothing
    , _cName = Nothing
    , _cVersion = Nothing
    , _cHoldoutPercent = Nothing
    , _cTreatmentDescription = Nothing
    , _cId = Nothing
    , _cCreationDate = Nothing
    , _cMessageConfiguration = Nothing
    , _cDescription = Nothing
    , _cSegmentId = Nothing
    , _cAdditionalTreatments = Nothing
    , _cSegmentVersion = Nothing
    }

-- | The campaign status. An A/B test campaign will have a status of COMPLETED only when all treatments have a status of COMPLETED.
cState :: Lens' CampaignResponse (Maybe CampaignState)
cState = lens _cState (\ s a -> s{_cState = a});

-- | The date the campaign was last updated in ISO 8601 format.
cLastModifiedDate :: Lens' CampaignResponse (Maybe Text)
cLastModifiedDate = lens _cLastModifiedDate (\ s a -> s{_cLastModifiedDate = a});

-- | The campaign schedule.
cSchedule :: Lens' CampaignResponse (Maybe Schedule)
cSchedule = lens _cSchedule (\ s a -> s{_cSchedule = a});

-- | The custom name of a variation of the campaign used for A/B testing.
cTreatmentName :: Lens' CampaignResponse (Maybe Text)
cTreatmentName = lens _cTreatmentName (\ s a -> s{_cTreatmentName = a});

-- | The campaign limits settings.
cLimits :: Lens' CampaignResponse (Maybe CampaignLimits)
cLimits = lens _cLimits (\ s a -> s{_cLimits = a});

-- | Indicates whether the campaign is paused. A paused campaign does not send messages unless you resume it by setting IsPaused to false.
cIsPaused :: Lens' CampaignResponse (Maybe Bool)
cIsPaused = lens _cIsPaused (\ s a -> s{_cIsPaused = a});

-- | The status of the campaign's default treatment. Only present for A/B test campaigns.
cDefaultState :: Lens' CampaignResponse (Maybe CampaignState)
cDefaultState = lens _cDefaultState (\ s a -> s{_cDefaultState = a});

-- | The ID of the application to which the campaign applies.
cApplicationId :: Lens' CampaignResponse (Maybe Text)
cApplicationId = lens _cApplicationId (\ s a -> s{_cApplicationId = a});

-- | The custom name of the campaign.
cName :: Lens' CampaignResponse (Maybe Text)
cName = lens _cName (\ s a -> s{_cName = a});

-- | The campaign version number.
cVersion :: Lens' CampaignResponse (Maybe Int)
cVersion = lens _cVersion (\ s a -> s{_cVersion = a});

-- | The allocated percentage of end users who will not receive messages from this campaign.
cHoldoutPercent :: Lens' CampaignResponse (Maybe Int)
cHoldoutPercent = lens _cHoldoutPercent (\ s a -> s{_cHoldoutPercent = a});

-- | A custom description for the treatment.
cTreatmentDescription :: Lens' CampaignResponse (Maybe Text)
cTreatmentDescription = lens _cTreatmentDescription (\ s a -> s{_cTreatmentDescription = a});

-- | The unique campaign ID.
cId :: Lens' CampaignResponse (Maybe Text)
cId = lens _cId (\ s a -> s{_cId = a});

-- | The date the campaign was created in ISO 8601 format.
cCreationDate :: Lens' CampaignResponse (Maybe Text)
cCreationDate = lens _cCreationDate (\ s a -> s{_cCreationDate = a});

-- | The message configuration settings.
cMessageConfiguration :: Lens' CampaignResponse (Maybe MessageConfiguration)
cMessageConfiguration = lens _cMessageConfiguration (\ s a -> s{_cMessageConfiguration = a});

-- | A description of the campaign.
cDescription :: Lens' CampaignResponse (Maybe Text)
cDescription = lens _cDescription (\ s a -> s{_cDescription = a});

-- | The ID of the segment to which the campaign sends messages.
cSegmentId :: Lens' CampaignResponse (Maybe Text)
cSegmentId = lens _cSegmentId (\ s a -> s{_cSegmentId = a});

-- | Treatments that are defined in addition to the default treatment.
cAdditionalTreatments :: Lens' CampaignResponse [TreatmentResource]
cAdditionalTreatments = lens _cAdditionalTreatments (\ s a -> s{_cAdditionalTreatments = a}) . _Default . _Coerce;

-- | The version of the segment to which the campaign sends messages.
cSegmentVersion :: Lens' CampaignResponse (Maybe Int)
cSegmentVersion = lens _cSegmentVersion (\ s a -> s{_cSegmentVersion = a});

instance FromJSON CampaignResponse where
        parseJSON
          = withObject "CampaignResponse"
              (\ x ->
                 CampaignResponse' <$>
                   (x .:? "State") <*> (x .:? "LastModifiedDate") <*>
                     (x .:? "Schedule")
                     <*> (x .:? "TreatmentName")
                     <*> (x .:? "Limits")
                     <*> (x .:? "IsPaused")
                     <*> (x .:? "DefaultState")
                     <*> (x .:? "ApplicationId")
                     <*> (x .:? "Name")
                     <*> (x .:? "Version")
                     <*> (x .:? "HoldoutPercent")
                     <*> (x .:? "TreatmentDescription")
                     <*> (x .:? "Id")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "MessageConfiguration")
                     <*> (x .:? "Description")
                     <*> (x .:? "SegmentId")
                     <*> (x .:? "AdditionalTreatments" .!= mempty)
                     <*> (x .:? "SegmentVersion"))

instance Hashable CampaignResponse

instance NFData CampaignResponse

-- | /See:/ 'campaignState' smart constructor.
newtype CampaignState = CampaignState'
    { _csCampaignStatus :: Maybe CampaignStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CampaignState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csCampaignStatus' - The status of the campaign, or the status of a treatment that belongs to an A/B test campaign. Valid values: SCHEDULED, EXECUTING, PENDING_NEXT_RUN, COMPLETED, PAUSED
campaignState
    :: CampaignState
campaignState =
    CampaignState'
    { _csCampaignStatus = Nothing
    }

-- | The status of the campaign, or the status of a treatment that belongs to an A/B test campaign. Valid values: SCHEDULED, EXECUTING, PENDING_NEXT_RUN, COMPLETED, PAUSED
csCampaignStatus :: Lens' CampaignState (Maybe CampaignStatus)
csCampaignStatus = lens _csCampaignStatus (\ s a -> s{_csCampaignStatus = a});

instance FromJSON CampaignState where
        parseJSON
          = withObject "CampaignState"
              (\ x -> CampaignState' <$> (x .:? "CampaignStatus"))

instance Hashable CampaignState

instance NFData CampaignState

-- | /See:/ 'campaignsResponse' smart constructor.
data CampaignsResponse = CampaignsResponse'
    { _cNextToken :: !(Maybe Text)
    , _cItem      :: !(Maybe [CampaignResponse])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CampaignsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cNextToken' - The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- * 'cItem' - A list of campaigns.
campaignsResponse
    :: CampaignsResponse
campaignsResponse =
    CampaignsResponse'
    { _cNextToken = Nothing
    , _cItem = Nothing
    }

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
cNextToken :: Lens' CampaignsResponse (Maybe Text)
cNextToken = lens _cNextToken (\ s a -> s{_cNextToken = a});

-- | A list of campaigns.
cItem :: Lens' CampaignsResponse [CampaignResponse]
cItem = lens _cItem (\ s a -> s{_cItem = a}) . _Default . _Coerce;

instance FromJSON CampaignsResponse where
        parseJSON
          = withObject "CampaignsResponse"
              (\ x ->
                 CampaignsResponse' <$>
                   (x .:? "NextToken") <*> (x .:? "Item" .!= mempty))

instance Hashable CampaignsResponse

instance NFData CampaignsResponse

-- | /See:/ 'endpointBatchItem' smart constructor.
data EndpointBatchItem = EndpointBatchItem'
    { _ebiRequestId      :: !(Maybe Text)
    , _ebiMetrics        :: !(Maybe (Map Text Double))
    , _ebiLocation       :: !(Maybe EndpointLocation)
    , _ebiDemographic    :: !(Maybe EndpointDemographic)
    , _ebiAddress        :: !(Maybe Text)
    , _ebiEffectiveDate  :: !(Maybe Text)
    , _ebiUser           :: !(Maybe EndpointUser)
    , _ebiAttributes     :: !(Maybe (Map Text [Text]))
    , _ebiEndpointStatus :: !(Maybe Text)
    , _ebiOptOut         :: !(Maybe Text)
    , _ebiId             :: !(Maybe Text)
    , _ebiChannelType    :: !(Maybe ChannelType)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EndpointBatchItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebiRequestId' - The unique ID for the most recent request to update the endpoint.
--
-- * 'ebiMetrics' - Custom metrics that your app reports to Amazon Pinpoint.
--
-- * 'ebiLocation' - The endpoint location attributes.
--
-- * 'ebiDemographic' - The endpoint demographic attributes.
--
-- * 'ebiAddress' - The address or token of the endpoint.
--
-- * 'ebiEffectiveDate' - The last time the endpoint was updated. Provided in ISO 8601 format.
--
-- * 'ebiUser' - Custom user-specific attributes that your app reports to Amazon Pinpoint.
--
-- * 'ebiAttributes' - Custom attributes that your app reports to Amazon Pinpoint. You can use these attributes as selection criteria when you create a segment.
--
-- * 'ebiEndpointStatus' - The endpoint status. Can be either ACTIVE or INACTIVE. Will be set to INACTIVE if a delivery fails. Will be set to ACTIVE if the address is updated.
--
-- * 'ebiOptOut' - Indicates whether a user has opted out of receiving messages with one of the following values: ALL – User receives all messages. NONE – User receives no messages.
--
-- * 'ebiId' - Undocumented member.
--
-- * 'ebiChannelType' - The channel type. Valid values: APNS, GCM
endpointBatchItem
    :: EndpointBatchItem
endpointBatchItem =
    EndpointBatchItem'
    { _ebiRequestId = Nothing
    , _ebiMetrics = Nothing
    , _ebiLocation = Nothing
    , _ebiDemographic = Nothing
    , _ebiAddress = Nothing
    , _ebiEffectiveDate = Nothing
    , _ebiUser = Nothing
    , _ebiAttributes = Nothing
    , _ebiEndpointStatus = Nothing
    , _ebiOptOut = Nothing
    , _ebiId = Nothing
    , _ebiChannelType = Nothing
    }

-- | The unique ID for the most recent request to update the endpoint.
ebiRequestId :: Lens' EndpointBatchItem (Maybe Text)
ebiRequestId = lens _ebiRequestId (\ s a -> s{_ebiRequestId = a});

-- | Custom metrics that your app reports to Amazon Pinpoint.
ebiMetrics :: Lens' EndpointBatchItem (HashMap Text Double)
ebiMetrics = lens _ebiMetrics (\ s a -> s{_ebiMetrics = a}) . _Default . _Map;

-- | The endpoint location attributes.
ebiLocation :: Lens' EndpointBatchItem (Maybe EndpointLocation)
ebiLocation = lens _ebiLocation (\ s a -> s{_ebiLocation = a});

-- | The endpoint demographic attributes.
ebiDemographic :: Lens' EndpointBatchItem (Maybe EndpointDemographic)
ebiDemographic = lens _ebiDemographic (\ s a -> s{_ebiDemographic = a});

-- | The address or token of the endpoint.
ebiAddress :: Lens' EndpointBatchItem (Maybe Text)
ebiAddress = lens _ebiAddress (\ s a -> s{_ebiAddress = a});

-- | The last time the endpoint was updated. Provided in ISO 8601 format.
ebiEffectiveDate :: Lens' EndpointBatchItem (Maybe Text)
ebiEffectiveDate = lens _ebiEffectiveDate (\ s a -> s{_ebiEffectiveDate = a});

-- | Custom user-specific attributes that your app reports to Amazon Pinpoint.
ebiUser :: Lens' EndpointBatchItem (Maybe EndpointUser)
ebiUser = lens _ebiUser (\ s a -> s{_ebiUser = a});

-- | Custom attributes that your app reports to Amazon Pinpoint. You can use these attributes as selection criteria when you create a segment.
ebiAttributes :: Lens' EndpointBatchItem (HashMap Text [Text])
ebiAttributes = lens _ebiAttributes (\ s a -> s{_ebiAttributes = a}) . _Default . _Map;

-- | The endpoint status. Can be either ACTIVE or INACTIVE. Will be set to INACTIVE if a delivery fails. Will be set to ACTIVE if the address is updated.
ebiEndpointStatus :: Lens' EndpointBatchItem (Maybe Text)
ebiEndpointStatus = lens _ebiEndpointStatus (\ s a -> s{_ebiEndpointStatus = a});

-- | Indicates whether a user has opted out of receiving messages with one of the following values: ALL – User receives all messages. NONE – User receives no messages.
ebiOptOut :: Lens' EndpointBatchItem (Maybe Text)
ebiOptOut = lens _ebiOptOut (\ s a -> s{_ebiOptOut = a});

-- | Undocumented member.
ebiId :: Lens' EndpointBatchItem (Maybe Text)
ebiId = lens _ebiId (\ s a -> s{_ebiId = a});

-- | The channel type. Valid values: APNS, GCM
ebiChannelType :: Lens' EndpointBatchItem (Maybe ChannelType)
ebiChannelType = lens _ebiChannelType (\ s a -> s{_ebiChannelType = a});

instance Hashable EndpointBatchItem

instance NFData EndpointBatchItem

instance ToJSON EndpointBatchItem where
        toJSON EndpointBatchItem'{..}
          = object
              (catMaybes
                 [("RequestId" .=) <$> _ebiRequestId,
                  ("Metrics" .=) <$> _ebiMetrics,
                  ("Location" .=) <$> _ebiLocation,
                  ("Demographic" .=) <$> _ebiDemographic,
                  ("Address" .=) <$> _ebiAddress,
                  ("EffectiveDate" .=) <$> _ebiEffectiveDate,
                  ("User" .=) <$> _ebiUser,
                  ("Attributes" .=) <$> _ebiAttributes,
                  ("EndpointStatus" .=) <$> _ebiEndpointStatus,
                  ("OptOut" .=) <$> _ebiOptOut, ("Id" .=) <$> _ebiId,
                  ("ChannelType" .=) <$> _ebiChannelType])

-- | /See:/ 'endpointBatchRequest' smart constructor.
newtype EndpointBatchRequest = EndpointBatchRequest'
    { _ebrItem :: Maybe [EndpointBatchItem]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EndpointBatchRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebrItem' - List of items to update. Maximum 100 items
endpointBatchRequest
    :: EndpointBatchRequest
endpointBatchRequest =
    EndpointBatchRequest'
    { _ebrItem = Nothing
    }

-- | List of items to update. Maximum 100 items
ebrItem :: Lens' EndpointBatchRequest [EndpointBatchItem]
ebrItem = lens _ebrItem (\ s a -> s{_ebrItem = a}) . _Default . _Coerce;

instance Hashable EndpointBatchRequest

instance NFData EndpointBatchRequest

instance ToJSON EndpointBatchRequest where
        toJSON EndpointBatchRequest'{..}
          = object (catMaybes [("Item" .=) <$> _ebrItem])

-- | /See:/ 'endpointDemographic' smart constructor.
data EndpointDemographic = EndpointDemographic'
    { _edPlatform        :: !(Maybe Text)
    , _edPlatformVersion :: !(Maybe Text)
    , _edLocale          :: !(Maybe Text)
    , _edAppVersion      :: !(Maybe Text)
    , _edModel           :: !(Maybe Text)
    , _edMake            :: !(Maybe Text)
    , _edModelVersion    :: !(Maybe Text)
    , _edTimezone        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EndpointDemographic' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edPlatform' - The endpoint platform, such as ios or android.
--
-- * 'edPlatformVersion' - The endpoint platform version.
--
-- * 'edLocale' - The endpoint locale in the following format: The ISO 639-1 alpha-2 code, followed by an underscore, followed by an ISO 3166-1 alpha-2 value.
--
-- * 'edAppVersion' - The version of the application associated with the endpoint.
--
-- * 'edModel' - The endpoint model, such as iPhone.
--
-- * 'edMake' - The endpoint make, such as such as Apple or Samsung.
--
-- * 'edModelVersion' - The endpoint model version.
--
-- * 'edTimezone' - The timezone of the endpoint. Specified as a tz database value, such as Americas/Los_Angeles.
endpointDemographic
    :: EndpointDemographic
endpointDemographic =
    EndpointDemographic'
    { _edPlatform = Nothing
    , _edPlatformVersion = Nothing
    , _edLocale = Nothing
    , _edAppVersion = Nothing
    , _edModel = Nothing
    , _edMake = Nothing
    , _edModelVersion = Nothing
    , _edTimezone = Nothing
    }

-- | The endpoint platform, such as ios or android.
edPlatform :: Lens' EndpointDemographic (Maybe Text)
edPlatform = lens _edPlatform (\ s a -> s{_edPlatform = a});

-- | The endpoint platform version.
edPlatformVersion :: Lens' EndpointDemographic (Maybe Text)
edPlatformVersion = lens _edPlatformVersion (\ s a -> s{_edPlatformVersion = a});

-- | The endpoint locale in the following format: The ISO 639-1 alpha-2 code, followed by an underscore, followed by an ISO 3166-1 alpha-2 value.
edLocale :: Lens' EndpointDemographic (Maybe Text)
edLocale = lens _edLocale (\ s a -> s{_edLocale = a});

-- | The version of the application associated with the endpoint.
edAppVersion :: Lens' EndpointDemographic (Maybe Text)
edAppVersion = lens _edAppVersion (\ s a -> s{_edAppVersion = a});

-- | The endpoint model, such as iPhone.
edModel :: Lens' EndpointDemographic (Maybe Text)
edModel = lens _edModel (\ s a -> s{_edModel = a});

-- | The endpoint make, such as such as Apple or Samsung.
edMake :: Lens' EndpointDemographic (Maybe Text)
edMake = lens _edMake (\ s a -> s{_edMake = a});

-- | The endpoint model version.
edModelVersion :: Lens' EndpointDemographic (Maybe Text)
edModelVersion = lens _edModelVersion (\ s a -> s{_edModelVersion = a});

-- | The timezone of the endpoint. Specified as a tz database value, such as Americas/Los_Angeles.
edTimezone :: Lens' EndpointDemographic (Maybe Text)
edTimezone = lens _edTimezone (\ s a -> s{_edTimezone = a});

instance FromJSON EndpointDemographic where
        parseJSON
          = withObject "EndpointDemographic"
              (\ x ->
                 EndpointDemographic' <$>
                   (x .:? "Platform") <*> (x .:? "PlatformVersion") <*>
                     (x .:? "Locale")
                     <*> (x .:? "AppVersion")
                     <*> (x .:? "Model")
                     <*> (x .:? "Make")
                     <*> (x .:? "ModelVersion")
                     <*> (x .:? "Timezone"))

instance Hashable EndpointDemographic

instance NFData EndpointDemographic

instance ToJSON EndpointDemographic where
        toJSON EndpointDemographic'{..}
          = object
              (catMaybes
                 [("Platform" .=) <$> _edPlatform,
                  ("PlatformVersion" .=) <$> _edPlatformVersion,
                  ("Locale" .=) <$> _edLocale,
                  ("AppVersion" .=) <$> _edAppVersion,
                  ("Model" .=) <$> _edModel, ("Make" .=) <$> _edMake,
                  ("ModelVersion" .=) <$> _edModelVersion,
                  ("Timezone" .=) <$> _edTimezone])

-- | /See:/ 'endpointLocation' smart constructor.
data EndpointLocation = EndpointLocation'
    { _elPostalCode :: !(Maybe Text)
    , _elLatitude   :: !(Maybe Double)
    , _elCountry    :: !(Maybe Text)
    , _elCity       :: !(Maybe Text)
    , _elRegion     :: !(Maybe Text)
    , _elLongitude  :: !(Maybe Double)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EndpointLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'elPostalCode' - The postal code or zip code of the endpoint.
--
-- * 'elLatitude' - The latitude of the endpoint location. Rounded to one decimal (Roughly corresponding to a mile).
--
-- * 'elCountry' - Country according to ISO 3166-1 Alpha-2 codes. For example, US.
--
-- * 'elCity' - The city where the endpoint is located.
--
-- * 'elRegion' - The region of the endpoint location. For example, corresponds to a state in US.
--
-- * 'elLongitude' - The longitude of the endpoint location. Rounded to one decimal (Roughly corresponding to a mile).
endpointLocation
    :: EndpointLocation
endpointLocation =
    EndpointLocation'
    { _elPostalCode = Nothing
    , _elLatitude = Nothing
    , _elCountry = Nothing
    , _elCity = Nothing
    , _elRegion = Nothing
    , _elLongitude = Nothing
    }

-- | The postal code or zip code of the endpoint.
elPostalCode :: Lens' EndpointLocation (Maybe Text)
elPostalCode = lens _elPostalCode (\ s a -> s{_elPostalCode = a});

-- | The latitude of the endpoint location. Rounded to one decimal (Roughly corresponding to a mile).
elLatitude :: Lens' EndpointLocation (Maybe Double)
elLatitude = lens _elLatitude (\ s a -> s{_elLatitude = a});

-- | Country according to ISO 3166-1 Alpha-2 codes. For example, US.
elCountry :: Lens' EndpointLocation (Maybe Text)
elCountry = lens _elCountry (\ s a -> s{_elCountry = a});

-- | The city where the endpoint is located.
elCity :: Lens' EndpointLocation (Maybe Text)
elCity = lens _elCity (\ s a -> s{_elCity = a});

-- | The region of the endpoint location. For example, corresponds to a state in US.
elRegion :: Lens' EndpointLocation (Maybe Text)
elRegion = lens _elRegion (\ s a -> s{_elRegion = a});

-- | The longitude of the endpoint location. Rounded to one decimal (Roughly corresponding to a mile).
elLongitude :: Lens' EndpointLocation (Maybe Double)
elLongitude = lens _elLongitude (\ s a -> s{_elLongitude = a});

instance FromJSON EndpointLocation where
        parseJSON
          = withObject "EndpointLocation"
              (\ x ->
                 EndpointLocation' <$>
                   (x .:? "PostalCode") <*> (x .:? "Latitude") <*>
                     (x .:? "Country")
                     <*> (x .:? "City")
                     <*> (x .:? "Region")
                     <*> (x .:? "Longitude"))

instance Hashable EndpointLocation

instance NFData EndpointLocation

instance ToJSON EndpointLocation where
        toJSON EndpointLocation'{..}
          = object
              (catMaybes
                 [("PostalCode" .=) <$> _elPostalCode,
                  ("Latitude" .=) <$> _elLatitude,
                  ("Country" .=) <$> _elCountry,
                  ("City" .=) <$> _elCity, ("Region" .=) <$> _elRegion,
                  ("Longitude" .=) <$> _elLongitude])

-- | /See:/ 'endpointRequest' smart constructor.
data EndpointRequest = EndpointRequest'
    { _erRequestId      :: !(Maybe Text)
    , _erMetrics        :: !(Maybe (Map Text Double))
    , _erLocation       :: !(Maybe EndpointLocation)
    , _erDemographic    :: !(Maybe EndpointDemographic)
    , _erAddress        :: !(Maybe Text)
    , _erEffectiveDate  :: !(Maybe Text)
    , _erUser           :: !(Maybe EndpointUser)
    , _erAttributes     :: !(Maybe (Map Text [Text]))
    , _erEndpointStatus :: !(Maybe Text)
    , _erOptOut         :: !(Maybe Text)
    , _erChannelType    :: !(Maybe ChannelType)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EndpointRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erRequestId' - The unique ID for the most recent request to update the endpoint.
--
-- * 'erMetrics' - Custom metrics that your app reports to Amazon Pinpoint.
--
-- * 'erLocation' - The endpoint location attributes.
--
-- * 'erDemographic' - The endpoint demographic attributes.
--
-- * 'erAddress' - The address or token of the endpoint.
--
-- * 'erEffectiveDate' - The last time the endpoint was updated. Provided in ISO 8601 format.
--
-- * 'erUser' - Custom user-specific attributes that your app reports to Amazon Pinpoint.
--
-- * 'erAttributes' - Custom attributes that your app reports to Amazon Pinpoint. You can use these attributes as selection criteria when you create a segment.
--
-- * 'erEndpointStatus' - The endpoint status. Can be either ACTIVE or INACTIVE. Will be set to INACTIVE if a delivery fails. Will be set to ACTIVE if the address is updated.
--
-- * 'erOptOut' - Indicates whether a user has opted out of receiving messages with one of the following values: ALL – User receives all messages. NONE – User receives no messages.
--
-- * 'erChannelType' - The channel type. Valid values: APNS, GCM
endpointRequest
    :: EndpointRequest
endpointRequest =
    EndpointRequest'
    { _erRequestId = Nothing
    , _erMetrics = Nothing
    , _erLocation = Nothing
    , _erDemographic = Nothing
    , _erAddress = Nothing
    , _erEffectiveDate = Nothing
    , _erUser = Nothing
    , _erAttributes = Nothing
    , _erEndpointStatus = Nothing
    , _erOptOut = Nothing
    , _erChannelType = Nothing
    }

-- | The unique ID for the most recent request to update the endpoint.
erRequestId :: Lens' EndpointRequest (Maybe Text)
erRequestId = lens _erRequestId (\ s a -> s{_erRequestId = a});

-- | Custom metrics that your app reports to Amazon Pinpoint.
erMetrics :: Lens' EndpointRequest (HashMap Text Double)
erMetrics = lens _erMetrics (\ s a -> s{_erMetrics = a}) . _Default . _Map;

-- | The endpoint location attributes.
erLocation :: Lens' EndpointRequest (Maybe EndpointLocation)
erLocation = lens _erLocation (\ s a -> s{_erLocation = a});

-- | The endpoint demographic attributes.
erDemographic :: Lens' EndpointRequest (Maybe EndpointDemographic)
erDemographic = lens _erDemographic (\ s a -> s{_erDemographic = a});

-- | The address or token of the endpoint.
erAddress :: Lens' EndpointRequest (Maybe Text)
erAddress = lens _erAddress (\ s a -> s{_erAddress = a});

-- | The last time the endpoint was updated. Provided in ISO 8601 format.
erEffectiveDate :: Lens' EndpointRequest (Maybe Text)
erEffectiveDate = lens _erEffectiveDate (\ s a -> s{_erEffectiveDate = a});

-- | Custom user-specific attributes that your app reports to Amazon Pinpoint.
erUser :: Lens' EndpointRequest (Maybe EndpointUser)
erUser = lens _erUser (\ s a -> s{_erUser = a});

-- | Custom attributes that your app reports to Amazon Pinpoint. You can use these attributes as selection criteria when you create a segment.
erAttributes :: Lens' EndpointRequest (HashMap Text [Text])
erAttributes = lens _erAttributes (\ s a -> s{_erAttributes = a}) . _Default . _Map;

-- | The endpoint status. Can be either ACTIVE or INACTIVE. Will be set to INACTIVE if a delivery fails. Will be set to ACTIVE if the address is updated.
erEndpointStatus :: Lens' EndpointRequest (Maybe Text)
erEndpointStatus = lens _erEndpointStatus (\ s a -> s{_erEndpointStatus = a});

-- | Indicates whether a user has opted out of receiving messages with one of the following values: ALL – User receives all messages. NONE – User receives no messages.
erOptOut :: Lens' EndpointRequest (Maybe Text)
erOptOut = lens _erOptOut (\ s a -> s{_erOptOut = a});

-- | The channel type. Valid values: APNS, GCM
erChannelType :: Lens' EndpointRequest (Maybe ChannelType)
erChannelType = lens _erChannelType (\ s a -> s{_erChannelType = a});

instance Hashable EndpointRequest

instance NFData EndpointRequest

instance ToJSON EndpointRequest where
        toJSON EndpointRequest'{..}
          = object
              (catMaybes
                 [("RequestId" .=) <$> _erRequestId,
                  ("Metrics" .=) <$> _erMetrics,
                  ("Location" .=) <$> _erLocation,
                  ("Demographic" .=) <$> _erDemographic,
                  ("Address" .=) <$> _erAddress,
                  ("EffectiveDate" .=) <$> _erEffectiveDate,
                  ("User" .=) <$> _erUser,
                  ("Attributes" .=) <$> _erAttributes,
                  ("EndpointStatus" .=) <$> _erEndpointStatus,
                  ("OptOut" .=) <$> _erOptOut,
                  ("ChannelType" .=) <$> _erChannelType])

-- | /See:/ 'endpointResponse' smart constructor.
data EndpointResponse = EndpointResponse'
    { _eRequestId      :: !(Maybe Text)
    , _eMetrics        :: !(Maybe (Map Text Double))
    , _eLocation       :: !(Maybe EndpointLocation)
    , _eDemographic    :: !(Maybe EndpointDemographic)
    , _eCohortId       :: !(Maybe Text)
    , _eAddress        :: !(Maybe Text)
    , _eEffectiveDate  :: !(Maybe Text)
    , _eUser           :: !(Maybe EndpointUser)
    , _eApplicationId  :: !(Maybe Text)
    , _eAttributes     :: !(Maybe (Map Text [Text]))
    , _eEndpointStatus :: !(Maybe Text)
    , _eOptOut         :: !(Maybe Text)
    , _eId             :: !(Maybe Text)
    , _eCreationDate   :: !(Maybe Text)
    , _eChannelType    :: !(Maybe ChannelType)
    , _eShardId        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eRequestId' - The unique ID for the most recent request to update the endpoint.
--
-- * 'eMetrics' - Custom metrics that your app reports to Amazon Pinpoint.
--
-- * 'eLocation' - The endpoint location attributes.
--
-- * 'eDemographic' - The endpoint demographic attributes.
--
-- * 'eCohortId' - A number from 0 - 99 that represents the cohort the endpoint is assigned to. Endpoints are grouped into cohorts randomly, and each cohort contains approximately 1 percent of the endpoints for an app. Amazon Pinpoint assigns cohorts to the holdout or treatment allocations for a campaign.
--
-- * 'eAddress' - The address or token of the endpoint.
--
-- * 'eEffectiveDate' - The last time the endpoint was updated. Provided in ISO 8601 format.
--
-- * 'eUser' - Custom user-specific attributes that your app reports to Amazon Pinpoint.
--
-- * 'eApplicationId' - The ID of the application associated with the endpoint.
--
-- * 'eAttributes' - Custom attributes that your app reports to Amazon Pinpoint. You can use these attributes as selection criteria when you create a segment.
--
-- * 'eEndpointStatus' - The endpoint status. Can be either ACTIVE or INACTIVE. Will be set to INACTIVE if a delivery fails. Will be set to ACTIVE if the address is updated.
--
-- * 'eOptOut' - Indicates whether a user has opted out of receiving messages with one of the following values: ALL – User receives all messages. NONE – User receives no messages.
--
-- * 'eId' - The unique ID that you assigned to the endpoint. The ID should be a globally unique identifier (GUID) to ensure that it is unique compared to all other endpoints for the application.
--
-- * 'eCreationDate' - The last time the endpoint was created. Provided in ISO 8601 format.
--
-- * 'eChannelType' - The channel type. Valid values: APNS, GCM
--
-- * 'eShardId' - The ShardId of endpoint
endpointResponse
    :: EndpointResponse
endpointResponse =
    EndpointResponse'
    { _eRequestId = Nothing
    , _eMetrics = Nothing
    , _eLocation = Nothing
    , _eDemographic = Nothing
    , _eCohortId = Nothing
    , _eAddress = Nothing
    , _eEffectiveDate = Nothing
    , _eUser = Nothing
    , _eApplicationId = Nothing
    , _eAttributes = Nothing
    , _eEndpointStatus = Nothing
    , _eOptOut = Nothing
    , _eId = Nothing
    , _eCreationDate = Nothing
    , _eChannelType = Nothing
    , _eShardId = Nothing
    }

-- | The unique ID for the most recent request to update the endpoint.
eRequestId :: Lens' EndpointResponse (Maybe Text)
eRequestId = lens _eRequestId (\ s a -> s{_eRequestId = a});

-- | Custom metrics that your app reports to Amazon Pinpoint.
eMetrics :: Lens' EndpointResponse (HashMap Text Double)
eMetrics = lens _eMetrics (\ s a -> s{_eMetrics = a}) . _Default . _Map;

-- | The endpoint location attributes.
eLocation :: Lens' EndpointResponse (Maybe EndpointLocation)
eLocation = lens _eLocation (\ s a -> s{_eLocation = a});

-- | The endpoint demographic attributes.
eDemographic :: Lens' EndpointResponse (Maybe EndpointDemographic)
eDemographic = lens _eDemographic (\ s a -> s{_eDemographic = a});

-- | A number from 0 - 99 that represents the cohort the endpoint is assigned to. Endpoints are grouped into cohorts randomly, and each cohort contains approximately 1 percent of the endpoints for an app. Amazon Pinpoint assigns cohorts to the holdout or treatment allocations for a campaign.
eCohortId :: Lens' EndpointResponse (Maybe Text)
eCohortId = lens _eCohortId (\ s a -> s{_eCohortId = a});

-- | The address or token of the endpoint.
eAddress :: Lens' EndpointResponse (Maybe Text)
eAddress = lens _eAddress (\ s a -> s{_eAddress = a});

-- | The last time the endpoint was updated. Provided in ISO 8601 format.
eEffectiveDate :: Lens' EndpointResponse (Maybe Text)
eEffectiveDate = lens _eEffectiveDate (\ s a -> s{_eEffectiveDate = a});

-- | Custom user-specific attributes that your app reports to Amazon Pinpoint.
eUser :: Lens' EndpointResponse (Maybe EndpointUser)
eUser = lens _eUser (\ s a -> s{_eUser = a});

-- | The ID of the application associated with the endpoint.
eApplicationId :: Lens' EndpointResponse (Maybe Text)
eApplicationId = lens _eApplicationId (\ s a -> s{_eApplicationId = a});

-- | Custom attributes that your app reports to Amazon Pinpoint. You can use these attributes as selection criteria when you create a segment.
eAttributes :: Lens' EndpointResponse (HashMap Text [Text])
eAttributes = lens _eAttributes (\ s a -> s{_eAttributes = a}) . _Default . _Map;

-- | The endpoint status. Can be either ACTIVE or INACTIVE. Will be set to INACTIVE if a delivery fails. Will be set to ACTIVE if the address is updated.
eEndpointStatus :: Lens' EndpointResponse (Maybe Text)
eEndpointStatus = lens _eEndpointStatus (\ s a -> s{_eEndpointStatus = a});

-- | Indicates whether a user has opted out of receiving messages with one of the following values: ALL – User receives all messages. NONE – User receives no messages.
eOptOut :: Lens' EndpointResponse (Maybe Text)
eOptOut = lens _eOptOut (\ s a -> s{_eOptOut = a});

-- | The unique ID that you assigned to the endpoint. The ID should be a globally unique identifier (GUID) to ensure that it is unique compared to all other endpoints for the application.
eId :: Lens' EndpointResponse (Maybe Text)
eId = lens _eId (\ s a -> s{_eId = a});

-- | The last time the endpoint was created. Provided in ISO 8601 format.
eCreationDate :: Lens' EndpointResponse (Maybe Text)
eCreationDate = lens _eCreationDate (\ s a -> s{_eCreationDate = a});

-- | The channel type. Valid values: APNS, GCM
eChannelType :: Lens' EndpointResponse (Maybe ChannelType)
eChannelType = lens _eChannelType (\ s a -> s{_eChannelType = a});

-- | The ShardId of endpoint
eShardId :: Lens' EndpointResponse (Maybe Text)
eShardId = lens _eShardId (\ s a -> s{_eShardId = a});

instance FromJSON EndpointResponse where
        parseJSON
          = withObject "EndpointResponse"
              (\ x ->
                 EndpointResponse' <$>
                   (x .:? "RequestId") <*> (x .:? "Metrics" .!= mempty)
                     <*> (x .:? "Location")
                     <*> (x .:? "Demographic")
                     <*> (x .:? "CohortId")
                     <*> (x .:? "Address")
                     <*> (x .:? "EffectiveDate")
                     <*> (x .:? "User")
                     <*> (x .:? "ApplicationId")
                     <*> (x .:? "Attributes" .!= mempty)
                     <*> (x .:? "EndpointStatus")
                     <*> (x .:? "OptOut")
                     <*> (x .:? "Id")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "ChannelType")
                     <*> (x .:? "ShardId"))

instance Hashable EndpointResponse

instance NFData EndpointResponse

-- | /See:/ 'endpointUser' smart constructor.
data EndpointUser = EndpointUser'
    { _euUserAttributes :: !(Maybe (Map Text [Text]))
    , _euUserId         :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EndpointUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'euUserAttributes' - Custom attributesd specific to the user.
--
-- * 'euUserId' - The unique ID of the user.
endpointUser
    :: EndpointUser
endpointUser =
    EndpointUser'
    { _euUserAttributes = Nothing
    , _euUserId = Nothing
    }

-- | Custom attributesd specific to the user.
euUserAttributes :: Lens' EndpointUser (HashMap Text [Text])
euUserAttributes = lens _euUserAttributes (\ s a -> s{_euUserAttributes = a}) . _Default . _Map;

-- | The unique ID of the user.
euUserId :: Lens' EndpointUser (Maybe Text)
euUserId = lens _euUserId (\ s a -> s{_euUserId = a});

instance FromJSON EndpointUser where
        parseJSON
          = withObject "EndpointUser"
              (\ x ->
                 EndpointUser' <$>
                   (x .:? "UserAttributes" .!= mempty) <*>
                     (x .:? "UserId"))

instance Hashable EndpointUser

instance NFData EndpointUser

instance ToJSON EndpointUser where
        toJSON EndpointUser'{..}
          = object
              (catMaybes
                 [("UserAttributes" .=) <$> _euUserAttributes,
                  ("UserId" .=) <$> _euUserId])

-- | /See:/ 'gcmChannelRequest' smart constructor.
newtype GCMChannelRequest = GCMChannelRequest'
    { _gcrAPIKey :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GCMChannelRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcrAPIKey' - Platform credential API key from Google.
gcmChannelRequest
    :: GCMChannelRequest
gcmChannelRequest =
    GCMChannelRequest'
    { _gcrAPIKey = Nothing
    }

-- | Platform credential API key from Google.
gcrAPIKey :: Lens' GCMChannelRequest (Maybe Text)
gcrAPIKey = lens _gcrAPIKey (\ s a -> s{_gcrAPIKey = a});

instance Hashable GCMChannelRequest

instance NFData GCMChannelRequest

instance ToJSON GCMChannelRequest where
        toJSON GCMChannelRequest'{..}
          = object (catMaybes [("ApiKey" .=) <$> _gcrAPIKey])

-- | /See:/ 'gcmChannelResponse' smart constructor.
data GCMChannelResponse = GCMChannelResponse'
    { _gcPlatform         :: !(Maybe Text)
    , _gcLastModifiedDate :: !(Maybe Text)
    , _gcCredential       :: !(Maybe Text)
    , _gcIsArchived       :: !(Maybe Bool)
    , _gcApplicationId    :: !(Maybe Text)
    , _gcVersion          :: !(Maybe Int)
    , _gcId               :: !(Maybe Text)
    , _gcCreationDate     :: !(Maybe Text)
    , _gcLastModifiedBy   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GCMChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcPlatform' - The platform type. Will be GCM
--
-- * 'gcLastModifiedDate' - Last date this was updated
--
-- * 'gcCredential' - The GCM API key from Google.
--
-- * 'gcIsArchived' - Is this channel archived
--
-- * 'gcApplicationId' - Undocumented member.
--
-- * 'gcVersion' - Version of channel
--
-- * 'gcId' - Undocumented member.
--
-- * 'gcCreationDate' - When was this segment created
--
-- * 'gcLastModifiedBy' - Who last updated this entry
gcmChannelResponse
    :: GCMChannelResponse
gcmChannelResponse =
    GCMChannelResponse'
    { _gcPlatform = Nothing
    , _gcLastModifiedDate = Nothing
    , _gcCredential = Nothing
    , _gcIsArchived = Nothing
    , _gcApplicationId = Nothing
    , _gcVersion = Nothing
    , _gcId = Nothing
    , _gcCreationDate = Nothing
    , _gcLastModifiedBy = Nothing
    }

-- | The platform type. Will be GCM
gcPlatform :: Lens' GCMChannelResponse (Maybe Text)
gcPlatform = lens _gcPlatform (\ s a -> s{_gcPlatform = a});

-- | Last date this was updated
gcLastModifiedDate :: Lens' GCMChannelResponse (Maybe Text)
gcLastModifiedDate = lens _gcLastModifiedDate (\ s a -> s{_gcLastModifiedDate = a});

-- | The GCM API key from Google.
gcCredential :: Lens' GCMChannelResponse (Maybe Text)
gcCredential = lens _gcCredential (\ s a -> s{_gcCredential = a});

-- | Is this channel archived
gcIsArchived :: Lens' GCMChannelResponse (Maybe Bool)
gcIsArchived = lens _gcIsArchived (\ s a -> s{_gcIsArchived = a});

-- | Undocumented member.
gcApplicationId :: Lens' GCMChannelResponse (Maybe Text)
gcApplicationId = lens _gcApplicationId (\ s a -> s{_gcApplicationId = a});

-- | Version of channel
gcVersion :: Lens' GCMChannelResponse (Maybe Int)
gcVersion = lens _gcVersion (\ s a -> s{_gcVersion = a});

-- | Undocumented member.
gcId :: Lens' GCMChannelResponse (Maybe Text)
gcId = lens _gcId (\ s a -> s{_gcId = a});

-- | When was this segment created
gcCreationDate :: Lens' GCMChannelResponse (Maybe Text)
gcCreationDate = lens _gcCreationDate (\ s a -> s{_gcCreationDate = a});

-- | Who last updated this entry
gcLastModifiedBy :: Lens' GCMChannelResponse (Maybe Text)
gcLastModifiedBy = lens _gcLastModifiedBy (\ s a -> s{_gcLastModifiedBy = a});

instance FromJSON GCMChannelResponse where
        parseJSON
          = withObject "GCMChannelResponse"
              (\ x ->
                 GCMChannelResponse' <$>
                   (x .:? "Platform") <*> (x .:? "LastModifiedDate") <*>
                     (x .:? "Credential")
                     <*> (x .:? "IsArchived")
                     <*> (x .:? "ApplicationId")
                     <*> (x .:? "Version")
                     <*> (x .:? "Id")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "LastModifiedBy"))

instance Hashable GCMChannelResponse

instance NFData GCMChannelResponse

-- | /See:/ 'importJobRequest' smart constructor.
data ImportJobRequest = ImportJobRequest'
    { _iSegmentName       :: !(Maybe Text)
    , _iFormat            :: !(Maybe DefinitionFormat)
    , _iDefineSegment     :: !(Maybe Bool)
    , _iRegisterEndpoints :: !(Maybe Bool)
    , _iExternalId        :: !(Maybe Text)
    , _iS3URL             :: !(Maybe Text)
    , _iSegmentId         :: !(Maybe Text)
    , _iRoleARN           :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ImportJobRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iSegmentName' - A custom name for the segment created by the import job. Use if DefineSegment is true.
--
-- * 'iFormat' - The format of the files that contain the endpoint definitions. Valid values: CSV, JSON
--
-- * 'iDefineSegment' - Sets whether the endpoints create a segment when they are imported.
--
-- * 'iRegisterEndpoints' - Sets whether the endpoints are registered with Amazon Pinpoint when they are imported.
--
-- * 'iExternalId' - A unique, custom ID assigned to the IAM role that restricts who can assume the role.
--
-- * 'iS3URL' - A URL that points to the location within an Amazon S3 bucket that contains the endpoints to import. The location can be a folder or a single file. The URL should follow this format: s3://bucket-name/folder-name/file-name Amazon Pinpoint will import endpoints from this location and any subfolders it contains.
--
-- * 'iSegmentId' - The ID of the segment to update if the import job is meant to update an existing segment.
--
-- * 'iRoleARN' - The Amazon Resource Name (ARN) of an IAM role that grants Amazon Pinpoint access to the Amazon S3 location that contains the endpoints to import.
importJobRequest
    :: ImportJobRequest
importJobRequest =
    ImportJobRequest'
    { _iSegmentName = Nothing
    , _iFormat = Nothing
    , _iDefineSegment = Nothing
    , _iRegisterEndpoints = Nothing
    , _iExternalId = Nothing
    , _iS3URL = Nothing
    , _iSegmentId = Nothing
    , _iRoleARN = Nothing
    }

-- | A custom name for the segment created by the import job. Use if DefineSegment is true.
iSegmentName :: Lens' ImportJobRequest (Maybe Text)
iSegmentName = lens _iSegmentName (\ s a -> s{_iSegmentName = a});

-- | The format of the files that contain the endpoint definitions. Valid values: CSV, JSON
iFormat :: Lens' ImportJobRequest (Maybe DefinitionFormat)
iFormat = lens _iFormat (\ s a -> s{_iFormat = a});

-- | Sets whether the endpoints create a segment when they are imported.
iDefineSegment :: Lens' ImportJobRequest (Maybe Bool)
iDefineSegment = lens _iDefineSegment (\ s a -> s{_iDefineSegment = a});

-- | Sets whether the endpoints are registered with Amazon Pinpoint when they are imported.
iRegisterEndpoints :: Lens' ImportJobRequest (Maybe Bool)
iRegisterEndpoints = lens _iRegisterEndpoints (\ s a -> s{_iRegisterEndpoints = a});

-- | A unique, custom ID assigned to the IAM role that restricts who can assume the role.
iExternalId :: Lens' ImportJobRequest (Maybe Text)
iExternalId = lens _iExternalId (\ s a -> s{_iExternalId = a});

-- | A URL that points to the location within an Amazon S3 bucket that contains the endpoints to import. The location can be a folder or a single file. The URL should follow this format: s3://bucket-name/folder-name/file-name Amazon Pinpoint will import endpoints from this location and any subfolders it contains.
iS3URL :: Lens' ImportJobRequest (Maybe Text)
iS3URL = lens _iS3URL (\ s a -> s{_iS3URL = a});

-- | The ID of the segment to update if the import job is meant to update an existing segment.
iSegmentId :: Lens' ImportJobRequest (Maybe Text)
iSegmentId = lens _iSegmentId (\ s a -> s{_iSegmentId = a});

-- | The Amazon Resource Name (ARN) of an IAM role that grants Amazon Pinpoint access to the Amazon S3 location that contains the endpoints to import.
iRoleARN :: Lens' ImportJobRequest (Maybe Text)
iRoleARN = lens _iRoleARN (\ s a -> s{_iRoleARN = a});

instance Hashable ImportJobRequest

instance NFData ImportJobRequest

instance ToJSON ImportJobRequest where
        toJSON ImportJobRequest'{..}
          = object
              (catMaybes
                 [("SegmentName" .=) <$> _iSegmentName,
                  ("Format" .=) <$> _iFormat,
                  ("DefineSegment" .=) <$> _iDefineSegment,
                  ("RegisterEndpoints" .=) <$> _iRegisterEndpoints,
                  ("ExternalId" .=) <$> _iExternalId,
                  ("S3Url" .=) <$> _iS3URL,
                  ("SegmentId" .=) <$> _iSegmentId,
                  ("RoleArn" .=) <$> _iRoleARN])

-- | /See:/ 'importJobResource' smart constructor.
data ImportJobResource = ImportJobResource'
    { _ijrSegmentName       :: !(Maybe Text)
    , _ijrFormat            :: !(Maybe DefinitionFormat)
    , _ijrDefineSegment     :: !(Maybe Bool)
    , _ijrRegisterEndpoints :: !(Maybe Bool)
    , _ijrExternalId        :: !(Maybe Text)
    , _ijrS3URL             :: !(Maybe Text)
    , _ijrSegmentId         :: !(Maybe Text)
    , _ijrRoleARN           :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ImportJobResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ijrSegmentName' - A custom name for the segment created by the import job. Use if DefineSegment is true.
--
-- * 'ijrFormat' - The format of the files that contain the endpoint definitions. Valid values: CSV, JSON
--
-- * 'ijrDefineSegment' - Sets whether the endpoints create a segment when they are imported.
--
-- * 'ijrRegisterEndpoints' - Sets whether the endpoints are registered with Amazon Pinpoint when they are imported.
--
-- * 'ijrExternalId' - A unique, custom ID assigned to the IAM role that restricts who can assume the role.
--
-- * 'ijrS3URL' - A URL that points to the location within an Amazon S3 bucket that contains the endpoints to import. The location can be a folder or a single file. The URL should follow this format: s3://bucket-name/folder-name/file-name Amazon Pinpoint will import endpoints from this location and any subfolders it contains.
--
-- * 'ijrSegmentId' - The ID of the segment to update if the import job is meant to update an existing segment.
--
-- * 'ijrRoleARN' - The Amazon Resource Name (ARN) of an IAM role that grants Amazon Pinpoint access to the Amazon S3 location that contains the endpoints to import.
importJobResource
    :: ImportJobResource
importJobResource =
    ImportJobResource'
    { _ijrSegmentName = Nothing
    , _ijrFormat = Nothing
    , _ijrDefineSegment = Nothing
    , _ijrRegisterEndpoints = Nothing
    , _ijrExternalId = Nothing
    , _ijrS3URL = Nothing
    , _ijrSegmentId = Nothing
    , _ijrRoleARN = Nothing
    }

-- | A custom name for the segment created by the import job. Use if DefineSegment is true.
ijrSegmentName :: Lens' ImportJobResource (Maybe Text)
ijrSegmentName = lens _ijrSegmentName (\ s a -> s{_ijrSegmentName = a});

-- | The format of the files that contain the endpoint definitions. Valid values: CSV, JSON
ijrFormat :: Lens' ImportJobResource (Maybe DefinitionFormat)
ijrFormat = lens _ijrFormat (\ s a -> s{_ijrFormat = a});

-- | Sets whether the endpoints create a segment when they are imported.
ijrDefineSegment :: Lens' ImportJobResource (Maybe Bool)
ijrDefineSegment = lens _ijrDefineSegment (\ s a -> s{_ijrDefineSegment = a});

-- | Sets whether the endpoints are registered with Amazon Pinpoint when they are imported.
ijrRegisterEndpoints :: Lens' ImportJobResource (Maybe Bool)
ijrRegisterEndpoints = lens _ijrRegisterEndpoints (\ s a -> s{_ijrRegisterEndpoints = a});

-- | A unique, custom ID assigned to the IAM role that restricts who can assume the role.
ijrExternalId :: Lens' ImportJobResource (Maybe Text)
ijrExternalId = lens _ijrExternalId (\ s a -> s{_ijrExternalId = a});

-- | A URL that points to the location within an Amazon S3 bucket that contains the endpoints to import. The location can be a folder or a single file. The URL should follow this format: s3://bucket-name/folder-name/file-name Amazon Pinpoint will import endpoints from this location and any subfolders it contains.
ijrS3URL :: Lens' ImportJobResource (Maybe Text)
ijrS3URL = lens _ijrS3URL (\ s a -> s{_ijrS3URL = a});

-- | The ID of the segment to update if the import job is meant to update an existing segment.
ijrSegmentId :: Lens' ImportJobResource (Maybe Text)
ijrSegmentId = lens _ijrSegmentId (\ s a -> s{_ijrSegmentId = a});

-- | The Amazon Resource Name (ARN) of an IAM role that grants Amazon Pinpoint access to the Amazon S3 location that contains the endpoints to import.
ijrRoleARN :: Lens' ImportJobResource (Maybe Text)
ijrRoleARN = lens _ijrRoleARN (\ s a -> s{_ijrRoleARN = a});

instance FromJSON ImportJobResource where
        parseJSON
          = withObject "ImportJobResource"
              (\ x ->
                 ImportJobResource' <$>
                   (x .:? "SegmentName") <*> (x .:? "Format") <*>
                     (x .:? "DefineSegment")
                     <*> (x .:? "RegisterEndpoints")
                     <*> (x .:? "ExternalId")
                     <*> (x .:? "S3Url")
                     <*> (x .:? "SegmentId")
                     <*> (x .:? "RoleArn"))

instance Hashable ImportJobResource

instance NFData ImportJobResource

-- | /See:/ 'importJobResponse' smart constructor.
data ImportJobResponse = ImportJobResponse'
    { _ijCompletedPieces :: !(Maybe Int)
    , _ijFailedPieces    :: !(Maybe Int)
    , _ijDefinition      :: !(Maybe ImportJobResource)
    , _ijTotalProcessed  :: !(Maybe Int)
    , _ijFailures        :: !(Maybe [Text])
    , _ijTotalPieces     :: !(Maybe Int)
    , _ijApplicationId   :: !(Maybe Text)
    , _ijId              :: !(Maybe Text)
    , _ijCreationDate    :: !(Maybe Text)
    , _ijType            :: !(Maybe Text)
    , _ijCompletionDate  :: !(Maybe Text)
    , _ijJobStatus       :: !(Maybe JobStatus)
    , _ijTotalFailures   :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ImportJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ijCompletedPieces' - The number of pieces that have successfully imported as of the time of the request.
--
-- * 'ijFailedPieces' - The number of pieces that have failed to import as of the time of the request.
--
-- * 'ijDefinition' - The import job settings.
--
-- * 'ijTotalProcessed' - The number of endpoints that were processed by the import job.
--
-- * 'ijFailures' - Provides up to 100 of the first failed entries for the job, if any exist.
--
-- * 'ijTotalPieces' - The total number of pieces that must be imported to finish the job. Each piece is an approximately equal portion of the endpoints to import.
--
-- * 'ijApplicationId' - The unique ID of the application to which the import job applies.
--
-- * 'ijId' - The unique ID of the import job.
--
-- * 'ijCreationDate' - The date the import job was created in ISO 8601 format.
--
-- * 'ijType' - The job type. Will be Import.
--
-- * 'ijCompletionDate' - The date the import job completed in ISO 8601 format.
--
-- * 'ijJobStatus' - The status of the import job. Valid values: CREATED, INITIALIZING, PROCESSING, COMPLETING, COMPLETED, FAILING, FAILED The job status is FAILED if one or more pieces failed to import.
--
-- * 'ijTotalFailures' - The number of endpoints that failed to import; for example, because of syntax errors.
importJobResponse
    :: ImportJobResponse
importJobResponse =
    ImportJobResponse'
    { _ijCompletedPieces = Nothing
    , _ijFailedPieces = Nothing
    , _ijDefinition = Nothing
    , _ijTotalProcessed = Nothing
    , _ijFailures = Nothing
    , _ijTotalPieces = Nothing
    , _ijApplicationId = Nothing
    , _ijId = Nothing
    , _ijCreationDate = Nothing
    , _ijType = Nothing
    , _ijCompletionDate = Nothing
    , _ijJobStatus = Nothing
    , _ijTotalFailures = Nothing
    }

-- | The number of pieces that have successfully imported as of the time of the request.
ijCompletedPieces :: Lens' ImportJobResponse (Maybe Int)
ijCompletedPieces = lens _ijCompletedPieces (\ s a -> s{_ijCompletedPieces = a});

-- | The number of pieces that have failed to import as of the time of the request.
ijFailedPieces :: Lens' ImportJobResponse (Maybe Int)
ijFailedPieces = lens _ijFailedPieces (\ s a -> s{_ijFailedPieces = a});

-- | The import job settings.
ijDefinition :: Lens' ImportJobResponse (Maybe ImportJobResource)
ijDefinition = lens _ijDefinition (\ s a -> s{_ijDefinition = a});

-- | The number of endpoints that were processed by the import job.
ijTotalProcessed :: Lens' ImportJobResponse (Maybe Int)
ijTotalProcessed = lens _ijTotalProcessed (\ s a -> s{_ijTotalProcessed = a});

-- | Provides up to 100 of the first failed entries for the job, if any exist.
ijFailures :: Lens' ImportJobResponse [Text]
ijFailures = lens _ijFailures (\ s a -> s{_ijFailures = a}) . _Default . _Coerce;

-- | The total number of pieces that must be imported to finish the job. Each piece is an approximately equal portion of the endpoints to import.
ijTotalPieces :: Lens' ImportJobResponse (Maybe Int)
ijTotalPieces = lens _ijTotalPieces (\ s a -> s{_ijTotalPieces = a});

-- | The unique ID of the application to which the import job applies.
ijApplicationId :: Lens' ImportJobResponse (Maybe Text)
ijApplicationId = lens _ijApplicationId (\ s a -> s{_ijApplicationId = a});

-- | The unique ID of the import job.
ijId :: Lens' ImportJobResponse (Maybe Text)
ijId = lens _ijId (\ s a -> s{_ijId = a});

-- | The date the import job was created in ISO 8601 format.
ijCreationDate :: Lens' ImportJobResponse (Maybe Text)
ijCreationDate = lens _ijCreationDate (\ s a -> s{_ijCreationDate = a});

-- | The job type. Will be Import.
ijType :: Lens' ImportJobResponse (Maybe Text)
ijType = lens _ijType (\ s a -> s{_ijType = a});

-- | The date the import job completed in ISO 8601 format.
ijCompletionDate :: Lens' ImportJobResponse (Maybe Text)
ijCompletionDate = lens _ijCompletionDate (\ s a -> s{_ijCompletionDate = a});

-- | The status of the import job. Valid values: CREATED, INITIALIZING, PROCESSING, COMPLETING, COMPLETED, FAILING, FAILED The job status is FAILED if one or more pieces failed to import.
ijJobStatus :: Lens' ImportJobResponse (Maybe JobStatus)
ijJobStatus = lens _ijJobStatus (\ s a -> s{_ijJobStatus = a});

-- | The number of endpoints that failed to import; for example, because of syntax errors.
ijTotalFailures :: Lens' ImportJobResponse (Maybe Int)
ijTotalFailures = lens _ijTotalFailures (\ s a -> s{_ijTotalFailures = a});

instance FromJSON ImportJobResponse where
        parseJSON
          = withObject "ImportJobResponse"
              (\ x ->
                 ImportJobResponse' <$>
                   (x .:? "CompletedPieces") <*> (x .:? "FailedPieces")
                     <*> (x .:? "Definition")
                     <*> (x .:? "TotalProcessed")
                     <*> (x .:? "Failures" .!= mempty)
                     <*> (x .:? "TotalPieces")
                     <*> (x .:? "ApplicationId")
                     <*> (x .:? "Id")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "Type")
                     <*> (x .:? "CompletionDate")
                     <*> (x .:? "JobStatus")
                     <*> (x .:? "TotalFailures"))

instance Hashable ImportJobResponse

instance NFData ImportJobResponse

-- | /See:/ 'importJobsResponse' smart constructor.
data ImportJobsResponse = ImportJobsResponse'
    { _ijNextToken :: !(Maybe Text)
    , _ijItem      :: !(Maybe [ImportJobResponse])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ImportJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ijNextToken' - The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- * 'ijItem' - A list of import jobs for the application.
importJobsResponse
    :: ImportJobsResponse
importJobsResponse =
    ImportJobsResponse'
    { _ijNextToken = Nothing
    , _ijItem = Nothing
    }

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
ijNextToken :: Lens' ImportJobsResponse (Maybe Text)
ijNextToken = lens _ijNextToken (\ s a -> s{_ijNextToken = a});

-- | A list of import jobs for the application.
ijItem :: Lens' ImportJobsResponse [ImportJobResponse]
ijItem = lens _ijItem (\ s a -> s{_ijItem = a}) . _Default . _Coerce;

instance FromJSON ImportJobsResponse where
        parseJSON
          = withObject "ImportJobsResponse"
              (\ x ->
                 ImportJobsResponse' <$>
                   (x .:? "NextToken") <*> (x .:? "Item" .!= mempty))

instance Hashable ImportJobsResponse

instance NFData ImportJobsResponse

-- | /See:/ 'message' smart constructor.
data Message = Message'
    { _mSilentPush   :: !(Maybe Bool)
    , _mImageIconURL :: !(Maybe Text)
    , _mBody         :: !(Maybe Text)
    , _mJSONBody     :: !(Maybe Text)
    , _mURL          :: !(Maybe Text)
    , _mAction       :: !(Maybe Action)
    , _mImageURL     :: !(Maybe Text)
    , _mMediaURL     :: !(Maybe Text)
    , _mTitle        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Message' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mSilentPush' - Indicates if the message should display on the users device. Silent pushes can be used for Remote Configuration and Phone Home use cases.
--
-- * 'mImageIconURL' - The URL that points to the icon image for the push notification icon, for example, the app icon.
--
-- * 'mBody' - The message body. Can include up to 140 characters.
--
-- * 'mJSONBody' - The JSON payload used for a silent push.
--
-- * 'mURL' - The URL to open in the user's mobile browser. Used if the value for Action is URL.
--
-- * 'mAction' - The action that occurs if the user taps a push notification delivered by the campaign: OPEN_APP – Your app launches, or it becomes the foreground app if it has been sent to the background. This is the default action. DEEP_LINK – Uses deep linking features in iOS and Android to open your app and display a designated user interface within the app. URL – The default mobile browser on the user's device launches and opens a web page at the URL you specify.
--
-- * 'mImageURL' - The URL that points to an image used in the push notification.
--
-- * 'mMediaURL' - The URL that points to the media resource, for example a .mp4 or .gif file.
--
-- * 'mTitle' - The message title that displays above the message on the user's device.
message
    :: Message
message =
    Message'
    { _mSilentPush = Nothing
    , _mImageIconURL = Nothing
    , _mBody = Nothing
    , _mJSONBody = Nothing
    , _mURL = Nothing
    , _mAction = Nothing
    , _mImageURL = Nothing
    , _mMediaURL = Nothing
    , _mTitle = Nothing
    }

-- | Indicates if the message should display on the users device. Silent pushes can be used for Remote Configuration and Phone Home use cases.
mSilentPush :: Lens' Message (Maybe Bool)
mSilentPush = lens _mSilentPush (\ s a -> s{_mSilentPush = a});

-- | The URL that points to the icon image for the push notification icon, for example, the app icon.
mImageIconURL :: Lens' Message (Maybe Text)
mImageIconURL = lens _mImageIconURL (\ s a -> s{_mImageIconURL = a});

-- | The message body. Can include up to 140 characters.
mBody :: Lens' Message (Maybe Text)
mBody = lens _mBody (\ s a -> s{_mBody = a});

-- | The JSON payload used for a silent push.
mJSONBody :: Lens' Message (Maybe Text)
mJSONBody = lens _mJSONBody (\ s a -> s{_mJSONBody = a});

-- | The URL to open in the user's mobile browser. Used if the value for Action is URL.
mURL :: Lens' Message (Maybe Text)
mURL = lens _mURL (\ s a -> s{_mURL = a});

-- | The action that occurs if the user taps a push notification delivered by the campaign: OPEN_APP – Your app launches, or it becomes the foreground app if it has been sent to the background. This is the default action. DEEP_LINK – Uses deep linking features in iOS and Android to open your app and display a designated user interface within the app. URL – The default mobile browser on the user's device launches and opens a web page at the URL you specify.
mAction :: Lens' Message (Maybe Action)
mAction = lens _mAction (\ s a -> s{_mAction = a});

-- | The URL that points to an image used in the push notification.
mImageURL :: Lens' Message (Maybe Text)
mImageURL = lens _mImageURL (\ s a -> s{_mImageURL = a});

-- | The URL that points to the media resource, for example a .mp4 or .gif file.
mMediaURL :: Lens' Message (Maybe Text)
mMediaURL = lens _mMediaURL (\ s a -> s{_mMediaURL = a});

-- | The message title that displays above the message on the user's device.
mTitle :: Lens' Message (Maybe Text)
mTitle = lens _mTitle (\ s a -> s{_mTitle = a});

instance FromJSON Message where
        parseJSON
          = withObject "Message"
              (\ x ->
                 Message' <$>
                   (x .:? "SilentPush") <*> (x .:? "ImageIconUrl") <*>
                     (x .:? "Body")
                     <*> (x .:? "JsonBody")
                     <*> (x .:? "Url")
                     <*> (x .:? "Action")
                     <*> (x .:? "ImageUrl")
                     <*> (x .:? "MediaUrl")
                     <*> (x .:? "Title"))

instance Hashable Message

instance NFData Message

instance ToJSON Message where
        toJSON Message'{..}
          = object
              (catMaybes
                 [("SilentPush" .=) <$> _mSilentPush,
                  ("ImageIconUrl" .=) <$> _mImageIconURL,
                  ("Body" .=) <$> _mBody,
                  ("JsonBody" .=) <$> _mJSONBody, ("Url" .=) <$> _mURL,
                  ("Action" .=) <$> _mAction,
                  ("ImageUrl" .=) <$> _mImageURL,
                  ("MediaUrl" .=) <$> _mMediaURL,
                  ("Title" .=) <$> _mTitle])

-- | /See:/ 'messageBody' smart constructor.
data MessageBody = MessageBody'
    { _mbRequestId :: !(Maybe Text)
    , _mbMessage   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MessageBody' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mbRequestId' - Undocumented member.
--
-- * 'mbMessage' - Undocumented member.
messageBody
    :: MessageBody
messageBody =
    MessageBody'
    { _mbRequestId = Nothing
    , _mbMessage = Nothing
    }

-- | Undocumented member.
mbRequestId :: Lens' MessageBody (Maybe Text)
mbRequestId = lens _mbRequestId (\ s a -> s{_mbRequestId = a});

-- | Undocumented member.
mbMessage :: Lens' MessageBody (Maybe Text)
mbMessage = lens _mbMessage (\ s a -> s{_mbMessage = a});

instance FromJSON MessageBody where
        parseJSON
          = withObject "MessageBody"
              (\ x ->
                 MessageBody' <$>
                   (x .:? "RequestID") <*> (x .:? "Message"))

instance Hashable MessageBody

instance NFData MessageBody

-- | /See:/ 'messageConfiguration' smart constructor.
data MessageConfiguration = MessageConfiguration'
    { _mcAPNSMessage    :: !(Maybe Message)
    , _mcGCMMessage     :: !(Maybe Message)
    , _mcDefaultMessage :: !(Maybe Message)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MessageConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcAPNSMessage' - The message that the campaign delivers to APNS channels. Overrides the default message.
--
-- * 'mcGCMMessage' - The message that the campaign delivers to GCM channels. Overrides the default message.
--
-- * 'mcDefaultMessage' - The default message for all channels.
messageConfiguration
    :: MessageConfiguration
messageConfiguration =
    MessageConfiguration'
    { _mcAPNSMessage = Nothing
    , _mcGCMMessage = Nothing
    , _mcDefaultMessage = Nothing
    }

-- | The message that the campaign delivers to APNS channels. Overrides the default message.
mcAPNSMessage :: Lens' MessageConfiguration (Maybe Message)
mcAPNSMessage = lens _mcAPNSMessage (\ s a -> s{_mcAPNSMessage = a});

-- | The message that the campaign delivers to GCM channels. Overrides the default message.
mcGCMMessage :: Lens' MessageConfiguration (Maybe Message)
mcGCMMessage = lens _mcGCMMessage (\ s a -> s{_mcGCMMessage = a});

-- | The default message for all channels.
mcDefaultMessage :: Lens' MessageConfiguration (Maybe Message)
mcDefaultMessage = lens _mcDefaultMessage (\ s a -> s{_mcDefaultMessage = a});

instance FromJSON MessageConfiguration where
        parseJSON
          = withObject "MessageConfiguration"
              (\ x ->
                 MessageConfiguration' <$>
                   (x .:? "APNSMessage") <*> (x .:? "GCMMessage") <*>
                     (x .:? "DefaultMessage"))

instance Hashable MessageConfiguration

instance NFData MessageConfiguration

instance ToJSON MessageConfiguration where
        toJSON MessageConfiguration'{..}
          = object
              (catMaybes
                 [("APNSMessage" .=) <$> _mcAPNSMessage,
                  ("GCMMessage" .=) <$> _mcGCMMessage,
                  ("DefaultMessage" .=) <$> _mcDefaultMessage])

-- | /See:/ 'quietTime' smart constructor.
data QuietTime = QuietTime'
    { _qtStart :: !(Maybe Text)
    , _qtEnd   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'QuietTime' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qtStart' - The default start time for quiet time in ISO 8601 format.
--
-- * 'qtEnd' - The default end time for quiet time in ISO 8601 format.
quietTime
    :: QuietTime
quietTime =
    QuietTime'
    { _qtStart = Nothing
    , _qtEnd = Nothing
    }

-- | The default start time for quiet time in ISO 8601 format.
qtStart :: Lens' QuietTime (Maybe Text)
qtStart = lens _qtStart (\ s a -> s{_qtStart = a});

-- | The default end time for quiet time in ISO 8601 format.
qtEnd :: Lens' QuietTime (Maybe Text)
qtEnd = lens _qtEnd (\ s a -> s{_qtEnd = a});

instance FromJSON QuietTime where
        parseJSON
          = withObject "QuietTime"
              (\ x ->
                 QuietTime' <$> (x .:? "Start") <*> (x .:? "End"))

instance Hashable QuietTime

instance NFData QuietTime

instance ToJSON QuietTime where
        toJSON QuietTime'{..}
          = object
              (catMaybes
                 [("Start" .=) <$> _qtStart, ("End" .=) <$> _qtEnd])

-- | /See:/ 'recencyDimension' smart constructor.
data RecencyDimension = RecencyDimension'
    { _rdRecencyType :: !(Maybe RecencyType)
    , _rdDuration    :: !(Maybe Duration)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RecencyDimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdRecencyType' - The recency dimension type: ACTIVE – Users who have used your app within the specified duration are included in the segment. INACTIVE – Users who have not used your app within the specified duration are included in the segment.
--
-- * 'rdDuration' - The length of time during which users have been active or inactive with your app. Valid values: HR_24, DAY_7, DAY_14, DAY_30
recencyDimension
    :: RecencyDimension
recencyDimension =
    RecencyDimension'
    { _rdRecencyType = Nothing
    , _rdDuration = Nothing
    }

-- | The recency dimension type: ACTIVE – Users who have used your app within the specified duration are included in the segment. INACTIVE – Users who have not used your app within the specified duration are included in the segment.
rdRecencyType :: Lens' RecencyDimension (Maybe RecencyType)
rdRecencyType = lens _rdRecencyType (\ s a -> s{_rdRecencyType = a});

-- | The length of time during which users have been active or inactive with your app. Valid values: HR_24, DAY_7, DAY_14, DAY_30
rdDuration :: Lens' RecencyDimension (Maybe Duration)
rdDuration = lens _rdDuration (\ s a -> s{_rdDuration = a});

instance FromJSON RecencyDimension where
        parseJSON
          = withObject "RecencyDimension"
              (\ x ->
                 RecencyDimension' <$>
                   (x .:? "RecencyType") <*> (x .:? "Duration"))

instance Hashable RecencyDimension

instance NFData RecencyDimension

instance ToJSON RecencyDimension where
        toJSON RecencyDimension'{..}
          = object
              (catMaybes
                 [("RecencyType" .=) <$> _rdRecencyType,
                  ("Duration" .=) <$> _rdDuration])

-- | /See:/ 'schedule' smart constructor.
data Schedule = Schedule'
    { _sFrequency   :: !(Maybe Frequency)
    , _sStartTime   :: !(Maybe Text)
    , _sQuietTime   :: !(Maybe QuietTime)
    , _sIsLocalTime :: !(Maybe Bool)
    , _sEndTime     :: !(Maybe Text)
    , _sTimezone    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Schedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sFrequency' - How often the campaign delivers messages. Valid values: ONCE, HOURLY, DAILY, WEEKLY, MONTHLY
--
-- * 'sStartTime' - The scheduled time that the campaign begins in ISO 8601 format.
--
-- * 'sQuietTime' - The time during which the campaign sends no messages.
--
-- * 'sIsLocalTime' - Indicates whether the campaign schedule takes effect according to each user's local time.
--
-- * 'sEndTime' - The scheduled time that the campaign ends in ISO 8601 format.
--
-- * 'sTimezone' - The starting UTC offset for the schedule if the value for isLocalTime is true Valid values:  UTC UTC+01 UTC+02 UTC+03 UTC+03:30 UTC+04 UTC+04:30 UTC+05 UTC+05:30 UTC+05:45 UTC+06 UTC+06:30 UTC+07 UTC+08 UTC+09 UTC+09:30 UTC+10 UTC+10:30 UTC+11 UTC+12 UTC+13 UTC-02 UTC-03 UTC-04 UTC-05 UTC-06 UTC-07 UTC-08 UTC-09 UTC-10 UTC-11
schedule
    :: Schedule
schedule =
    Schedule'
    { _sFrequency = Nothing
    , _sStartTime = Nothing
    , _sQuietTime = Nothing
    , _sIsLocalTime = Nothing
    , _sEndTime = Nothing
    , _sTimezone = Nothing
    }

-- | How often the campaign delivers messages. Valid values: ONCE, HOURLY, DAILY, WEEKLY, MONTHLY
sFrequency :: Lens' Schedule (Maybe Frequency)
sFrequency = lens _sFrequency (\ s a -> s{_sFrequency = a});

-- | The scheduled time that the campaign begins in ISO 8601 format.
sStartTime :: Lens' Schedule (Maybe Text)
sStartTime = lens _sStartTime (\ s a -> s{_sStartTime = a});

-- | The time during which the campaign sends no messages.
sQuietTime :: Lens' Schedule (Maybe QuietTime)
sQuietTime = lens _sQuietTime (\ s a -> s{_sQuietTime = a});

-- | Indicates whether the campaign schedule takes effect according to each user's local time.
sIsLocalTime :: Lens' Schedule (Maybe Bool)
sIsLocalTime = lens _sIsLocalTime (\ s a -> s{_sIsLocalTime = a});

-- | The scheduled time that the campaign ends in ISO 8601 format.
sEndTime :: Lens' Schedule (Maybe Text)
sEndTime = lens _sEndTime (\ s a -> s{_sEndTime = a});

-- | The starting UTC offset for the schedule if the value for isLocalTime is true Valid values:  UTC UTC+01 UTC+02 UTC+03 UTC+03:30 UTC+04 UTC+04:30 UTC+05 UTC+05:30 UTC+05:45 UTC+06 UTC+06:30 UTC+07 UTC+08 UTC+09 UTC+09:30 UTC+10 UTC+10:30 UTC+11 UTC+12 UTC+13 UTC-02 UTC-03 UTC-04 UTC-05 UTC-06 UTC-07 UTC-08 UTC-09 UTC-10 UTC-11
sTimezone :: Lens' Schedule (Maybe Text)
sTimezone = lens _sTimezone (\ s a -> s{_sTimezone = a});

instance FromJSON Schedule where
        parseJSON
          = withObject "Schedule"
              (\ x ->
                 Schedule' <$>
                   (x .:? "Frequency") <*> (x .:? "StartTime") <*>
                     (x .:? "QuietTime")
                     <*> (x .:? "IsLocalTime")
                     <*> (x .:? "EndTime")
                     <*> (x .:? "Timezone"))

instance Hashable Schedule

instance NFData Schedule

instance ToJSON Schedule where
        toJSON Schedule'{..}
          = object
              (catMaybes
                 [("Frequency" .=) <$> _sFrequency,
                  ("StartTime" .=) <$> _sStartTime,
                  ("QuietTime" .=) <$> _sQuietTime,
                  ("IsLocalTime" .=) <$> _sIsLocalTime,
                  ("EndTime" .=) <$> _sEndTime,
                  ("Timezone" .=) <$> _sTimezone])

-- | /See:/ 'segmentBehaviors' smart constructor.
newtype SegmentBehaviors = SegmentBehaviors'
    { _sbRecency :: Maybe RecencyDimension
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SegmentBehaviors' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sbRecency' - The recency of use.
segmentBehaviors
    :: SegmentBehaviors
segmentBehaviors =
    SegmentBehaviors'
    { _sbRecency = Nothing
    }

-- | The recency of use.
sbRecency :: Lens' SegmentBehaviors (Maybe RecencyDimension)
sbRecency = lens _sbRecency (\ s a -> s{_sbRecency = a});

instance FromJSON SegmentBehaviors where
        parseJSON
          = withObject "SegmentBehaviors"
              (\ x -> SegmentBehaviors' <$> (x .:? "Recency"))

instance Hashable SegmentBehaviors

instance NFData SegmentBehaviors

instance ToJSON SegmentBehaviors where
        toJSON SegmentBehaviors'{..}
          = object (catMaybes [("Recency" .=) <$> _sbRecency])

-- | /See:/ 'segmentDemographics' smart constructor.
data SegmentDemographics = SegmentDemographics'
    { _sdPlatform   :: !(Maybe SetDimension)
    , _sdAppVersion :: !(Maybe SetDimension)
    , _sdModel      :: !(Maybe SetDimension)
    , _sdMake       :: !(Maybe SetDimension)
    , _sdDeviceType :: !(Maybe SetDimension)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SegmentDemographics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdPlatform' - The device platform criteria for the segment.
--
-- * 'sdAppVersion' - The app version criteria for the segment.
--
-- * 'sdModel' - The device model criteria for the segment.
--
-- * 'sdMake' - The device make criteria for the segment.
--
-- * 'sdDeviceType' - The device type criteria for the segment.
segmentDemographics
    :: SegmentDemographics
segmentDemographics =
    SegmentDemographics'
    { _sdPlatform = Nothing
    , _sdAppVersion = Nothing
    , _sdModel = Nothing
    , _sdMake = Nothing
    , _sdDeviceType = Nothing
    }

-- | The device platform criteria for the segment.
sdPlatform :: Lens' SegmentDemographics (Maybe SetDimension)
sdPlatform = lens _sdPlatform (\ s a -> s{_sdPlatform = a});

-- | The app version criteria for the segment.
sdAppVersion :: Lens' SegmentDemographics (Maybe SetDimension)
sdAppVersion = lens _sdAppVersion (\ s a -> s{_sdAppVersion = a});

-- | The device model criteria for the segment.
sdModel :: Lens' SegmentDemographics (Maybe SetDimension)
sdModel = lens _sdModel (\ s a -> s{_sdModel = a});

-- | The device make criteria for the segment.
sdMake :: Lens' SegmentDemographics (Maybe SetDimension)
sdMake = lens _sdMake (\ s a -> s{_sdMake = a});

-- | The device type criteria for the segment.
sdDeviceType :: Lens' SegmentDemographics (Maybe SetDimension)
sdDeviceType = lens _sdDeviceType (\ s a -> s{_sdDeviceType = a});

instance FromJSON SegmentDemographics where
        parseJSON
          = withObject "SegmentDemographics"
              (\ x ->
                 SegmentDemographics' <$>
                   (x .:? "Platform") <*> (x .:? "AppVersion") <*>
                     (x .:? "Model")
                     <*> (x .:? "Make")
                     <*> (x .:? "DeviceType"))

instance Hashable SegmentDemographics

instance NFData SegmentDemographics

instance ToJSON SegmentDemographics where
        toJSON SegmentDemographics'{..}
          = object
              (catMaybes
                 [("Platform" .=) <$> _sdPlatform,
                  ("AppVersion" .=) <$> _sdAppVersion,
                  ("Model" .=) <$> _sdModel, ("Make" .=) <$> _sdMake,
                  ("DeviceType" .=) <$> _sdDeviceType])

-- | /See:/ 'segmentDimensions' smart constructor.
data SegmentDimensions = SegmentDimensions'
    { _sdLocation    :: !(Maybe SegmentLocation)
    , _sdDemographic :: !(Maybe SegmentDemographics)
    , _sdBehavior    :: !(Maybe SegmentBehaviors)
    , _sdAttributes  :: !(Maybe (Map Text AttributeDimension))
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SegmentDimensions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdLocation' - The segment location attributes.
--
-- * 'sdDemographic' - The segment demographics attributes.
--
-- * 'sdBehavior' - The segment behaviors attributes.
--
-- * 'sdAttributes' - Custom segment attributes.
segmentDimensions
    :: SegmentDimensions
segmentDimensions =
    SegmentDimensions'
    { _sdLocation = Nothing
    , _sdDemographic = Nothing
    , _sdBehavior = Nothing
    , _sdAttributes = Nothing
    }

-- | The segment location attributes.
sdLocation :: Lens' SegmentDimensions (Maybe SegmentLocation)
sdLocation = lens _sdLocation (\ s a -> s{_sdLocation = a});

-- | The segment demographics attributes.
sdDemographic :: Lens' SegmentDimensions (Maybe SegmentDemographics)
sdDemographic = lens _sdDemographic (\ s a -> s{_sdDemographic = a});

-- | The segment behaviors attributes.
sdBehavior :: Lens' SegmentDimensions (Maybe SegmentBehaviors)
sdBehavior = lens _sdBehavior (\ s a -> s{_sdBehavior = a});

-- | Custom segment attributes.
sdAttributes :: Lens' SegmentDimensions (HashMap Text AttributeDimension)
sdAttributes = lens _sdAttributes (\ s a -> s{_sdAttributes = a}) . _Default . _Map;

instance FromJSON SegmentDimensions where
        parseJSON
          = withObject "SegmentDimensions"
              (\ x ->
                 SegmentDimensions' <$>
                   (x .:? "Location") <*> (x .:? "Demographic") <*>
                     (x .:? "Behavior")
                     <*> (x .:? "Attributes" .!= mempty))

instance Hashable SegmentDimensions

instance NFData SegmentDimensions

instance ToJSON SegmentDimensions where
        toJSON SegmentDimensions'{..}
          = object
              (catMaybes
                 [("Location" .=) <$> _sdLocation,
                  ("Demographic" .=) <$> _sdDemographic,
                  ("Behavior" .=) <$> _sdBehavior,
                  ("Attributes" .=) <$> _sdAttributes])

-- | /See:/ 'segmentImportResource' smart constructor.
data SegmentImportResource = SegmentImportResource'
    { _sirSize       :: !(Maybe Int)
    , _sirFormat     :: !(Maybe DefinitionFormat)
    , _sirExternalId :: !(Maybe Text)
    , _sirS3URL      :: !(Maybe Text)
    , _sirRoleARN    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SegmentImportResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sirSize' - The number of endpoints that were successfully imported to create this segment.
--
-- * 'sirFormat' - The format of the endpoint files that were imported to create this segment. Valid values: CSV, JSON
--
-- * 'sirExternalId' - A unique, custom ID assigned to the IAM role that restricts who can assume the role.
--
-- * 'sirS3URL' - A URL that points to the Amazon S3 location from which the endpoints for this segment were imported.
--
-- * 'sirRoleARN' - The Amazon Resource Name (ARN) of an IAM role that grants Amazon Pinpoint access to the endpoints in Amazon S3.
segmentImportResource
    :: SegmentImportResource
segmentImportResource =
    SegmentImportResource'
    { _sirSize = Nothing
    , _sirFormat = Nothing
    , _sirExternalId = Nothing
    , _sirS3URL = Nothing
    , _sirRoleARN = Nothing
    }

-- | The number of endpoints that were successfully imported to create this segment.
sirSize :: Lens' SegmentImportResource (Maybe Int)
sirSize = lens _sirSize (\ s a -> s{_sirSize = a});

-- | The format of the endpoint files that were imported to create this segment. Valid values: CSV, JSON
sirFormat :: Lens' SegmentImportResource (Maybe DefinitionFormat)
sirFormat = lens _sirFormat (\ s a -> s{_sirFormat = a});

-- | A unique, custom ID assigned to the IAM role that restricts who can assume the role.
sirExternalId :: Lens' SegmentImportResource (Maybe Text)
sirExternalId = lens _sirExternalId (\ s a -> s{_sirExternalId = a});

-- | A URL that points to the Amazon S3 location from which the endpoints for this segment were imported.
sirS3URL :: Lens' SegmentImportResource (Maybe Text)
sirS3URL = lens _sirS3URL (\ s a -> s{_sirS3URL = a});

-- | The Amazon Resource Name (ARN) of an IAM role that grants Amazon Pinpoint access to the endpoints in Amazon S3.
sirRoleARN :: Lens' SegmentImportResource (Maybe Text)
sirRoleARN = lens _sirRoleARN (\ s a -> s{_sirRoleARN = a});

instance FromJSON SegmentImportResource where
        parseJSON
          = withObject "SegmentImportResource"
              (\ x ->
                 SegmentImportResource' <$>
                   (x .:? "Size") <*> (x .:? "Format") <*>
                     (x .:? "ExternalId")
                     <*> (x .:? "S3Url")
                     <*> (x .:? "RoleArn"))

instance Hashable SegmentImportResource

instance NFData SegmentImportResource

-- | /See:/ 'segmentLocation' smart constructor.
newtype SegmentLocation = SegmentLocation'
    { _slCountry :: Maybe SetDimension
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SegmentLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slCountry' - The country filter according to ISO 3166-1 Alpha-2 codes.
segmentLocation
    :: SegmentLocation
segmentLocation =
    SegmentLocation'
    { _slCountry = Nothing
    }

-- | The country filter according to ISO 3166-1 Alpha-2 codes.
slCountry :: Lens' SegmentLocation (Maybe SetDimension)
slCountry = lens _slCountry (\ s a -> s{_slCountry = a});

instance FromJSON SegmentLocation where
        parseJSON
          = withObject "SegmentLocation"
              (\ x -> SegmentLocation' <$> (x .:? "Country"))

instance Hashable SegmentLocation

instance NFData SegmentLocation

instance ToJSON SegmentLocation where
        toJSON SegmentLocation'{..}
          = object (catMaybes [("Country" .=) <$> _slCountry])

-- | /See:/ 'segmentResponse' smart constructor.
data SegmentResponse = SegmentResponse'
    { _sLastModifiedDate :: !(Maybe Text)
    , _sSegmentType      :: !(Maybe SegmentType)
    , _sApplicationId    :: !(Maybe Text)
    , _sName             :: !(Maybe Text)
    , _sVersion          :: !(Maybe Int)
    , _sId               :: !(Maybe Text)
    , _sCreationDate     :: !(Maybe Text)
    , _sImportDefinition :: !(Maybe SegmentImportResource)
    , _sDimensions       :: !(Maybe SegmentDimensions)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SegmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sLastModifiedDate' - The date the segment was last updated in ISO 8601 format.
--
-- * 'sSegmentType' - The segment type: DIMENSIONAL – A dynamic segment built from selection criteria based on endpoint data reported by your app. You create this type of segment by using the segment builder in the Amazon Pinpoint console or by making a POST request to the segments resource. IMPORT – A static segment built from an imported set of endpoint definitions. You create this type of segment by importing a segment in the Amazon Pinpoint console or by making a POST request to the jobs/import resource.
--
-- * 'sApplicationId' - The ID of the application to which the segment applies.
--
-- * 'sName' - The name of segment
--
-- * 'sVersion' - The segment version number.
--
-- * 'sId' - The unique segment ID.
--
-- * 'sCreationDate' - The date the segment was created in ISO 8601 format.
--
-- * 'sImportDefinition' - The import job settings.
--
-- * 'sDimensions' - The segment dimensions attributes.
segmentResponse
    :: SegmentResponse
segmentResponse =
    SegmentResponse'
    { _sLastModifiedDate = Nothing
    , _sSegmentType = Nothing
    , _sApplicationId = Nothing
    , _sName = Nothing
    , _sVersion = Nothing
    , _sId = Nothing
    , _sCreationDate = Nothing
    , _sImportDefinition = Nothing
    , _sDimensions = Nothing
    }

-- | The date the segment was last updated in ISO 8601 format.
sLastModifiedDate :: Lens' SegmentResponse (Maybe Text)
sLastModifiedDate = lens _sLastModifiedDate (\ s a -> s{_sLastModifiedDate = a});

-- | The segment type: DIMENSIONAL – A dynamic segment built from selection criteria based on endpoint data reported by your app. You create this type of segment by using the segment builder in the Amazon Pinpoint console or by making a POST request to the segments resource. IMPORT – A static segment built from an imported set of endpoint definitions. You create this type of segment by importing a segment in the Amazon Pinpoint console or by making a POST request to the jobs/import resource.
sSegmentType :: Lens' SegmentResponse (Maybe SegmentType)
sSegmentType = lens _sSegmentType (\ s a -> s{_sSegmentType = a});

-- | The ID of the application to which the segment applies.
sApplicationId :: Lens' SegmentResponse (Maybe Text)
sApplicationId = lens _sApplicationId (\ s a -> s{_sApplicationId = a});

-- | The name of segment
sName :: Lens' SegmentResponse (Maybe Text)
sName = lens _sName (\ s a -> s{_sName = a});

-- | The segment version number.
sVersion :: Lens' SegmentResponse (Maybe Int)
sVersion = lens _sVersion (\ s a -> s{_sVersion = a});

-- | The unique segment ID.
sId :: Lens' SegmentResponse (Maybe Text)
sId = lens _sId (\ s a -> s{_sId = a});

-- | The date the segment was created in ISO 8601 format.
sCreationDate :: Lens' SegmentResponse (Maybe Text)
sCreationDate = lens _sCreationDate (\ s a -> s{_sCreationDate = a});

-- | The import job settings.
sImportDefinition :: Lens' SegmentResponse (Maybe SegmentImportResource)
sImportDefinition = lens _sImportDefinition (\ s a -> s{_sImportDefinition = a});

-- | The segment dimensions attributes.
sDimensions :: Lens' SegmentResponse (Maybe SegmentDimensions)
sDimensions = lens _sDimensions (\ s a -> s{_sDimensions = a});

instance FromJSON SegmentResponse where
        parseJSON
          = withObject "SegmentResponse"
              (\ x ->
                 SegmentResponse' <$>
                   (x .:? "LastModifiedDate") <*> (x .:? "SegmentType")
                     <*> (x .:? "ApplicationId")
                     <*> (x .:? "Name")
                     <*> (x .:? "Version")
                     <*> (x .:? "Id")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "ImportDefinition")
                     <*> (x .:? "Dimensions"))

instance Hashable SegmentResponse

instance NFData SegmentResponse

-- | /See:/ 'segmentsResponse' smart constructor.
data SegmentsResponse = SegmentsResponse'
    { _sNextToken :: !(Maybe Text)
    , _sItem      :: !(Maybe [SegmentResponse])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SegmentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sNextToken' - An identifier used to retrieve the next page of results. The token is null if no additional pages exist.
--
-- * 'sItem' - The list of segments.
segmentsResponse
    :: SegmentsResponse
segmentsResponse =
    SegmentsResponse'
    { _sNextToken = Nothing
    , _sItem = Nothing
    }

-- | An identifier used to retrieve the next page of results. The token is null if no additional pages exist.
sNextToken :: Lens' SegmentsResponse (Maybe Text)
sNextToken = lens _sNextToken (\ s a -> s{_sNextToken = a});

-- | The list of segments.
sItem :: Lens' SegmentsResponse [SegmentResponse]
sItem = lens _sItem (\ s a -> s{_sItem = a}) . _Default . _Coerce;

instance FromJSON SegmentsResponse where
        parseJSON
          = withObject "SegmentsResponse"
              (\ x ->
                 SegmentsResponse' <$>
                   (x .:? "NextToken") <*> (x .:? "Item" .!= mempty))

instance Hashable SegmentsResponse

instance NFData SegmentsResponse

-- | /See:/ 'setDimension' smart constructor.
data SetDimension = SetDimension'
    { _sdValues        :: !(Maybe [Text])
    , _sdDimensionType :: !(Maybe DimensionType)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetDimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdValues' - The criteria values for the segment dimension. Endpoints with matching attribute values are included or excluded from the segment, depending on the setting for Type.
--
-- * 'sdDimensionType' - The type of dimension: INCLUSIVE – Endpoints that match the criteria are included in the segment. EXCLUSIVE – Endpoints that match the criteria are excluded from the segment.
setDimension
    :: SetDimension
setDimension =
    SetDimension'
    { _sdValues = Nothing
    , _sdDimensionType = Nothing
    }

-- | The criteria values for the segment dimension. Endpoints with matching attribute values are included or excluded from the segment, depending on the setting for Type.
sdValues :: Lens' SetDimension [Text]
sdValues = lens _sdValues (\ s a -> s{_sdValues = a}) . _Default . _Coerce;

-- | The type of dimension: INCLUSIVE – Endpoints that match the criteria are included in the segment. EXCLUSIVE – Endpoints that match the criteria are excluded from the segment.
sdDimensionType :: Lens' SetDimension (Maybe DimensionType)
sdDimensionType = lens _sdDimensionType (\ s a -> s{_sdDimensionType = a});

instance FromJSON SetDimension where
        parseJSON
          = withObject "SetDimension"
              (\ x ->
                 SetDimension' <$>
                   (x .:? "Values" .!= mempty) <*>
                     (x .:? "DimensionType"))

instance Hashable SetDimension

instance NFData SetDimension

instance ToJSON SetDimension where
        toJSON SetDimension'{..}
          = object
              (catMaybes
                 [("Values" .=) <$> _sdValues,
                  ("DimensionType" .=) <$> _sdDimensionType])

-- | /See:/ 'treatmentResource' smart constructor.
data TreatmentResource = TreatmentResource'
    { _trState                :: !(Maybe CampaignState)
    , _trSchedule             :: !(Maybe Schedule)
    , _trTreatmentName        :: !(Maybe Text)
    , _trSizePercent          :: !(Maybe Int)
    , _trTreatmentDescription :: !(Maybe Text)
    , _trId                   :: !(Maybe Text)
    , _trMessageConfiguration :: !(Maybe MessageConfiguration)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TreatmentResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trState' - The treatment status.
--
-- * 'trSchedule' - The campaign schedule.
--
-- * 'trTreatmentName' - The custom name of a variation of the campaign used for A/B testing.
--
-- * 'trSizePercent' - The allocated percentage of users for this treatment.
--
-- * 'trTreatmentDescription' - A custom description for the treatment.
--
-- * 'trId' - The unique treatment ID.
--
-- * 'trMessageConfiguration' - The message configuration settings.
treatmentResource
    :: TreatmentResource
treatmentResource =
    TreatmentResource'
    { _trState = Nothing
    , _trSchedule = Nothing
    , _trTreatmentName = Nothing
    , _trSizePercent = Nothing
    , _trTreatmentDescription = Nothing
    , _trId = Nothing
    , _trMessageConfiguration = Nothing
    }

-- | The treatment status.
trState :: Lens' TreatmentResource (Maybe CampaignState)
trState = lens _trState (\ s a -> s{_trState = a});

-- | The campaign schedule.
trSchedule :: Lens' TreatmentResource (Maybe Schedule)
trSchedule = lens _trSchedule (\ s a -> s{_trSchedule = a});

-- | The custom name of a variation of the campaign used for A/B testing.
trTreatmentName :: Lens' TreatmentResource (Maybe Text)
trTreatmentName = lens _trTreatmentName (\ s a -> s{_trTreatmentName = a});

-- | The allocated percentage of users for this treatment.
trSizePercent :: Lens' TreatmentResource (Maybe Int)
trSizePercent = lens _trSizePercent (\ s a -> s{_trSizePercent = a});

-- | A custom description for the treatment.
trTreatmentDescription :: Lens' TreatmentResource (Maybe Text)
trTreatmentDescription = lens _trTreatmentDescription (\ s a -> s{_trTreatmentDescription = a});

-- | The unique treatment ID.
trId :: Lens' TreatmentResource (Maybe Text)
trId = lens _trId (\ s a -> s{_trId = a});

-- | The message configuration settings.
trMessageConfiguration :: Lens' TreatmentResource (Maybe MessageConfiguration)
trMessageConfiguration = lens _trMessageConfiguration (\ s a -> s{_trMessageConfiguration = a});

instance FromJSON TreatmentResource where
        parseJSON
          = withObject "TreatmentResource"
              (\ x ->
                 TreatmentResource' <$>
                   (x .:? "State") <*> (x .:? "Schedule") <*>
                     (x .:? "TreatmentName")
                     <*> (x .:? "SizePercent")
                     <*> (x .:? "TreatmentDescription")
                     <*> (x .:? "Id")
                     <*> (x .:? "MessageConfiguration"))

instance Hashable TreatmentResource

instance NFData TreatmentResource

-- | /See:/ 'writeApplicationSettingsRequest' smart constructor.
data WriteApplicationSettingsRequest = WriteApplicationSettingsRequest'
    { _wasrLimits    :: !(Maybe CampaignLimits)
    , _wasrQuietTime :: !(Maybe QuietTime)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'WriteApplicationSettingsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wasrLimits' - The default campaign limits for the app. These limits apply to each campaign for the app, unless the campaign overrides the default with limits of its own.
--
-- * 'wasrQuietTime' - The default quiet time for the app. Each campaign for this app sends no messages during this time unless the campaign overrides the default with a quiet time of its own.
writeApplicationSettingsRequest
    :: WriteApplicationSettingsRequest
writeApplicationSettingsRequest =
    WriteApplicationSettingsRequest'
    { _wasrLimits = Nothing
    , _wasrQuietTime = Nothing
    }

-- | The default campaign limits for the app. These limits apply to each campaign for the app, unless the campaign overrides the default with limits of its own.
wasrLimits :: Lens' WriteApplicationSettingsRequest (Maybe CampaignLimits)
wasrLimits = lens _wasrLimits (\ s a -> s{_wasrLimits = a});

-- | The default quiet time for the app. Each campaign for this app sends no messages during this time unless the campaign overrides the default with a quiet time of its own.
wasrQuietTime :: Lens' WriteApplicationSettingsRequest (Maybe QuietTime)
wasrQuietTime = lens _wasrQuietTime (\ s a -> s{_wasrQuietTime = a});

instance Hashable WriteApplicationSettingsRequest

instance NFData WriteApplicationSettingsRequest

instance ToJSON WriteApplicationSettingsRequest where
        toJSON WriteApplicationSettingsRequest'{..}
          = object
              (catMaybes
                 [("Limits" .=) <$> _wasrLimits,
                  ("QuietTime" .=) <$> _wasrQuietTime])

-- | /See:/ 'writeCampaignRequest' smart constructor.
data WriteCampaignRequest = WriteCampaignRequest'
    { _wcrSchedule             :: !(Maybe Schedule)
    , _wcrTreatmentName        :: !(Maybe Text)
    , _wcrLimits               :: !(Maybe CampaignLimits)
    , _wcrIsPaused             :: !(Maybe Bool)
    , _wcrName                 :: !(Maybe Text)
    , _wcrHoldoutPercent       :: !(Maybe Int)
    , _wcrTreatmentDescription :: !(Maybe Text)
    , _wcrMessageConfiguration :: !(Maybe MessageConfiguration)
    , _wcrDescription          :: !(Maybe Text)
    , _wcrSegmentId            :: !(Maybe Text)
    , _wcrAdditionalTreatments :: !(Maybe [WriteTreatmentResource])
    , _wcrSegmentVersion       :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'WriteCampaignRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wcrSchedule' - The campaign schedule.
--
-- * 'wcrTreatmentName' - The custom name of a variation of the campaign used for A/B testing.
--
-- * 'wcrLimits' - The campaign limits settings.
--
-- * 'wcrIsPaused' - Indicates whether the campaign is paused. A paused campaign does not send messages unless you resume it by setting IsPaused to false.
--
-- * 'wcrName' - The custom name of the campaign.
--
-- * 'wcrHoldoutPercent' - The allocated percentage of end users who will not receive messages from this campaign.
--
-- * 'wcrTreatmentDescription' - A custom description for the treatment.
--
-- * 'wcrMessageConfiguration' - The message configuration settings.
--
-- * 'wcrDescription' - A description of the campaign.
--
-- * 'wcrSegmentId' - The ID of the segment to which the campaign sends messages.
--
-- * 'wcrAdditionalTreatments' - Treatments that are defined in addition to the default treatment.
--
-- * 'wcrSegmentVersion' - The version of the segment to which the campaign sends messages.
writeCampaignRequest
    :: WriteCampaignRequest
writeCampaignRequest =
    WriteCampaignRequest'
    { _wcrSchedule = Nothing
    , _wcrTreatmentName = Nothing
    , _wcrLimits = Nothing
    , _wcrIsPaused = Nothing
    , _wcrName = Nothing
    , _wcrHoldoutPercent = Nothing
    , _wcrTreatmentDescription = Nothing
    , _wcrMessageConfiguration = Nothing
    , _wcrDescription = Nothing
    , _wcrSegmentId = Nothing
    , _wcrAdditionalTreatments = Nothing
    , _wcrSegmentVersion = Nothing
    }

-- | The campaign schedule.
wcrSchedule :: Lens' WriteCampaignRequest (Maybe Schedule)
wcrSchedule = lens _wcrSchedule (\ s a -> s{_wcrSchedule = a});

-- | The custom name of a variation of the campaign used for A/B testing.
wcrTreatmentName :: Lens' WriteCampaignRequest (Maybe Text)
wcrTreatmentName = lens _wcrTreatmentName (\ s a -> s{_wcrTreatmentName = a});

-- | The campaign limits settings.
wcrLimits :: Lens' WriteCampaignRequest (Maybe CampaignLimits)
wcrLimits = lens _wcrLimits (\ s a -> s{_wcrLimits = a});

-- | Indicates whether the campaign is paused. A paused campaign does not send messages unless you resume it by setting IsPaused to false.
wcrIsPaused :: Lens' WriteCampaignRequest (Maybe Bool)
wcrIsPaused = lens _wcrIsPaused (\ s a -> s{_wcrIsPaused = a});

-- | The custom name of the campaign.
wcrName :: Lens' WriteCampaignRequest (Maybe Text)
wcrName = lens _wcrName (\ s a -> s{_wcrName = a});

-- | The allocated percentage of end users who will not receive messages from this campaign.
wcrHoldoutPercent :: Lens' WriteCampaignRequest (Maybe Int)
wcrHoldoutPercent = lens _wcrHoldoutPercent (\ s a -> s{_wcrHoldoutPercent = a});

-- | A custom description for the treatment.
wcrTreatmentDescription :: Lens' WriteCampaignRequest (Maybe Text)
wcrTreatmentDescription = lens _wcrTreatmentDescription (\ s a -> s{_wcrTreatmentDescription = a});

-- | The message configuration settings.
wcrMessageConfiguration :: Lens' WriteCampaignRequest (Maybe MessageConfiguration)
wcrMessageConfiguration = lens _wcrMessageConfiguration (\ s a -> s{_wcrMessageConfiguration = a});

-- | A description of the campaign.
wcrDescription :: Lens' WriteCampaignRequest (Maybe Text)
wcrDescription = lens _wcrDescription (\ s a -> s{_wcrDescription = a});

-- | The ID of the segment to which the campaign sends messages.
wcrSegmentId :: Lens' WriteCampaignRequest (Maybe Text)
wcrSegmentId = lens _wcrSegmentId (\ s a -> s{_wcrSegmentId = a});

-- | Treatments that are defined in addition to the default treatment.
wcrAdditionalTreatments :: Lens' WriteCampaignRequest [WriteTreatmentResource]
wcrAdditionalTreatments = lens _wcrAdditionalTreatments (\ s a -> s{_wcrAdditionalTreatments = a}) . _Default . _Coerce;

-- | The version of the segment to which the campaign sends messages.
wcrSegmentVersion :: Lens' WriteCampaignRequest (Maybe Int)
wcrSegmentVersion = lens _wcrSegmentVersion (\ s a -> s{_wcrSegmentVersion = a});

instance Hashable WriteCampaignRequest

instance NFData WriteCampaignRequest

instance ToJSON WriteCampaignRequest where
        toJSON WriteCampaignRequest'{..}
          = object
              (catMaybes
                 [("Schedule" .=) <$> _wcrSchedule,
                  ("TreatmentName" .=) <$> _wcrTreatmentName,
                  ("Limits" .=) <$> _wcrLimits,
                  ("IsPaused" .=) <$> _wcrIsPaused,
                  ("Name" .=) <$> _wcrName,
                  ("HoldoutPercent" .=) <$> _wcrHoldoutPercent,
                  ("TreatmentDescription" .=) <$>
                    _wcrTreatmentDescription,
                  ("MessageConfiguration" .=) <$>
                    _wcrMessageConfiguration,
                  ("Description" .=) <$> _wcrDescription,
                  ("SegmentId" .=) <$> _wcrSegmentId,
                  ("AdditionalTreatments" .=) <$>
                    _wcrAdditionalTreatments,
                  ("SegmentVersion" .=) <$> _wcrSegmentVersion])

-- | /See:/ 'writeSegmentRequest' smart constructor.
data WriteSegmentRequest = WriteSegmentRequest'
    { _wsrName       :: !(Maybe Text)
    , _wsrDimensions :: !(Maybe SegmentDimensions)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'WriteSegmentRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wsrName' - The name of segment
--
-- * 'wsrDimensions' - The segment dimensions attributes.
writeSegmentRequest
    :: WriteSegmentRequest
writeSegmentRequest =
    WriteSegmentRequest'
    { _wsrName = Nothing
    , _wsrDimensions = Nothing
    }

-- | The name of segment
wsrName :: Lens' WriteSegmentRequest (Maybe Text)
wsrName = lens _wsrName (\ s a -> s{_wsrName = a});

-- | The segment dimensions attributes.
wsrDimensions :: Lens' WriteSegmentRequest (Maybe SegmentDimensions)
wsrDimensions = lens _wsrDimensions (\ s a -> s{_wsrDimensions = a});

instance Hashable WriteSegmentRequest

instance NFData WriteSegmentRequest

instance ToJSON WriteSegmentRequest where
        toJSON WriteSegmentRequest'{..}
          = object
              (catMaybes
                 [("Name" .=) <$> _wsrName,
                  ("Dimensions" .=) <$> _wsrDimensions])

-- | /See:/ 'writeTreatmentResource' smart constructor.
data WriteTreatmentResource = WriteTreatmentResource'
    { _wtrSchedule             :: !(Maybe Schedule)
    , _wtrTreatmentName        :: !(Maybe Text)
    , _wtrSizePercent          :: !(Maybe Int)
    , _wtrTreatmentDescription :: !(Maybe Text)
    , _wtrMessageConfiguration :: !(Maybe MessageConfiguration)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'WriteTreatmentResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wtrSchedule' - The campaign schedule.
--
-- * 'wtrTreatmentName' - The custom name of a variation of the campaign used for A/B testing.
--
-- * 'wtrSizePercent' - The allocated percentage of users for this treatment.
--
-- * 'wtrTreatmentDescription' - A custom description for the treatment.
--
-- * 'wtrMessageConfiguration' - The message configuration settings.
writeTreatmentResource
    :: WriteTreatmentResource
writeTreatmentResource =
    WriteTreatmentResource'
    { _wtrSchedule = Nothing
    , _wtrTreatmentName = Nothing
    , _wtrSizePercent = Nothing
    , _wtrTreatmentDescription = Nothing
    , _wtrMessageConfiguration = Nothing
    }

-- | The campaign schedule.
wtrSchedule :: Lens' WriteTreatmentResource (Maybe Schedule)
wtrSchedule = lens _wtrSchedule (\ s a -> s{_wtrSchedule = a});

-- | The custom name of a variation of the campaign used for A/B testing.
wtrTreatmentName :: Lens' WriteTreatmentResource (Maybe Text)
wtrTreatmentName = lens _wtrTreatmentName (\ s a -> s{_wtrTreatmentName = a});

-- | The allocated percentage of users for this treatment.
wtrSizePercent :: Lens' WriteTreatmentResource (Maybe Int)
wtrSizePercent = lens _wtrSizePercent (\ s a -> s{_wtrSizePercent = a});

-- | A custom description for the treatment.
wtrTreatmentDescription :: Lens' WriteTreatmentResource (Maybe Text)
wtrTreatmentDescription = lens _wtrTreatmentDescription (\ s a -> s{_wtrTreatmentDescription = a});

-- | The message configuration settings.
wtrMessageConfiguration :: Lens' WriteTreatmentResource (Maybe MessageConfiguration)
wtrMessageConfiguration = lens _wtrMessageConfiguration (\ s a -> s{_wtrMessageConfiguration = a});

instance Hashable WriteTreatmentResource

instance NFData WriteTreatmentResource

instance ToJSON WriteTreatmentResource where
        toJSON WriteTreatmentResource'{..}
          = object
              (catMaybes
                 [("Schedule" .=) <$> _wtrSchedule,
                  ("TreatmentName" .=) <$> _wtrTreatmentName,
                  ("SizePercent" .=) <$> _wtrSizePercent,
                  ("TreatmentDescription" .=) <$>
                    _wtrTreatmentDescription,
                  ("MessageConfiguration" .=) <$>
                    _wtrMessageConfiguration])
