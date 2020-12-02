{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigurationItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigurationItem where

import Network.AWS.Config.Types.ConfigurationItemStatus
import Network.AWS.Config.Types.Relationship
import Network.AWS.Config.Types.ResourceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list that contains detailed configurations of a specified resource.
--
--
--
-- /See:/ 'configurationItem' smart constructor.
data ConfigurationItem = ConfigurationItem'
  { _ciResourceId ::
      !(Maybe Text),
    _ciResourceType :: !(Maybe ResourceType),
    _ciConfigurationStateId :: !(Maybe Text),
    _ciArn :: !(Maybe Text),
    _ciResourceName :: !(Maybe Text),
    _ciResourceCreationTime :: !(Maybe POSIX),
    _ciConfigurationItemStatus ::
      !(Maybe ConfigurationItemStatus),
    _ciConfigurationItemCaptureTime :: !(Maybe POSIX),
    _ciAccountId :: !(Maybe Text),
    _ciSupplementaryConfiguration ::
      !(Maybe (Map Text (Text))),
    _ciAvailabilityZone :: !(Maybe Text),
    _ciRelationships :: !(Maybe [Relationship]),
    _ciVersion :: !(Maybe Text),
    _ciAwsRegion :: !(Maybe Text),
    _ciRelatedEvents :: !(Maybe [Text]),
    _ciConfiguration :: !(Maybe Text),
    _ciConfigurationItemMD5Hash :: !(Maybe Text),
    _ciTags :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConfigurationItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciResourceId' - The ID of the resource (for example, @sg-xxxxxx@ ).
--
-- * 'ciResourceType' - The type of AWS resource.
--
-- * 'ciConfigurationStateId' - An identifier that indicates the ordering of the configuration items of a resource.
--
-- * 'ciArn' - accoun
--
-- * 'ciResourceName' - The custom name of the resource, if available.
--
-- * 'ciResourceCreationTime' - The time stamp when the resource was created.
--
-- * 'ciConfigurationItemStatus' - The configuration item status. The valid values are:     * OK – The resource configuration has been updated     * ResourceDiscovered – The resource was newly discovered     * ResourceNotRecorded – The resource was discovered but its configuration was not recorded since the recorder excludes the recording of resources of this type     * ResourceDeleted – The resource was deleted     * ResourceDeletedNotRecorded – The resource was deleted but its configuration was not recorded since the recorder excludes the recording of resources of this type
--
-- * 'ciConfigurationItemCaptureTime' - The time when the configuration recording was initiated.
--
-- * 'ciAccountId' - The 12-digit AWS account ID associated with the resource.
--
-- * 'ciSupplementaryConfiguration' - Configuration attributes that AWS Config returns for certain resource types to supplement the information returned for the @configuration@ parameter.
--
-- * 'ciAvailabilityZone' - The Availability Zone associated with the resource.
--
-- * 'ciRelationships' - A list of related AWS resources.
--
-- * 'ciVersion' - The version number of the resource configuration.
--
-- * 'ciAwsRegion' - The region where the resource resides.
--
-- * 'ciRelatedEvents' - A list of CloudTrail event IDs. A populated field indicates that the current configuration was initiated by the events recorded in the CloudTrail log. For more information about CloudTrail, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/what_is_cloud_trail_top_level.html What Is AWS CloudTrail> . An empty field indicates that the current configuration was not initiated by any event. As of Version 1.3, the relatedEvents field is empty. You can access the <https://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_LookupEvents.html LookupEvents API> in the /AWS CloudTrail API Reference/ to retrieve the events for the resource.
--
-- * 'ciConfiguration' - The description of the resource configuration.
--
-- * 'ciConfigurationItemMD5Hash' - Unique MD5 hash that represents the configuration item's state. You can use MD5 hash to compare the states of two or more configuration items that are associated with the same resource.
--
-- * 'ciTags' - A mapping of key value tags associated with the resource.
configurationItem ::
  ConfigurationItem
configurationItem =
  ConfigurationItem'
    { _ciResourceId = Nothing,
      _ciResourceType = Nothing,
      _ciConfigurationStateId = Nothing,
      _ciArn = Nothing,
      _ciResourceName = Nothing,
      _ciResourceCreationTime = Nothing,
      _ciConfigurationItemStatus = Nothing,
      _ciConfigurationItemCaptureTime = Nothing,
      _ciAccountId = Nothing,
      _ciSupplementaryConfiguration = Nothing,
      _ciAvailabilityZone = Nothing,
      _ciRelationships = Nothing,
      _ciVersion = Nothing,
      _ciAwsRegion = Nothing,
      _ciRelatedEvents = Nothing,
      _ciConfiguration = Nothing,
      _ciConfigurationItemMD5Hash = Nothing,
      _ciTags = Nothing
    }

-- | The ID of the resource (for example, @sg-xxxxxx@ ).
ciResourceId :: Lens' ConfigurationItem (Maybe Text)
ciResourceId = lens _ciResourceId (\s a -> s {_ciResourceId = a})

-- | The type of AWS resource.
ciResourceType :: Lens' ConfigurationItem (Maybe ResourceType)
ciResourceType = lens _ciResourceType (\s a -> s {_ciResourceType = a})

-- | An identifier that indicates the ordering of the configuration items of a resource.
ciConfigurationStateId :: Lens' ConfigurationItem (Maybe Text)
ciConfigurationStateId = lens _ciConfigurationStateId (\s a -> s {_ciConfigurationStateId = a})

-- | accoun
ciArn :: Lens' ConfigurationItem (Maybe Text)
ciArn = lens _ciArn (\s a -> s {_ciArn = a})

-- | The custom name of the resource, if available.
ciResourceName :: Lens' ConfigurationItem (Maybe Text)
ciResourceName = lens _ciResourceName (\s a -> s {_ciResourceName = a})

-- | The time stamp when the resource was created.
ciResourceCreationTime :: Lens' ConfigurationItem (Maybe UTCTime)
ciResourceCreationTime = lens _ciResourceCreationTime (\s a -> s {_ciResourceCreationTime = a}) . mapping _Time

-- | The configuration item status. The valid values are:     * OK – The resource configuration has been updated     * ResourceDiscovered – The resource was newly discovered     * ResourceNotRecorded – The resource was discovered but its configuration was not recorded since the recorder excludes the recording of resources of this type     * ResourceDeleted – The resource was deleted     * ResourceDeletedNotRecorded – The resource was deleted but its configuration was not recorded since the recorder excludes the recording of resources of this type
ciConfigurationItemStatus :: Lens' ConfigurationItem (Maybe ConfigurationItemStatus)
ciConfigurationItemStatus = lens _ciConfigurationItemStatus (\s a -> s {_ciConfigurationItemStatus = a})

-- | The time when the configuration recording was initiated.
ciConfigurationItemCaptureTime :: Lens' ConfigurationItem (Maybe UTCTime)
ciConfigurationItemCaptureTime = lens _ciConfigurationItemCaptureTime (\s a -> s {_ciConfigurationItemCaptureTime = a}) . mapping _Time

-- | The 12-digit AWS account ID associated with the resource.
ciAccountId :: Lens' ConfigurationItem (Maybe Text)
ciAccountId = lens _ciAccountId (\s a -> s {_ciAccountId = a})

-- | Configuration attributes that AWS Config returns for certain resource types to supplement the information returned for the @configuration@ parameter.
ciSupplementaryConfiguration :: Lens' ConfigurationItem (HashMap Text (Text))
ciSupplementaryConfiguration = lens _ciSupplementaryConfiguration (\s a -> s {_ciSupplementaryConfiguration = a}) . _Default . _Map

-- | The Availability Zone associated with the resource.
ciAvailabilityZone :: Lens' ConfigurationItem (Maybe Text)
ciAvailabilityZone = lens _ciAvailabilityZone (\s a -> s {_ciAvailabilityZone = a})

-- | A list of related AWS resources.
ciRelationships :: Lens' ConfigurationItem [Relationship]
ciRelationships = lens _ciRelationships (\s a -> s {_ciRelationships = a}) . _Default . _Coerce

-- | The version number of the resource configuration.
ciVersion :: Lens' ConfigurationItem (Maybe Text)
ciVersion = lens _ciVersion (\s a -> s {_ciVersion = a})

-- | The region where the resource resides.
ciAwsRegion :: Lens' ConfigurationItem (Maybe Text)
ciAwsRegion = lens _ciAwsRegion (\s a -> s {_ciAwsRegion = a})

-- | A list of CloudTrail event IDs. A populated field indicates that the current configuration was initiated by the events recorded in the CloudTrail log. For more information about CloudTrail, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/what_is_cloud_trail_top_level.html What Is AWS CloudTrail> . An empty field indicates that the current configuration was not initiated by any event. As of Version 1.3, the relatedEvents field is empty. You can access the <https://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_LookupEvents.html LookupEvents API> in the /AWS CloudTrail API Reference/ to retrieve the events for the resource.
ciRelatedEvents :: Lens' ConfigurationItem [Text]
ciRelatedEvents = lens _ciRelatedEvents (\s a -> s {_ciRelatedEvents = a}) . _Default . _Coerce

-- | The description of the resource configuration.
ciConfiguration :: Lens' ConfigurationItem (Maybe Text)
ciConfiguration = lens _ciConfiguration (\s a -> s {_ciConfiguration = a})

-- | Unique MD5 hash that represents the configuration item's state. You can use MD5 hash to compare the states of two or more configuration items that are associated with the same resource.
ciConfigurationItemMD5Hash :: Lens' ConfigurationItem (Maybe Text)
ciConfigurationItemMD5Hash = lens _ciConfigurationItemMD5Hash (\s a -> s {_ciConfigurationItemMD5Hash = a})

-- | A mapping of key value tags associated with the resource.
ciTags :: Lens' ConfigurationItem (HashMap Text (Text))
ciTags = lens _ciTags (\s a -> s {_ciTags = a}) . _Default . _Map

instance FromJSON ConfigurationItem where
  parseJSON =
    withObject
      "ConfigurationItem"
      ( \x ->
          ConfigurationItem'
            <$> (x .:? "resourceId")
            <*> (x .:? "resourceType")
            <*> (x .:? "configurationStateId")
            <*> (x .:? "arn")
            <*> (x .:? "resourceName")
            <*> (x .:? "resourceCreationTime")
            <*> (x .:? "configurationItemStatus")
            <*> (x .:? "configurationItemCaptureTime")
            <*> (x .:? "accountId")
            <*> (x .:? "supplementaryConfiguration" .!= mempty)
            <*> (x .:? "availabilityZone")
            <*> (x .:? "relationships" .!= mempty)
            <*> (x .:? "version")
            <*> (x .:? "awsRegion")
            <*> (x .:? "relatedEvents" .!= mempty)
            <*> (x .:? "configuration")
            <*> (x .:? "configurationItemMD5Hash")
            <*> (x .:? "tags" .!= mempty)
      )

instance Hashable ConfigurationItem

instance NFData ConfigurationItem
