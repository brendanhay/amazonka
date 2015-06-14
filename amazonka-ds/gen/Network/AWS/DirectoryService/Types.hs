{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.DirectoryService.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.DirectoryService.Types
    (
    -- * Service
      DirectoryService
    -- ** Errors
    , JSONError

    -- * Attribute
    , Attribute
    , attribute
    , attValue
    , attName

    -- * Computer
    , Computer
    , computer
    , comComputerAttributes
    , comComputerId
    , comComputerName

    -- * DirectoryConnectSettings
    , DirectoryConnectSettings
    , directoryConnectSettings
    , dcsVPCId
    , dcsSubnetIds
    , dcsCustomerDNSIPs
    , dcsCustomerUserName

    -- * DirectoryConnectSettingsDescription
    , DirectoryConnectSettingsDescription
    , directoryConnectSettingsDescription
    , dcsdSubnetIds
    , dcsdVPCId
    , dcsdConnectIPs
    , dcsdSecurityGroupId
    , dcsdAvailabilityZones
    , dcsdCustomerUserName

    -- * DirectoryDescription
    , DirectoryDescription
    , directoryDescription
    , ddRadiusStatus
    , ddDirectoryId
    , ddStage
    , ddShortName
    , ddSize
    , ddRadiusSettings
    , ddLaunchTime
    , ddName
    , ddSsoEnabled
    , ddStageLastUpdatedDateTime
    , ddStageReason
    , ddDNSIPAddrs
    , ddVPCSettings
    , ddType
    , ddConnectSettings
    , ddDescription
    , ddAccessURL
    , ddAlias

    -- * DirectoryLimits
    , DirectoryLimits
    , directoryLimits
    , dlConnectedDirectoriesCurrentCount
    , dlConnectedDirectoriesLimit
    , dlConnectedDirectoriesLimitReached
    , dlCloudOnlyDirectoriesLimit
    , dlCloudOnlyDirectoriesCurrentCount
    , dlCloudOnlyDirectoriesLimitReached

    -- * DirectorySize
    , DirectorySize (..)

    -- * DirectoryStage
    , DirectoryStage (..)

    -- * DirectoryType
    , DirectoryType (..)

    -- * DirectoryVPCSettings
    , DirectoryVPCSettings
    , directoryVPCSettings
    , dvsVPCId
    , dvsSubnetIds

    -- * DirectoryVPCSettingsDescription
    , DirectoryVPCSettingsDescription
    , directoryVPCSettingsDescription
    , dvsdSubnetIds
    , dvsdVPCId
    , dvsdSecurityGroupId
    , dvsdAvailabilityZones

    -- * RadiusAuthenticationProtocol
    , RadiusAuthenticationProtocol (..)

    -- * RadiusSettings
    , RadiusSettings
    , radiusSettings
    , rsRadiusServers
    , rsRadiusRetries
    , rsAuthenticationProtocol
    , rsUseSameUsername
    , rsDisplayLabel
    , rsSharedSecret
    , rsRadiusTimeout
    , rsRadiusPort

    -- * RadiusStatus
    , RadiusStatus (..)

    -- * Snapshot
    , Snapshot
    , snapshot
    , snaDirectoryId
    , snaStatus
    , snaStartTime
    , snaName
    , snaType
    , snaSnapshotId

    -- * SnapshotLimits
    , SnapshotLimits
    , snapshotLimits
    , slManualSnapshotsLimitReached
    , slManualSnapshotsCurrentCount
    , slManualSnapshotsLimit

    -- * SnapshotStatus
    , SnapshotStatus (..)

    -- * SnapshotType
    , SnapshotType (..)
    ) where

import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | Version @2015-04-16@ of the Amazon Directory Service SDK.
data DirectoryService

instance AWSService DirectoryService where
    type Sg DirectoryService = V4
    type Er DirectoryService = JSONError

    service = service'
      where
        service' :: Service DirectoryService
        service' = Service
            { _svcAbbrev  = "DirectoryService"
            , _svcPrefix  = "ds"
            , _svcVersion = "2015-04-16"
            , _svcHandle  = handle
            , _svcRetry   = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError JSONError)
        handle = jsonError statusSuccess service'

        retry :: Retry DirectoryService
        retry = undefined

        check :: Status
              -> JSONError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e) = undefined

-- | /See:/ 'attribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'attValue'
--
-- * 'attName'
data Attribute = Attribute'{_attValue :: Maybe Text, _attName :: Text} deriving (Eq, Read, Show)

-- | 'Attribute' smart constructor.
attribute :: Text -> Attribute
attribute pName = Attribute'{_attValue = Nothing, _attName = pName};

-- | The value of the attribute.
attValue :: Lens' Attribute (Maybe Text)
attValue = lens _attValue (\ s a -> s{_attValue = a});

-- | The name of the attribute.
attName :: Lens' Attribute Text
attName = lens _attName (\ s a -> s{_attName = a});

instance FromJSON Attribute where
        parseJSON
          = withObject "Attribute"
              (\ x -> Attribute' <$> x .:? "Value" <*> x .: "Name")

instance ToJSON Attribute where
        toJSON Attribute'{..}
          = object ["Value" .= _attValue, "Name" .= _attName]

-- | /See:/ 'computer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'comComputerAttributes'
--
-- * 'comComputerId'
--
-- * 'comComputerName'
data Computer = Computer'{_comComputerAttributes :: [Attribute], _comComputerId :: Text, _comComputerName :: Text} deriving (Eq, Read, Show)

-- | 'Computer' smart constructor.
computer :: Text -> Text -> Computer
computer pComputerId pComputerName = Computer'{_comComputerAttributes = mempty, _comComputerId = pComputerId, _comComputerName = pComputerName};

-- | An array of Attribute objects that contain the LDAP attributes that
-- belong to the computer account.
comComputerAttributes :: Lens' Computer [Attribute]
comComputerAttributes = lens _comComputerAttributes (\ s a -> s{_comComputerAttributes = a});

-- | The identifier of the computer.
comComputerId :: Lens' Computer Text
comComputerId = lens _comComputerId (\ s a -> s{_comComputerId = a});

-- | The computer name.
comComputerName :: Lens' Computer Text
comComputerName = lens _comComputerName (\ s a -> s{_comComputerName = a});

instance FromJSON Computer where
        parseJSON
          = withObject "Computer"
              (\ x ->
                 Computer' <$>
                   x .:? "ComputerAttributes" .!= mempty <*>
                     x .: "ComputerId"
                     <*> x .: "ComputerName")

-- | /See:/ 'directoryConnectSettings' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsVPCId'
--
-- * 'dcsSubnetIds'
--
-- * 'dcsCustomerDNSIPs'
--
-- * 'dcsCustomerUserName'
data DirectoryConnectSettings = DirectoryConnectSettings'{_dcsVPCId :: Text, _dcsSubnetIds :: [Text], _dcsCustomerDNSIPs :: [Text], _dcsCustomerUserName :: Text} deriving (Eq, Read, Show)

-- | 'DirectoryConnectSettings' smart constructor.
directoryConnectSettings :: Text -> [Text] -> [Text] -> Text -> DirectoryConnectSettings
directoryConnectSettings pVPCId pSubnetIds pCustomerDNSIPs pCustomerUserName = DirectoryConnectSettings'{_dcsVPCId = pVPCId, _dcsSubnetIds = pSubnetIds, _dcsCustomerDNSIPs = pCustomerDNSIPs, _dcsCustomerUserName = pCustomerUserName};

-- | The identifier of the VPC that the AD Connector is created in.
dcsVPCId :: Lens' DirectoryConnectSettings Text
dcsVPCId = lens _dcsVPCId (\ s a -> s{_dcsVPCId = a});

-- | A list of subnet identifiers in the VPC that the AD Connector is created
-- in.
dcsSubnetIds :: Lens' DirectoryConnectSettings [Text]
dcsSubnetIds = lens _dcsSubnetIds (\ s a -> s{_dcsSubnetIds = a});

-- | A list of one or more IP addresses of DNS servers or domain controllers
-- in the on-premises directory.
dcsCustomerDNSIPs :: Lens' DirectoryConnectSettings [Text]
dcsCustomerDNSIPs = lens _dcsCustomerDNSIPs (\ s a -> s{_dcsCustomerDNSIPs = a});

-- | The username of an account in the on-premises directory that is used to
-- connect to the directory. This account must have the following
-- privileges:
--
-- -   Read users and groups
-- -   Create computer objects
-- -   Join computers to the domain
dcsCustomerUserName :: Lens' DirectoryConnectSettings Text
dcsCustomerUserName = lens _dcsCustomerUserName (\ s a -> s{_dcsCustomerUserName = a});

instance ToJSON DirectoryConnectSettings where
        toJSON DirectoryConnectSettings'{..}
          = object
              ["VpcId" .= _dcsVPCId, "SubnetIds" .= _dcsSubnetIds,
               "CustomerDnsIps" .= _dcsCustomerDNSIPs,
               "CustomerUserName" .= _dcsCustomerUserName]

-- | /See:/ 'directoryConnectSettingsDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsdSubnetIds'
--
-- * 'dcsdVPCId'
--
-- * 'dcsdConnectIPs'
--
-- * 'dcsdSecurityGroupId'
--
-- * 'dcsdAvailabilityZones'
--
-- * 'dcsdCustomerUserName'
data DirectoryConnectSettingsDescription = DirectoryConnectSettingsDescription'{_dcsdSubnetIds :: [Text], _dcsdVPCId :: Maybe Text, _dcsdConnectIPs :: [Text], _dcsdSecurityGroupId :: Maybe Text, _dcsdAvailabilityZones :: [Text], _dcsdCustomerUserName :: Text} deriving (Eq, Read, Show)

-- | 'DirectoryConnectSettingsDescription' smart constructor.
directoryConnectSettingsDescription :: Text -> DirectoryConnectSettingsDescription
directoryConnectSettingsDescription pCustomerUserName = DirectoryConnectSettingsDescription'{_dcsdSubnetIds = mempty, _dcsdVPCId = Nothing, _dcsdConnectIPs = mempty, _dcsdSecurityGroupId = Nothing, _dcsdAvailabilityZones = mempty, _dcsdCustomerUserName = pCustomerUserName};

-- | A list of subnet identifiers in the VPC that the AD connector is in.
dcsdSubnetIds :: Lens' DirectoryConnectSettingsDescription [Text]
dcsdSubnetIds = lens _dcsdSubnetIds (\ s a -> s{_dcsdSubnetIds = a});

-- | The identifier of the VPC that the AD Connector is in.
dcsdVPCId :: Lens' DirectoryConnectSettingsDescription (Maybe Text)
dcsdVPCId = lens _dcsdVPCId (\ s a -> s{_dcsdVPCId = a});

-- | The IP addresses of the AD Connector servers.
dcsdConnectIPs :: Lens' DirectoryConnectSettingsDescription [Text]
dcsdConnectIPs = lens _dcsdConnectIPs (\ s a -> s{_dcsdConnectIPs = a});

-- | The security group identifier for the AD Connector directory.
dcsdSecurityGroupId :: Lens' DirectoryConnectSettingsDescription (Maybe Text)
dcsdSecurityGroupId = lens _dcsdSecurityGroupId (\ s a -> s{_dcsdSecurityGroupId = a});

-- | A list of the Availability Zones that the directory is in.
dcsdAvailabilityZones :: Lens' DirectoryConnectSettingsDescription [Text]
dcsdAvailabilityZones = lens _dcsdAvailabilityZones (\ s a -> s{_dcsdAvailabilityZones = a});

-- | The username of the service account in the on-premises directory.
dcsdCustomerUserName :: Lens' DirectoryConnectSettingsDescription Text
dcsdCustomerUserName = lens _dcsdCustomerUserName (\ s a -> s{_dcsdCustomerUserName = a});

instance FromJSON DirectoryConnectSettingsDescription
         where
        parseJSON
          = withObject "DirectoryConnectSettingsDescription"
              (\ x ->
                 DirectoryConnectSettingsDescription' <$>
                   x .:? "SubnetIds" .!= mempty <*> x .:? "VpcId" <*>
                     x .:? "ConnectIps" .!= mempty
                     <*> x .:? "SecurityGroupId"
                     <*> x .:? "AvailabilityZones" .!= mempty
                     <*> x .: "CustomerUserName")

-- | /See:/ 'directoryDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddRadiusStatus'
--
-- * 'ddDirectoryId'
--
-- * 'ddStage'
--
-- * 'ddShortName'
--
-- * 'ddSize'
--
-- * 'ddRadiusSettings'
--
-- * 'ddLaunchTime'
--
-- * 'ddName'
--
-- * 'ddSsoEnabled'
--
-- * 'ddStageLastUpdatedDateTime'
--
-- * 'ddStageReason'
--
-- * 'ddDNSIPAddrs'
--
-- * 'ddVPCSettings'
--
-- * 'ddType'
--
-- * 'ddConnectSettings'
--
-- * 'ddDescription'
--
-- * 'ddAccessURL'
--
-- * 'ddAlias'
data DirectoryDescription = DirectoryDescription'{_ddRadiusStatus :: Maybe RadiusStatus, _ddDirectoryId :: Maybe Text, _ddStage :: Maybe DirectoryStage, _ddShortName :: Maybe Text, _ddSize :: Maybe DirectorySize, _ddRadiusSettings :: Maybe RadiusSettings, _ddLaunchTime :: Maybe POSIX, _ddName :: Maybe Text, _ddSsoEnabled :: Maybe Bool, _ddStageLastUpdatedDateTime :: Maybe POSIX, _ddStageReason :: Maybe Text, _ddDNSIPAddrs :: [Text], _ddVPCSettings :: Maybe DirectoryVPCSettingsDescription, _ddType :: Maybe DirectoryType, _ddConnectSettings :: Maybe DirectoryConnectSettingsDescription, _ddDescription :: Maybe Text, _ddAccessURL :: Text, _ddAlias :: Text} deriving (Eq, Read, Show)

-- | 'DirectoryDescription' smart constructor.
directoryDescription :: Text -> Text -> DirectoryDescription
directoryDescription pAccessURL pAlias = DirectoryDescription'{_ddRadiusStatus = Nothing, _ddDirectoryId = Nothing, _ddStage = Nothing, _ddShortName = Nothing, _ddSize = Nothing, _ddRadiusSettings = Nothing, _ddLaunchTime = Nothing, _ddName = Nothing, _ddSsoEnabled = Nothing, _ddStageLastUpdatedDateTime = Nothing, _ddStageReason = Nothing, _ddDNSIPAddrs = mempty, _ddVPCSettings = Nothing, _ddType = Nothing, _ddConnectSettings = Nothing, _ddDescription = Nothing, _ddAccessURL = pAccessURL, _ddAlias = pAlias};

-- | The status of the RADIUS MFA server connection.
ddRadiusStatus :: Lens' DirectoryDescription (Maybe RadiusStatus)
ddRadiusStatus = lens _ddRadiusStatus (\ s a -> s{_ddRadiusStatus = a});

-- | The directory identifier.
ddDirectoryId :: Lens' DirectoryDescription (Maybe Text)
ddDirectoryId = lens _ddDirectoryId (\ s a -> s{_ddDirectoryId = a});

-- | The current stage of the directory.
ddStage :: Lens' DirectoryDescription (Maybe DirectoryStage)
ddStage = lens _ddStage (\ s a -> s{_ddStage = a});

-- | The short name of the directory.
ddShortName :: Lens' DirectoryDescription (Maybe Text)
ddShortName = lens _ddShortName (\ s a -> s{_ddShortName = a});

-- | The directory size.
ddSize :: Lens' DirectoryDescription (Maybe DirectorySize)
ddSize = lens _ddSize (\ s a -> s{_ddSize = a});

-- | A RadiusSettings object that contains information about the RADIUS
-- server configured for this directory.
ddRadiusSettings :: Lens' DirectoryDescription (Maybe RadiusSettings)
ddRadiusSettings = lens _ddRadiusSettings (\ s a -> s{_ddRadiusSettings = a});

-- | Specifies when the directory was created.
ddLaunchTime :: Lens' DirectoryDescription (Maybe UTCTime)
ddLaunchTime = lens _ddLaunchTime (\ s a -> s{_ddLaunchTime = a}) . mapping _Time;

-- | The fully-qualified name of the directory.
ddName :: Lens' DirectoryDescription (Maybe Text)
ddName = lens _ddName (\ s a -> s{_ddName = a});

-- | Indicates if single-sign on is enabled for the directory. For more
-- information, see EnableSso and DisableSso.
ddSsoEnabled :: Lens' DirectoryDescription (Maybe Bool)
ddSsoEnabled = lens _ddSsoEnabled (\ s a -> s{_ddSsoEnabled = a});

-- | The date and time that the stage was last updated.
ddStageLastUpdatedDateTime :: Lens' DirectoryDescription (Maybe UTCTime)
ddStageLastUpdatedDateTime = lens _ddStageLastUpdatedDateTime (\ s a -> s{_ddStageLastUpdatedDateTime = a}) . mapping _Time;

-- | Additional information about the directory stage.
ddStageReason :: Lens' DirectoryDescription (Maybe Text)
ddStageReason = lens _ddStageReason (\ s a -> s{_ddStageReason = a});

-- | The IP addresses of the DNS servers for the directory. For a Simple AD
-- directory, these are the IP addresses of the Simple AD directory
-- servers. For an AD Connector directory, these are the IP addresses of
-- the DNS servers or domain controllers in the on-premises directory that
-- the AD Connector is connected to.
ddDNSIPAddrs :: Lens' DirectoryDescription [Text]
ddDNSIPAddrs = lens _ddDNSIPAddrs (\ s a -> s{_ddDNSIPAddrs = a});

-- | A DirectoryVpcSettingsDescription object that contains additional
-- information about a Simple AD directory. This member is only present if
-- the directory is a Simple AD directory.
ddVPCSettings :: Lens' DirectoryDescription (Maybe DirectoryVPCSettingsDescription)
ddVPCSettings = lens _ddVPCSettings (\ s a -> s{_ddVPCSettings = a});

-- | The directory size.
ddType :: Lens' DirectoryDescription (Maybe DirectoryType)
ddType = lens _ddType (\ s a -> s{_ddType = a});

-- | A DirectoryConnectSettingsDescription object that contains additional
-- information about an AD Connector directory. This member is only present
-- if the directory is an AD Connector directory.
ddConnectSettings :: Lens' DirectoryDescription (Maybe DirectoryConnectSettingsDescription)
ddConnectSettings = lens _ddConnectSettings (\ s a -> s{_ddConnectSettings = a});

-- | The textual description for the directory.
ddDescription :: Lens' DirectoryDescription (Maybe Text)
ddDescription = lens _ddDescription (\ s a -> s{_ddDescription = a});

-- | The access URL for the directory, such as
-- @http:\/\/\<alias>.awsapps.com@.
ddAccessURL :: Lens' DirectoryDescription Text
ddAccessURL = lens _ddAccessURL (\ s a -> s{_ddAccessURL = a});

-- | The alias for the directory.
ddAlias :: Lens' DirectoryDescription Text
ddAlias = lens _ddAlias (\ s a -> s{_ddAlias = a});

instance FromJSON DirectoryDescription where
        parseJSON
          = withObject "DirectoryDescription"
              (\ x ->
                 DirectoryDescription' <$>
                   x .:? "RadiusStatus" <*> x .:? "DirectoryId" <*>
                     x .:? "Stage"
                     <*> x .:? "ShortName"
                     <*> x .:? "Size"
                     <*> x .:? "RadiusSettings"
                     <*> x .:? "LaunchTime"
                     <*> x .:? "Name"
                     <*> x .:? "SsoEnabled"
                     <*> x .:? "StageLastUpdatedDateTime"
                     <*> x .:? "StageReason"
                     <*> x .:? "DnsIpAddrs" .!= mempty
                     <*> x .:? "VpcSettings"
                     <*> x .:? "Type"
                     <*> x .:? "ConnectSettings"
                     <*> x .:? "Description"
                     <*> x .: "AccessUrl"
                     <*> x .: "Alias")

-- | /See:/ 'directoryLimits' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlConnectedDirectoriesCurrentCount'
--
-- * 'dlConnectedDirectoriesLimit'
--
-- * 'dlConnectedDirectoriesLimitReached'
--
-- * 'dlCloudOnlyDirectoriesLimit'
--
-- * 'dlCloudOnlyDirectoriesCurrentCount'
--
-- * 'dlCloudOnlyDirectoriesLimitReached'
data DirectoryLimits = DirectoryLimits'{_dlConnectedDirectoriesCurrentCount :: Maybe Nat, _dlConnectedDirectoriesLimit :: Maybe Nat, _dlConnectedDirectoriesLimitReached :: Maybe Bool, _dlCloudOnlyDirectoriesLimit :: Maybe Nat, _dlCloudOnlyDirectoriesCurrentCount :: Maybe Nat, _dlCloudOnlyDirectoriesLimitReached :: Maybe Bool} deriving (Eq, Read, Show)

-- | 'DirectoryLimits' smart constructor.
directoryLimits :: DirectoryLimits
directoryLimits = DirectoryLimits'{_dlConnectedDirectoriesCurrentCount = Nothing, _dlConnectedDirectoriesLimit = Nothing, _dlConnectedDirectoriesLimitReached = Nothing, _dlCloudOnlyDirectoriesLimit = Nothing, _dlCloudOnlyDirectoriesCurrentCount = Nothing, _dlCloudOnlyDirectoriesLimitReached = Nothing};

-- | The current number of connected directories in the region.
dlConnectedDirectoriesCurrentCount :: Lens' DirectoryLimits (Maybe Natural)
dlConnectedDirectoriesCurrentCount = lens _dlConnectedDirectoriesCurrentCount (\ s a -> s{_dlConnectedDirectoriesCurrentCount = a}) . mapping _Nat;

-- | The maximum number of connected directories allowed in the region.
dlConnectedDirectoriesLimit :: Lens' DirectoryLimits (Maybe Natural)
dlConnectedDirectoriesLimit = lens _dlConnectedDirectoriesLimit (\ s a -> s{_dlConnectedDirectoriesLimit = a}) . mapping _Nat;

-- | Indicates if the connected directory limit has been reached.
dlConnectedDirectoriesLimitReached :: Lens' DirectoryLimits (Maybe Bool)
dlConnectedDirectoriesLimitReached = lens _dlConnectedDirectoriesLimitReached (\ s a -> s{_dlConnectedDirectoriesLimitReached = a});

-- | The maximum number of cloud directories allowed in the region.
dlCloudOnlyDirectoriesLimit :: Lens' DirectoryLimits (Maybe Natural)
dlCloudOnlyDirectoriesLimit = lens _dlCloudOnlyDirectoriesLimit (\ s a -> s{_dlCloudOnlyDirectoriesLimit = a}) . mapping _Nat;

-- | The current number of cloud directories in the region.
dlCloudOnlyDirectoriesCurrentCount :: Lens' DirectoryLimits (Maybe Natural)
dlCloudOnlyDirectoriesCurrentCount = lens _dlCloudOnlyDirectoriesCurrentCount (\ s a -> s{_dlCloudOnlyDirectoriesCurrentCount = a}) . mapping _Nat;

-- | Indicates if the cloud directory limit has been reached.
dlCloudOnlyDirectoriesLimitReached :: Lens' DirectoryLimits (Maybe Bool)
dlCloudOnlyDirectoriesLimitReached = lens _dlCloudOnlyDirectoriesLimitReached (\ s a -> s{_dlCloudOnlyDirectoriesLimitReached = a});

instance FromJSON DirectoryLimits where
        parseJSON
          = withObject "DirectoryLimits"
              (\ x ->
                 DirectoryLimits' <$>
                   x .:? "ConnectedDirectoriesCurrentCount" <*>
                     x .:? "ConnectedDirectoriesLimit"
                     <*> x .:? "ConnectedDirectoriesLimitReached"
                     <*> x .:? "CloudOnlyDirectoriesLimit"
                     <*> x .:? "CloudOnlyDirectoriesCurrentCount"
                     <*> x .:? "CloudOnlyDirectoriesLimitReached")

data DirectorySize = Small | Large deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText DirectorySize where
    parser = takeLowerText >>= \case
        "Large" -> pure Large
        "Small" -> pure Small
        e -> fail ("Failure parsing DirectorySize from " ++ show e)

instance ToText DirectorySize where
    toText = \case
        Large -> "Large"
        Small -> "Small"

instance Hashable DirectorySize
instance ToQuery DirectorySize
instance ToHeader DirectorySize

instance ToJSON DirectorySize where
    toJSON = toJSONText

instance FromJSON DirectorySize where
    parseJSON = parseJSONText "DirectorySize"

data DirectoryStage = DSRestoreFailed | DSDeleted | DSRestoring | DSImpaired | DSDeleting | DSFailed | DSRequested | DSCreated | DSInoperable | DSActive | DSCreating deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText DirectoryStage where
    parser = takeLowerText >>= \case
        "Active" -> pure DSActive
        "Created" -> pure DSCreated
        "Creating" -> pure DSCreating
        "Deleted" -> pure DSDeleted
        "Deleting" -> pure DSDeleting
        "Failed" -> pure DSFailed
        "Impaired" -> pure DSImpaired
        "Inoperable" -> pure DSInoperable
        "Requested" -> pure DSRequested
        "RestoreFailed" -> pure DSRestoreFailed
        "Restoring" -> pure DSRestoring
        e -> fail ("Failure parsing DirectoryStage from " ++ show e)

instance ToText DirectoryStage where
    toText = \case
        DSActive -> "Active"
        DSCreated -> "Created"
        DSCreating -> "Creating"
        DSDeleted -> "Deleted"
        DSDeleting -> "Deleting"
        DSFailed -> "Failed"
        DSImpaired -> "Impaired"
        DSInoperable -> "Inoperable"
        DSRequested -> "Requested"
        DSRestoreFailed -> "RestoreFailed"
        DSRestoring -> "Restoring"

instance Hashable DirectoryStage
instance ToQuery DirectoryStage
instance ToHeader DirectoryStage

instance FromJSON DirectoryStage where
    parseJSON = parseJSONText "DirectoryStage"

data DirectoryType = ADConnector | SimpleAD deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText DirectoryType where
    parser = takeLowerText >>= \case
        "ADConnector" -> pure ADConnector
        "SimpleAD" -> pure SimpleAD
        e -> fail ("Failure parsing DirectoryType from " ++ show e)

instance ToText DirectoryType where
    toText = \case
        ADConnector -> "ADConnector"
        SimpleAD -> "SimpleAD"

instance Hashable DirectoryType
instance ToQuery DirectoryType
instance ToHeader DirectoryType

instance FromJSON DirectoryType where
    parseJSON = parseJSONText "DirectoryType"

-- | /See:/ 'directoryVPCSettings' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvsVPCId'
--
-- * 'dvsSubnetIds'
data DirectoryVPCSettings = DirectoryVPCSettings'{_dvsVPCId :: Text, _dvsSubnetIds :: [Text]} deriving (Eq, Read, Show)

-- | 'DirectoryVPCSettings' smart constructor.
directoryVPCSettings :: Text -> [Text] -> DirectoryVPCSettings
directoryVPCSettings pVPCId pSubnetIds = DirectoryVPCSettings'{_dvsVPCId = pVPCId, _dvsSubnetIds = pSubnetIds};

-- | The identifier of the VPC to create the Simple AD directory in.
dvsVPCId :: Lens' DirectoryVPCSettings Text
dvsVPCId = lens _dvsVPCId (\ s a -> s{_dvsVPCId = a});

-- | The identifiers of the subnets for the directory servers. The two
-- subnets must be in different Availability Zones. AWS Directory Service
-- creates a directory server and a DNS server in each of these subnets.
dvsSubnetIds :: Lens' DirectoryVPCSettings [Text]
dvsSubnetIds = lens _dvsSubnetIds (\ s a -> s{_dvsSubnetIds = a});

instance ToJSON DirectoryVPCSettings where
        toJSON DirectoryVPCSettings'{..}
          = object
              ["VpcId" .= _dvsVPCId, "SubnetIds" .= _dvsSubnetIds]

-- | /See:/ 'directoryVPCSettingsDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvsdSubnetIds'
--
-- * 'dvsdVPCId'
--
-- * 'dvsdSecurityGroupId'
--
-- * 'dvsdAvailabilityZones'
data DirectoryVPCSettingsDescription = DirectoryVPCSettingsDescription'{_dvsdSubnetIds :: [Text], _dvsdVPCId :: Maybe Text, _dvsdSecurityGroupId :: Maybe Text, _dvsdAvailabilityZones :: [Text]} deriving (Eq, Read, Show)

-- | 'DirectoryVPCSettingsDescription' smart constructor.
directoryVPCSettingsDescription :: DirectoryVPCSettingsDescription
directoryVPCSettingsDescription = DirectoryVPCSettingsDescription'{_dvsdSubnetIds = mempty, _dvsdVPCId = Nothing, _dvsdSecurityGroupId = Nothing, _dvsdAvailabilityZones = mempty};

-- | The identifiers of the subnets for the directory servers.
dvsdSubnetIds :: Lens' DirectoryVPCSettingsDescription [Text]
dvsdSubnetIds = lens _dvsdSubnetIds (\ s a -> s{_dvsdSubnetIds = a});

-- | The identifier of the VPC that the directory is in.
dvsdVPCId :: Lens' DirectoryVPCSettingsDescription (Maybe Text)
dvsdVPCId = lens _dvsdVPCId (\ s a -> s{_dvsdVPCId = a});

-- | The security group identifier for the directory.
dvsdSecurityGroupId :: Lens' DirectoryVPCSettingsDescription (Maybe Text)
dvsdSecurityGroupId = lens _dvsdSecurityGroupId (\ s a -> s{_dvsdSecurityGroupId = a});

-- | The list of Availability Zones that the directory is in.
dvsdAvailabilityZones :: Lens' DirectoryVPCSettingsDescription [Text]
dvsdAvailabilityZones = lens _dvsdAvailabilityZones (\ s a -> s{_dvsdAvailabilityZones = a});

instance FromJSON DirectoryVPCSettingsDescription
         where
        parseJSON
          = withObject "DirectoryVPCSettingsDescription"
              (\ x ->
                 DirectoryVPCSettingsDescription' <$>
                   x .:? "SubnetIds" .!= mempty <*> x .:? "VpcId" <*>
                     x .:? "SecurityGroupId"
                     <*> x .:? "AvailabilityZones" .!= mempty)

data RadiusAuthenticationProtocol = CHAP | MSCHAPV1 | MSCHAPV2 | PAP deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText RadiusAuthenticationProtocol where
    parser = takeLowerText >>= \case
        "CHAP" -> pure CHAP
        "MS-CHAPv1" -> pure MSCHAPV1
        "MS-CHAPv2" -> pure MSCHAPV2
        "PAP" -> pure PAP
        e -> fail ("Failure parsing RadiusAuthenticationProtocol from " ++ show e)

instance ToText RadiusAuthenticationProtocol where
    toText = \case
        CHAP -> "CHAP"
        MSCHAPV1 -> "MS-CHAPv1"
        MSCHAPV2 -> "MS-CHAPv2"
        PAP -> "PAP"

instance Hashable RadiusAuthenticationProtocol
instance ToQuery RadiusAuthenticationProtocol
instance ToHeader RadiusAuthenticationProtocol

instance ToJSON RadiusAuthenticationProtocol where
    toJSON = toJSONText

instance FromJSON RadiusAuthenticationProtocol where
    parseJSON = parseJSONText "RadiusAuthenticationProtocol"

-- | /See:/ 'radiusSettings' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsRadiusServers'
--
-- * 'rsRadiusRetries'
--
-- * 'rsAuthenticationProtocol'
--
-- * 'rsUseSameUsername'
--
-- * 'rsDisplayLabel'
--
-- * 'rsSharedSecret'
--
-- * 'rsRadiusTimeout'
--
-- * 'rsRadiusPort'
data RadiusSettings = RadiusSettings'{_rsRadiusServers :: [Text], _rsRadiusRetries :: Maybe Nat, _rsAuthenticationProtocol :: Maybe RadiusAuthenticationProtocol, _rsUseSameUsername :: Maybe Bool, _rsDisplayLabel :: Text, _rsSharedSecret :: Sensitive Text, _rsRadiusTimeout :: Nat, _rsRadiusPort :: Nat} deriving (Eq, Read, Show)

-- | 'RadiusSettings' smart constructor.
radiusSettings :: Text -> Text -> Natural -> Natural -> RadiusSettings
radiusSettings pDisplayLabel pSharedSecret pRadiusTimeout pRadiusPort = RadiusSettings'{_rsRadiusServers = mempty, _rsRadiusRetries = Nothing, _rsAuthenticationProtocol = Nothing, _rsUseSameUsername = Nothing, _rsDisplayLabel = pDisplayLabel, _rsSharedSecret = _Sensitive # pSharedSecret, _rsRadiusTimeout = _Nat # pRadiusTimeout, _rsRadiusPort = _Nat # pRadiusPort};

-- | An array of strings that contains the IP addresses of the RADIUS server
-- endpoints, or the IP addresses of your RADIUS server load balancer.
rsRadiusServers :: Lens' RadiusSettings [Text]
rsRadiusServers = lens _rsRadiusServers (\ s a -> s{_rsRadiusServers = a});

-- | The maximum number of times that communication with the RADIUS server is
-- attempted.
rsRadiusRetries :: Lens' RadiusSettings (Maybe Natural)
rsRadiusRetries = lens _rsRadiusRetries (\ s a -> s{_rsRadiusRetries = a}) . mapping _Nat;

-- | The protocol specified for your RADIUS endpoints.
rsAuthenticationProtocol :: Lens' RadiusSettings (Maybe RadiusAuthenticationProtocol)
rsAuthenticationProtocol = lens _rsAuthenticationProtocol (\ s a -> s{_rsAuthenticationProtocol = a});

-- | Not currently used.
rsUseSameUsername :: Lens' RadiusSettings (Maybe Bool)
rsUseSameUsername = lens _rsUseSameUsername (\ s a -> s{_rsUseSameUsername = a});

-- | Not currently used.
rsDisplayLabel :: Lens' RadiusSettings Text
rsDisplayLabel = lens _rsDisplayLabel (\ s a -> s{_rsDisplayLabel = a});

-- | The shared secret code that was specified when your RADIUS endpoints
-- were created.
rsSharedSecret :: Lens' RadiusSettings Text
rsSharedSecret = lens _rsSharedSecret (\ s a -> s{_rsSharedSecret = a}) . _Sensitive;

-- | The amount of time, in seconds, to wait for the RADIUS server to
-- respond.
rsRadiusTimeout :: Lens' RadiusSettings Natural
rsRadiusTimeout = lens _rsRadiusTimeout (\ s a -> s{_rsRadiusTimeout = a}) . _Nat;

-- | The port that your RADIUS server is using for communications. Your
-- on-premises network must allow inbound traffic over this port from the
-- AWS Directory Service servers.
rsRadiusPort :: Lens' RadiusSettings Natural
rsRadiusPort = lens _rsRadiusPort (\ s a -> s{_rsRadiusPort = a}) . _Nat;

instance FromJSON RadiusSettings where
        parseJSON
          = withObject "RadiusSettings"
              (\ x ->
                 RadiusSettings' <$>
                   x .:? "RadiusServers" .!= mempty <*>
                     x .:? "RadiusRetries"
                     <*> x .:? "AuthenticationProtocol"
                     <*> x .:? "UseSameUsername"
                     <*> x .: "DisplayLabel"
                     <*> x .: "SharedSecret"
                     <*> x .: "RadiusTimeout"
                     <*> x .: "RadiusPort")

instance ToJSON RadiusSettings where
        toJSON RadiusSettings'{..}
          = object
              ["RadiusServers" .= _rsRadiusServers,
               "RadiusRetries" .= _rsRadiusRetries,
               "AuthenticationProtocol" .=
                 _rsAuthenticationProtocol,
               "UseSameUsername" .= _rsUseSameUsername,
               "DisplayLabel" .= _rsDisplayLabel,
               "SharedSecret" .= _rsSharedSecret,
               "RadiusTimeout" .= _rsRadiusTimeout,
               "RadiusPort" .= _rsRadiusPort]

data RadiusStatus = Creating | Completed | Failed deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText RadiusStatus where
    parser = takeLowerText >>= \case
        "Completed" -> pure Completed
        "Creating" -> pure Creating
        "Failed" -> pure Failed
        e -> fail ("Failure parsing RadiusStatus from " ++ show e)

instance ToText RadiusStatus where
    toText = \case
        Completed -> "Completed"
        Creating -> "Creating"
        Failed -> "Failed"

instance Hashable RadiusStatus
instance ToQuery RadiusStatus
instance ToHeader RadiusStatus

instance FromJSON RadiusStatus where
    parseJSON = parseJSONText "RadiusStatus"

-- | /See:/ 'snapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'snaDirectoryId'
--
-- * 'snaStatus'
--
-- * 'snaStartTime'
--
-- * 'snaName'
--
-- * 'snaType'
--
-- * 'snaSnapshotId'
data Snapshot = Snapshot'{_snaDirectoryId :: Maybe Text, _snaStatus :: Maybe SnapshotStatus, _snaStartTime :: Maybe POSIX, _snaName :: Maybe Text, _snaType :: Maybe SnapshotType, _snaSnapshotId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Snapshot' smart constructor.
snapshot :: Snapshot
snapshot = Snapshot'{_snaDirectoryId = Nothing, _snaStatus = Nothing, _snaStartTime = Nothing, _snaName = Nothing, _snaType = Nothing, _snaSnapshotId = Nothing};

-- | The directory identifier.
snaDirectoryId :: Lens' Snapshot (Maybe Text)
snaDirectoryId = lens _snaDirectoryId (\ s a -> s{_snaDirectoryId = a});

-- | The snapshot status.
snaStatus :: Lens' Snapshot (Maybe SnapshotStatus)
snaStatus = lens _snaStatus (\ s a -> s{_snaStatus = a});

-- | The date and time that the snapshot was taken.
snaStartTime :: Lens' Snapshot (Maybe UTCTime)
snaStartTime = lens _snaStartTime (\ s a -> s{_snaStartTime = a}) . mapping _Time;

-- | The descriptive name of the snapshot.
snaName :: Lens' Snapshot (Maybe Text)
snaName = lens _snaName (\ s a -> s{_snaName = a});

-- | The snapshot type.
snaType :: Lens' Snapshot (Maybe SnapshotType)
snaType = lens _snaType (\ s a -> s{_snaType = a});

-- | The snapshot identifier.
snaSnapshotId :: Lens' Snapshot (Maybe Text)
snaSnapshotId = lens _snaSnapshotId (\ s a -> s{_snaSnapshotId = a});

instance FromJSON Snapshot where
        parseJSON
          = withObject "Snapshot"
              (\ x ->
                 Snapshot' <$>
                   x .:? "DirectoryId" <*> x .:? "Status" <*>
                     x .:? "StartTime"
                     <*> x .:? "Name"
                     <*> x .:? "Type"
                     <*> x .:? "SnapshotId")

-- | /See:/ 'snapshotLimits' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slManualSnapshotsLimitReached'
--
-- * 'slManualSnapshotsCurrentCount'
--
-- * 'slManualSnapshotsLimit'
data SnapshotLimits = SnapshotLimits'{_slManualSnapshotsLimitReached :: Maybe Bool, _slManualSnapshotsCurrentCount :: Maybe Nat, _slManualSnapshotsLimit :: Maybe Nat} deriving (Eq, Read, Show)

-- | 'SnapshotLimits' smart constructor.
snapshotLimits :: SnapshotLimits
snapshotLimits = SnapshotLimits'{_slManualSnapshotsLimitReached = Nothing, _slManualSnapshotsCurrentCount = Nothing, _slManualSnapshotsLimit = Nothing};

-- | Indicates if the manual snapshot limit has been reached.
slManualSnapshotsLimitReached :: Lens' SnapshotLimits (Maybe Bool)
slManualSnapshotsLimitReached = lens _slManualSnapshotsLimitReached (\ s a -> s{_slManualSnapshotsLimitReached = a});

-- | The current number of manual snapshots of the directory.
slManualSnapshotsCurrentCount :: Lens' SnapshotLimits (Maybe Natural)
slManualSnapshotsCurrentCount = lens _slManualSnapshotsCurrentCount (\ s a -> s{_slManualSnapshotsCurrentCount = a}) . mapping _Nat;

-- | The maximum number of manual snapshots allowed.
slManualSnapshotsLimit :: Lens' SnapshotLimits (Maybe Natural)
slManualSnapshotsLimit = lens _slManualSnapshotsLimit (\ s a -> s{_slManualSnapshotsLimit = a}) . mapping _Nat;

instance FromJSON SnapshotLimits where
        parseJSON
          = withObject "SnapshotLimits"
              (\ x ->
                 SnapshotLimits' <$>
                   x .:? "ManualSnapshotsLimitReached" <*>
                     x .:? "ManualSnapshotsCurrentCount"
                     <*> x .:? "ManualSnapshotsLimit")

data SnapshotStatus = SSCompleted | SSFailed | SSCreating deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText SnapshotStatus where
    parser = takeLowerText >>= \case
        "Completed" -> pure SSCompleted
        "Creating" -> pure SSCreating
        "Failed" -> pure SSFailed
        e -> fail ("Failure parsing SnapshotStatus from " ++ show e)

instance ToText SnapshotStatus where
    toText = \case
        SSCompleted -> "Completed"
        SSCreating -> "Creating"
        SSFailed -> "Failed"

instance Hashable SnapshotStatus
instance ToQuery SnapshotStatus
instance ToHeader SnapshotStatus

instance FromJSON SnapshotStatus where
    parseJSON = parseJSONText "SnapshotStatus"

data SnapshotType = Auto | Manual deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText SnapshotType where
    parser = takeLowerText >>= \case
        "Auto" -> pure Auto
        "Manual" -> pure Manual
        e -> fail ("Failure parsing SnapshotType from " ++ show e)

instance ToText SnapshotType where
    toText = \case
        Auto -> "Auto"
        Manual -> "Manual"

instance Hashable SnapshotType
instance ToQuery SnapshotType
instance ToHeader SnapshotType

instance FromJSON SnapshotType where
    parseJSON = parseJSONText "SnapshotType"
