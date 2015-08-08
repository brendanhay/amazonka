{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.Product where

import           Network.AWS.DirectoryService.Types.Sum
import           Network.AWS.Prelude

-- | Represents a named directory attribute.
--
-- /See:/ 'attribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aValue'
--
-- * 'aName'
data Attribute = Attribute'
    { _aValue :: !(Maybe Text)
    , _aName  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Attribute' smart constructor.
attribute :: Attribute
attribute =
    Attribute'
    { _aValue = Nothing
    , _aName = Nothing
    }

-- | The value of the attribute.
aValue :: Lens' Attribute (Maybe Text)
aValue = lens _aValue (\ s a -> s{_aValue = a});

-- | The name of the attribute.
aName :: Lens' Attribute (Maybe Text)
aName = lens _aName (\ s a -> s{_aName = a});

instance FromJSON Attribute where
        parseJSON
          = withObject "Attribute"
              (\ x ->
                 Attribute' <$> (x .:? "Value") <*> (x .:? "Name"))

instance ToJSON Attribute where
        toJSON Attribute'{..}
          = object ["Value" .= _aValue, "Name" .= _aName]

-- | Contains information about a computer account in a directory.
--
-- /See:/ 'computer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cComputerId'
--
-- * 'cComputerAttributes'
--
-- * 'cComputerName'
data Computer = Computer'
    { _cComputerId         :: !(Maybe Text)
    , _cComputerAttributes :: !(Maybe [Attribute])
    , _cComputerName       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Computer' smart constructor.
computer :: Computer
computer =
    Computer'
    { _cComputerId = Nothing
    , _cComputerAttributes = Nothing
    , _cComputerName = Nothing
    }

-- | The identifier of the computer.
cComputerId :: Lens' Computer (Maybe Text)
cComputerId = lens _cComputerId (\ s a -> s{_cComputerId = a});

-- | An array of Attribute objects that contain the LDAP attributes that
-- belong to the computer account.
cComputerAttributes :: Lens' Computer [Attribute]
cComputerAttributes = lens _cComputerAttributes (\ s a -> s{_cComputerAttributes = a}) . _Default . _Coerce;

-- | The computer name.
cComputerName :: Lens' Computer (Maybe Text)
cComputerName = lens _cComputerName (\ s a -> s{_cComputerName = a});

instance FromJSON Computer where
        parseJSON
          = withObject "Computer"
              (\ x ->
                 Computer' <$>
                   (x .:? "ComputerId") <*>
                     (x .:? "ComputerAttributes" .!= mempty)
                     <*> (x .:? "ComputerName"))

-- | Contains information for the ConnectDirectory operation when an AD
-- Connector directory is being created.
--
-- /See:/ 'directoryConnectSettings' smart constructor.
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
data DirectoryConnectSettings = DirectoryConnectSettings'
    { _dcsVPCId            :: !Text
    , _dcsSubnetIds        :: ![Text]
    , _dcsCustomerDNSIPs   :: ![Text]
    , _dcsCustomerUserName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DirectoryConnectSettings' smart constructor.
directoryConnectSettings :: Text -> Text -> DirectoryConnectSettings
directoryConnectSettings pVPCId_ pCustomerUserName_ =
    DirectoryConnectSettings'
    { _dcsVPCId = pVPCId_
    , _dcsSubnetIds = mempty
    , _dcsCustomerDNSIPs = mempty
    , _dcsCustomerUserName = pCustomerUserName_
    }

-- | The identifier of the VPC that the AD Connector is created in.
dcsVPCId :: Lens' DirectoryConnectSettings Text
dcsVPCId = lens _dcsVPCId (\ s a -> s{_dcsVPCId = a});

-- | A list of subnet identifiers in the VPC that the AD Connector is created
-- in.
dcsSubnetIds :: Lens' DirectoryConnectSettings [Text]
dcsSubnetIds = lens _dcsSubnetIds (\ s a -> s{_dcsSubnetIds = a}) . _Coerce;

-- | A list of one or more IP addresses of DNS servers or domain controllers
-- in the on-premises directory.
dcsCustomerDNSIPs :: Lens' DirectoryConnectSettings [Text]
dcsCustomerDNSIPs = lens _dcsCustomerDNSIPs (\ s a -> s{_dcsCustomerDNSIPs = a}) . _Coerce;

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

-- | Contains information about an AD Connector directory.
--
-- /See:/ 'directoryConnectSettingsDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsdCustomerUserName'
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
data DirectoryConnectSettingsDescription = DirectoryConnectSettingsDescription'
    { _dcsdCustomerUserName  :: !(Maybe Text)
    , _dcsdSubnetIds         :: !(Maybe [Text])
    , _dcsdVPCId             :: !(Maybe Text)
    , _dcsdConnectIPs        :: !(Maybe [Text])
    , _dcsdSecurityGroupId   :: !(Maybe Text)
    , _dcsdAvailabilityZones :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DirectoryConnectSettingsDescription' smart constructor.
directoryConnectSettingsDescription :: DirectoryConnectSettingsDescription
directoryConnectSettingsDescription =
    DirectoryConnectSettingsDescription'
    { _dcsdCustomerUserName = Nothing
    , _dcsdSubnetIds = Nothing
    , _dcsdVPCId = Nothing
    , _dcsdConnectIPs = Nothing
    , _dcsdSecurityGroupId = Nothing
    , _dcsdAvailabilityZones = Nothing
    }

-- | The username of the service account in the on-premises directory.
dcsdCustomerUserName :: Lens' DirectoryConnectSettingsDescription (Maybe Text)
dcsdCustomerUserName = lens _dcsdCustomerUserName (\ s a -> s{_dcsdCustomerUserName = a});

-- | A list of subnet identifiers in the VPC that the AD connector is in.
dcsdSubnetIds :: Lens' DirectoryConnectSettingsDescription [Text]
dcsdSubnetIds = lens _dcsdSubnetIds (\ s a -> s{_dcsdSubnetIds = a}) . _Default . _Coerce;

-- | The identifier of the VPC that the AD Connector is in.
dcsdVPCId :: Lens' DirectoryConnectSettingsDescription (Maybe Text)
dcsdVPCId = lens _dcsdVPCId (\ s a -> s{_dcsdVPCId = a});

-- | The IP addresses of the AD Connector servers.
dcsdConnectIPs :: Lens' DirectoryConnectSettingsDescription [Text]
dcsdConnectIPs = lens _dcsdConnectIPs (\ s a -> s{_dcsdConnectIPs = a}) . _Default . _Coerce;

-- | The security group identifier for the AD Connector directory.
dcsdSecurityGroupId :: Lens' DirectoryConnectSettingsDescription (Maybe Text)
dcsdSecurityGroupId = lens _dcsdSecurityGroupId (\ s a -> s{_dcsdSecurityGroupId = a});

-- | A list of the Availability Zones that the directory is in.
dcsdAvailabilityZones :: Lens' DirectoryConnectSettingsDescription [Text]
dcsdAvailabilityZones = lens _dcsdAvailabilityZones (\ s a -> s{_dcsdAvailabilityZones = a}) . _Default . _Coerce;

instance FromJSON DirectoryConnectSettingsDescription
         where
        parseJSON
          = withObject "DirectoryConnectSettingsDescription"
              (\ x ->
                 DirectoryConnectSettingsDescription' <$>
                   (x .:? "CustomerUserName") <*>
                     (x .:? "SubnetIds" .!= mempty)
                     <*> (x .:? "VpcId")
                     <*> (x .:? "ConnectIps" .!= mempty)
                     <*> (x .:? "SecurityGroupId")
                     <*> (x .:? "AvailabilityZones" .!= mempty))

-- | Contains information about an AWS Directory Service directory.
--
-- /See:/ 'directoryDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddRadiusStatus'
--
-- * 'ddDirectoryId'
--
-- * 'ddStage'
--
-- * 'ddAccessURL'
--
-- * 'ddShortName'
--
-- * 'ddSize'
--
-- * 'ddRadiusSettings'
--
-- * 'ddLaunchTime'
--
-- * 'ddAlias'
--
-- * 'ddName'
--
-- * 'ddSSOEnabled'
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
data DirectoryDescription = DirectoryDescription'
    { _ddRadiusStatus             :: !(Maybe RadiusStatus)
    , _ddDirectoryId              :: !(Maybe Text)
    , _ddStage                    :: !(Maybe DirectoryStage)
    , _ddAccessURL                :: !(Maybe Text)
    , _ddShortName                :: !(Maybe Text)
    , _ddSize                     :: !(Maybe DirectorySize)
    , _ddRadiusSettings           :: !(Maybe RadiusSettings)
    , _ddLaunchTime               :: !(Maybe POSIX)
    , _ddAlias                    :: !(Maybe Text)
    , _ddName                     :: !(Maybe Text)
    , _ddSSOEnabled               :: !(Maybe Bool)
    , _ddStageLastUpdatedDateTime :: !(Maybe POSIX)
    , _ddStageReason              :: !(Maybe Text)
    , _ddDNSIPAddrs               :: !(Maybe [Text])
    , _ddVPCSettings              :: !(Maybe DirectoryVPCSettingsDescription)
    , _ddType                     :: !(Maybe DirectoryType)
    , _ddConnectSettings          :: !(Maybe DirectoryConnectSettingsDescription)
    , _ddDescription              :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DirectoryDescription' smart constructor.
directoryDescription :: DirectoryDescription
directoryDescription =
    DirectoryDescription'
    { _ddRadiusStatus = Nothing
    , _ddDirectoryId = Nothing
    , _ddStage = Nothing
    , _ddAccessURL = Nothing
    , _ddShortName = Nothing
    , _ddSize = Nothing
    , _ddRadiusSettings = Nothing
    , _ddLaunchTime = Nothing
    , _ddAlias = Nothing
    , _ddName = Nothing
    , _ddSSOEnabled = Nothing
    , _ddStageLastUpdatedDateTime = Nothing
    , _ddStageReason = Nothing
    , _ddDNSIPAddrs = Nothing
    , _ddVPCSettings = Nothing
    , _ddType = Nothing
    , _ddConnectSettings = Nothing
    , _ddDescription = Nothing
    }

-- | The status of the RADIUS MFA server connection.
ddRadiusStatus :: Lens' DirectoryDescription (Maybe RadiusStatus)
ddRadiusStatus = lens _ddRadiusStatus (\ s a -> s{_ddRadiusStatus = a});

-- | The directory identifier.
ddDirectoryId :: Lens' DirectoryDescription (Maybe Text)
ddDirectoryId = lens _ddDirectoryId (\ s a -> s{_ddDirectoryId = a});

-- | The current stage of the directory.
ddStage :: Lens' DirectoryDescription (Maybe DirectoryStage)
ddStage = lens _ddStage (\ s a -> s{_ddStage = a});

-- | The access URL for the directory, such as
-- @http:\/\/\<alias>.awsapps.com@.
ddAccessURL :: Lens' DirectoryDescription (Maybe Text)
ddAccessURL = lens _ddAccessURL (\ s a -> s{_ddAccessURL = a});

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

-- | The alias for the directory.
ddAlias :: Lens' DirectoryDescription (Maybe Text)
ddAlias = lens _ddAlias (\ s a -> s{_ddAlias = a});

-- | The fully-qualified name of the directory.
ddName :: Lens' DirectoryDescription (Maybe Text)
ddName = lens _ddName (\ s a -> s{_ddName = a});

-- | Indicates if single-sign on is enabled for the directory. For more
-- information, see EnableSso and DisableSso.
ddSSOEnabled :: Lens' DirectoryDescription (Maybe Bool)
ddSSOEnabled = lens _ddSSOEnabled (\ s a -> s{_ddSSOEnabled = a});

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
ddDNSIPAddrs = lens _ddDNSIPAddrs (\ s a -> s{_ddDNSIPAddrs = a}) . _Default . _Coerce;

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

instance FromJSON DirectoryDescription where
        parseJSON
          = withObject "DirectoryDescription"
              (\ x ->
                 DirectoryDescription' <$>
                   (x .:? "RadiusStatus") <*> (x .:? "DirectoryId") <*>
                     (x .:? "Stage")
                     <*> (x .:? "AccessUrl")
                     <*> (x .:? "ShortName")
                     <*> (x .:? "Size")
                     <*> (x .:? "RadiusSettings")
                     <*> (x .:? "LaunchTime")
                     <*> (x .:? "Alias")
                     <*> (x .:? "Name")
                     <*> (x .:? "SsoEnabled")
                     <*> (x .:? "StageLastUpdatedDateTime")
                     <*> (x .:? "StageReason")
                     <*> (x .:? "DnsIpAddrs" .!= mempty)
                     <*> (x .:? "VpcSettings")
                     <*> (x .:? "Type")
                     <*> (x .:? "ConnectSettings")
                     <*> (x .:? "Description"))

-- | Contains directory limit information for a region.
--
-- /See:/ 'directoryLimits' smart constructor.
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
data DirectoryLimits = DirectoryLimits'
    { _dlConnectedDirectoriesCurrentCount :: !(Maybe Nat)
    , _dlConnectedDirectoriesLimit        :: !(Maybe Nat)
    , _dlConnectedDirectoriesLimitReached :: !(Maybe Bool)
    , _dlCloudOnlyDirectoriesLimit        :: !(Maybe Nat)
    , _dlCloudOnlyDirectoriesCurrentCount :: !(Maybe Nat)
    , _dlCloudOnlyDirectoriesLimitReached :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DirectoryLimits' smart constructor.
directoryLimits :: DirectoryLimits
directoryLimits =
    DirectoryLimits'
    { _dlConnectedDirectoriesCurrentCount = Nothing
    , _dlConnectedDirectoriesLimit = Nothing
    , _dlConnectedDirectoriesLimitReached = Nothing
    , _dlCloudOnlyDirectoriesLimit = Nothing
    , _dlCloudOnlyDirectoriesCurrentCount = Nothing
    , _dlCloudOnlyDirectoriesLimitReached = Nothing
    }

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
                   (x .:? "ConnectedDirectoriesCurrentCount") <*>
                     (x .:? "ConnectedDirectoriesLimit")
                     <*> (x .:? "ConnectedDirectoriesLimitReached")
                     <*> (x .:? "CloudOnlyDirectoriesLimit")
                     <*> (x .:? "CloudOnlyDirectoriesCurrentCount")
                     <*> (x .:? "CloudOnlyDirectoriesLimitReached"))

-- | Contains information for the CreateDirectory operation when a Simple AD
-- directory is being created.
--
-- /See:/ 'directoryVPCSettings' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvsVPCId'
--
-- * 'dvsSubnetIds'
data DirectoryVPCSettings = DirectoryVPCSettings'
    { _dvsVPCId     :: !Text
    , _dvsSubnetIds :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DirectoryVPCSettings' smart constructor.
directoryVPCSettings :: Text -> DirectoryVPCSettings
directoryVPCSettings pVPCId_ =
    DirectoryVPCSettings'
    { _dvsVPCId = pVPCId_
    , _dvsSubnetIds = mempty
    }

-- | The identifier of the VPC to create the Simple AD directory in.
dvsVPCId :: Lens' DirectoryVPCSettings Text
dvsVPCId = lens _dvsVPCId (\ s a -> s{_dvsVPCId = a});

-- | The identifiers of the subnets for the directory servers. The two
-- subnets must be in different Availability Zones. AWS Directory Service
-- creates a directory server and a DNS server in each of these subnets.
dvsSubnetIds :: Lens' DirectoryVPCSettings [Text]
dvsSubnetIds = lens _dvsSubnetIds (\ s a -> s{_dvsSubnetIds = a}) . _Coerce;

instance ToJSON DirectoryVPCSettings where
        toJSON DirectoryVPCSettings'{..}
          = object
              ["VpcId" .= _dvsVPCId, "SubnetIds" .= _dvsSubnetIds]

-- | Contains information about a Simple AD directory.
--
-- /See:/ 'directoryVPCSettingsDescription' smart constructor.
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
data DirectoryVPCSettingsDescription = DirectoryVPCSettingsDescription'
    { _dvsdSubnetIds         :: !(Maybe [Text])
    , _dvsdVPCId             :: !(Maybe Text)
    , _dvsdSecurityGroupId   :: !(Maybe Text)
    , _dvsdAvailabilityZones :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DirectoryVPCSettingsDescription' smart constructor.
directoryVPCSettingsDescription :: DirectoryVPCSettingsDescription
directoryVPCSettingsDescription =
    DirectoryVPCSettingsDescription'
    { _dvsdSubnetIds = Nothing
    , _dvsdVPCId = Nothing
    , _dvsdSecurityGroupId = Nothing
    , _dvsdAvailabilityZones = Nothing
    }

-- | The identifiers of the subnets for the directory servers.
dvsdSubnetIds :: Lens' DirectoryVPCSettingsDescription [Text]
dvsdSubnetIds = lens _dvsdSubnetIds (\ s a -> s{_dvsdSubnetIds = a}) . _Default . _Coerce;

-- | The identifier of the VPC that the directory is in.
dvsdVPCId :: Lens' DirectoryVPCSettingsDescription (Maybe Text)
dvsdVPCId = lens _dvsdVPCId (\ s a -> s{_dvsdVPCId = a});

-- | The security group identifier for the directory.
dvsdSecurityGroupId :: Lens' DirectoryVPCSettingsDescription (Maybe Text)
dvsdSecurityGroupId = lens _dvsdSecurityGroupId (\ s a -> s{_dvsdSecurityGroupId = a});

-- | The list of Availability Zones that the directory is in.
dvsdAvailabilityZones :: Lens' DirectoryVPCSettingsDescription [Text]
dvsdAvailabilityZones = lens _dvsdAvailabilityZones (\ s a -> s{_dvsdAvailabilityZones = a}) . _Default . _Coerce;

instance FromJSON DirectoryVPCSettingsDescription
         where
        parseJSON
          = withObject "DirectoryVPCSettingsDescription"
              (\ x ->
                 DirectoryVPCSettingsDescription' <$>
                   (x .:? "SubnetIds" .!= mempty) <*> (x .:? "VpcId")
                     <*> (x .:? "SecurityGroupId")
                     <*> (x .:? "AvailabilityZones" .!= mempty))

-- | Contains information about a Remote Authentication Dial In User Service
-- (RADIUS) server.
--
-- /See:/ 'radiusSettings' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsDisplayLabel'
--
-- * 'rsRadiusServers'
--
-- * 'rsRadiusRetries'
--
-- * 'rsAuthenticationProtocol'
--
-- * 'rsUseSameUsername'
--
-- * 'rsSharedSecret'
--
-- * 'rsRadiusTimeout'
--
-- * 'rsRadiusPort'
data RadiusSettings = RadiusSettings'
    { _rsDisplayLabel           :: !(Maybe Text)
    , _rsRadiusServers          :: !(Maybe [Text])
    , _rsRadiusRetries          :: !(Maybe Nat)
    , _rsAuthenticationProtocol :: !(Maybe RadiusAuthenticationProtocol)
    , _rsUseSameUsername        :: !(Maybe Bool)
    , _rsSharedSecret           :: !(Maybe (Sensitive Text))
    , _rsRadiusTimeout          :: !(Maybe Nat)
    , _rsRadiusPort             :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RadiusSettings' smart constructor.
radiusSettings :: RadiusSettings
radiusSettings =
    RadiusSettings'
    { _rsDisplayLabel = Nothing
    , _rsRadiusServers = Nothing
    , _rsRadiusRetries = Nothing
    , _rsAuthenticationProtocol = Nothing
    , _rsUseSameUsername = Nothing
    , _rsSharedSecret = Nothing
    , _rsRadiusTimeout = Nothing
    , _rsRadiusPort = Nothing
    }

-- | Not currently used.
rsDisplayLabel :: Lens' RadiusSettings (Maybe Text)
rsDisplayLabel = lens _rsDisplayLabel (\ s a -> s{_rsDisplayLabel = a});

-- | An array of strings that contains the IP addresses of the RADIUS server
-- endpoints, or the IP addresses of your RADIUS server load balancer.
rsRadiusServers :: Lens' RadiusSettings [Text]
rsRadiusServers = lens _rsRadiusServers (\ s a -> s{_rsRadiusServers = a}) . _Default . _Coerce;

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

-- | The shared secret code that was specified when your RADIUS endpoints
-- were created.
rsSharedSecret :: Lens' RadiusSettings (Maybe Text)
rsSharedSecret = lens _rsSharedSecret (\ s a -> s{_rsSharedSecret = a}) . mapping _Sensitive;

-- | The amount of time, in seconds, to wait for the RADIUS server to
-- respond.
rsRadiusTimeout :: Lens' RadiusSettings (Maybe Natural)
rsRadiusTimeout = lens _rsRadiusTimeout (\ s a -> s{_rsRadiusTimeout = a}) . mapping _Nat;

-- | The port that your RADIUS server is using for communications. Your
-- on-premises network must allow inbound traffic over this port from the
-- AWS Directory Service servers.
rsRadiusPort :: Lens' RadiusSettings (Maybe Natural)
rsRadiusPort = lens _rsRadiusPort (\ s a -> s{_rsRadiusPort = a}) . mapping _Nat;

instance FromJSON RadiusSettings where
        parseJSON
          = withObject "RadiusSettings"
              (\ x ->
                 RadiusSettings' <$>
                   (x .:? "DisplayLabel") <*>
                     (x .:? "RadiusServers" .!= mempty)
                     <*> (x .:? "RadiusRetries")
                     <*> (x .:? "AuthenticationProtocol")
                     <*> (x .:? "UseSameUsername")
                     <*> (x .:? "SharedSecret")
                     <*> (x .:? "RadiusTimeout")
                     <*> (x .:? "RadiusPort"))

instance ToJSON RadiusSettings where
        toJSON RadiusSettings'{..}
          = object
              ["DisplayLabel" .= _rsDisplayLabel,
               "RadiusServers" .= _rsRadiusServers,
               "RadiusRetries" .= _rsRadiusRetries,
               "AuthenticationProtocol" .=
                 _rsAuthenticationProtocol,
               "UseSameUsername" .= _rsUseSameUsername,
               "SharedSecret" .= _rsSharedSecret,
               "RadiusTimeout" .= _rsRadiusTimeout,
               "RadiusPort" .= _rsRadiusPort]

-- | Describes a directory snapshot.
--
-- /See:/ 'snapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sDirectoryId'
--
-- * 'sStatus'
--
-- * 'sStartTime'
--
-- * 'sName'
--
-- * 'sType'
--
-- * 'sSnapshotId'
data Snapshot = Snapshot'
    { _sDirectoryId :: !(Maybe Text)
    , _sStatus      :: !(Maybe SnapshotStatus)
    , _sStartTime   :: !(Maybe POSIX)
    , _sName        :: !(Maybe Text)
    , _sType        :: !(Maybe SnapshotType)
    , _sSnapshotId  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Snapshot' smart constructor.
snapshot :: Snapshot
snapshot =
    Snapshot'
    { _sDirectoryId = Nothing
    , _sStatus = Nothing
    , _sStartTime = Nothing
    , _sName = Nothing
    , _sType = Nothing
    , _sSnapshotId = Nothing
    }

-- | The directory identifier.
sDirectoryId :: Lens' Snapshot (Maybe Text)
sDirectoryId = lens _sDirectoryId (\ s a -> s{_sDirectoryId = a});

-- | The snapshot status.
sStatus :: Lens' Snapshot (Maybe SnapshotStatus)
sStatus = lens _sStatus (\ s a -> s{_sStatus = a});

-- | The date and time that the snapshot was taken.
sStartTime :: Lens' Snapshot (Maybe UTCTime)
sStartTime = lens _sStartTime (\ s a -> s{_sStartTime = a}) . mapping _Time;

-- | The descriptive name of the snapshot.
sName :: Lens' Snapshot (Maybe Text)
sName = lens _sName (\ s a -> s{_sName = a});

-- | The snapshot type.
sType :: Lens' Snapshot (Maybe SnapshotType)
sType = lens _sType (\ s a -> s{_sType = a});

-- | The snapshot identifier.
sSnapshotId :: Lens' Snapshot (Maybe Text)
sSnapshotId = lens _sSnapshotId (\ s a -> s{_sSnapshotId = a});

instance FromJSON Snapshot where
        parseJSON
          = withObject "Snapshot"
              (\ x ->
                 Snapshot' <$>
                   (x .:? "DirectoryId") <*> (x .:? "Status") <*>
                     (x .:? "StartTime")
                     <*> (x .:? "Name")
                     <*> (x .:? "Type")
                     <*> (x .:? "SnapshotId"))

-- | Contains manual snapshot limit information for a directory.
--
-- /See:/ 'snapshotLimits' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slManualSnapshotsLimitReached'
--
-- * 'slManualSnapshotsCurrentCount'
--
-- * 'slManualSnapshotsLimit'
data SnapshotLimits = SnapshotLimits'
    { _slManualSnapshotsLimitReached :: !(Maybe Bool)
    , _slManualSnapshotsCurrentCount :: !(Maybe Nat)
    , _slManualSnapshotsLimit        :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SnapshotLimits' smart constructor.
snapshotLimits :: SnapshotLimits
snapshotLimits =
    SnapshotLimits'
    { _slManualSnapshotsLimitReached = Nothing
    , _slManualSnapshotsCurrentCount = Nothing
    , _slManualSnapshotsLimit = Nothing
    }

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
                   (x .:? "ManualSnapshotsLimitReached") <*>
                     (x .:? "ManualSnapshotsCurrentCount")
                     <*> (x .:? "ManualSnapshotsLimit"))
