{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.Product where

import           Network.AWS.DirectoryService.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | Represents a named directory attribute.
--
-- /See:/ 'attribute' smart constructor.
data Attribute = Attribute'
    { _aValue :: !(Maybe Text)
    , _aName  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Attribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aValue'
--
-- * 'aName'
attribute
    :: Attribute
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

instance Hashable Attribute

instance NFData Attribute

instance ToJSON Attribute where
        toJSON Attribute'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _aValue, ("Name" .=) <$> _aName])

-- | Contains information about a computer account in a directory.
--
-- /See:/ 'computer' smart constructor.
data Computer = Computer'
    { _cComputerId         :: !(Maybe Text)
    , _cComputerAttributes :: !(Maybe [Attribute])
    , _cComputerName       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Computer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cComputerId'
--
-- * 'cComputerAttributes'
--
-- * 'cComputerName'
computer
    :: Computer
computer =
    Computer'
    { _cComputerId = Nothing
    , _cComputerAttributes = Nothing
    , _cComputerName = Nothing
    }

-- | The identifier of the computer.
cComputerId :: Lens' Computer (Maybe Text)
cComputerId = lens _cComputerId (\ s a -> s{_cComputerId = a});

-- | An array of < Attribute> objects containing the LDAP attributes that belong to the computer account.
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

instance Hashable Computer

instance NFData Computer

-- | Points to a remote domain with which you are setting up a trust relationship. Conditional forwarders are required in order to set up a trust relationship with another domain.
--
-- /See:/ 'conditionalForwarder' smart constructor.
data ConditionalForwarder = ConditionalForwarder'
    { _cfDNSIPAddrs       :: !(Maybe [Text])
    , _cfRemoteDomainName :: !(Maybe Text)
    , _cfReplicationScope :: !(Maybe ReplicationScope)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ConditionalForwarder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfDNSIPAddrs'
--
-- * 'cfRemoteDomainName'
--
-- * 'cfReplicationScope'
conditionalForwarder
    :: ConditionalForwarder
conditionalForwarder =
    ConditionalForwarder'
    { _cfDNSIPAddrs = Nothing
    , _cfRemoteDomainName = Nothing
    , _cfReplicationScope = Nothing
    }

-- | The IP addresses of the remote DNS server associated with RemoteDomainName. This is the IP address of the DNS server that your conditional forwarder points to.
cfDNSIPAddrs :: Lens' ConditionalForwarder [Text]
cfDNSIPAddrs = lens _cfDNSIPAddrs (\ s a -> s{_cfDNSIPAddrs = a}) . _Default . _Coerce;

-- | The fully qualified domain name (FQDN) of the remote domains pointed to by the conditional forwarder.
cfRemoteDomainName :: Lens' ConditionalForwarder (Maybe Text)
cfRemoteDomainName = lens _cfRemoteDomainName (\ s a -> s{_cfRemoteDomainName = a});

-- | The replication scope of the conditional forwarder. The only allowed value is 'Domain', which will replicate the conditional forwarder to all of the domain controllers for your AWS directory.
cfReplicationScope :: Lens' ConditionalForwarder (Maybe ReplicationScope)
cfReplicationScope = lens _cfReplicationScope (\ s a -> s{_cfReplicationScope = a});

instance FromJSON ConditionalForwarder where
        parseJSON
          = withObject "ConditionalForwarder"
              (\ x ->
                 ConditionalForwarder' <$>
                   (x .:? "DnsIpAddrs" .!= mempty) <*>
                     (x .:? "RemoteDomainName")
                     <*> (x .:? "ReplicationScope"))

instance Hashable ConditionalForwarder

instance NFData ConditionalForwarder

-- | Contains information for the < ConnectDirectory> operation when an AD Connector directory is being created.
--
-- /See:/ 'directoryConnectSettings' smart constructor.
data DirectoryConnectSettings = DirectoryConnectSettings'
    { _dcsVPCId            :: !Text
    , _dcsSubnetIds        :: ![Text]
    , _dcsCustomerDNSIPs   :: ![Text]
    , _dcsCustomerUserName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DirectoryConnectSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsVPCId'
--
-- * 'dcsSubnetIds'
--
-- * 'dcsCustomerDNSIPs'
--
-- * 'dcsCustomerUserName'
directoryConnectSettings
    :: Text -- ^ 'dcsVPCId'
    -> Text -- ^ 'dcsCustomerUserName'
    -> DirectoryConnectSettings
directoryConnectSettings pVPCId_ pCustomerUserName_ =
    DirectoryConnectSettings'
    { _dcsVPCId = pVPCId_
    , _dcsSubnetIds = mempty
    , _dcsCustomerDNSIPs = mempty
    , _dcsCustomerUserName = pCustomerUserName_
    }

-- | The identifier of the VPC in which the AD Connector is created.
dcsVPCId :: Lens' DirectoryConnectSettings Text
dcsVPCId = lens _dcsVPCId (\ s a -> s{_dcsVPCId = a});

-- | A list of subnet identifiers in the VPC in which the AD Connector is created.
dcsSubnetIds :: Lens' DirectoryConnectSettings [Text]
dcsSubnetIds = lens _dcsSubnetIds (\ s a -> s{_dcsSubnetIds = a}) . _Coerce;

-- | A list of one or more IP addresses of DNS servers or domain controllers in the on-premises directory.
dcsCustomerDNSIPs :: Lens' DirectoryConnectSettings [Text]
dcsCustomerDNSIPs = lens _dcsCustomerDNSIPs (\ s a -> s{_dcsCustomerDNSIPs = a}) . _Coerce;

-- | The username of an account in the on-premises directory that is used to connect to the directory. This account must have the following privileges:
--
-- -   Read users and groups
-- -   Create computer objects
-- -   Join computers to the domain
dcsCustomerUserName :: Lens' DirectoryConnectSettings Text
dcsCustomerUserName = lens _dcsCustomerUserName (\ s a -> s{_dcsCustomerUserName = a});

instance Hashable DirectoryConnectSettings

instance NFData DirectoryConnectSettings

instance ToJSON DirectoryConnectSettings where
        toJSON DirectoryConnectSettings'{..}
          = object
              (catMaybes
                 [Just ("VpcId" .= _dcsVPCId),
                  Just ("SubnetIds" .= _dcsSubnetIds),
                  Just ("CustomerDnsIps" .= _dcsCustomerDNSIPs),
                  Just ("CustomerUserName" .= _dcsCustomerUserName)])

-- | Contains information about an AD Connector directory.
--
-- /See:/ 'directoryConnectSettingsDescription' smart constructor.
data DirectoryConnectSettingsDescription = DirectoryConnectSettingsDescription'
    { _dcsdCustomerUserName  :: !(Maybe Text)
    , _dcsdSubnetIds         :: !(Maybe [Text])
    , _dcsdVPCId             :: !(Maybe Text)
    , _dcsdSecurityGroupId   :: !(Maybe Text)
    , _dcsdConnectIPs        :: !(Maybe [Text])
    , _dcsdAvailabilityZones :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DirectoryConnectSettingsDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsdCustomerUserName'
--
-- * 'dcsdSubnetIds'
--
-- * 'dcsdVPCId'
--
-- * 'dcsdSecurityGroupId'
--
-- * 'dcsdConnectIPs'
--
-- * 'dcsdAvailabilityZones'
directoryConnectSettingsDescription
    :: DirectoryConnectSettingsDescription
directoryConnectSettingsDescription =
    DirectoryConnectSettingsDescription'
    { _dcsdCustomerUserName = Nothing
    , _dcsdSubnetIds = Nothing
    , _dcsdVPCId = Nothing
    , _dcsdSecurityGroupId = Nothing
    , _dcsdConnectIPs = Nothing
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

-- | The security group identifier for the AD Connector directory.
dcsdSecurityGroupId :: Lens' DirectoryConnectSettingsDescription (Maybe Text)
dcsdSecurityGroupId = lens _dcsdSecurityGroupId (\ s a -> s{_dcsdSecurityGroupId = a});

-- | The IP addresses of the AD Connector servers.
dcsdConnectIPs :: Lens' DirectoryConnectSettingsDescription [Text]
dcsdConnectIPs = lens _dcsdConnectIPs (\ s a -> s{_dcsdConnectIPs = a}) . _Default . _Coerce;

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
                     <*> (x .:? "SecurityGroupId")
                     <*> (x .:? "ConnectIps" .!= mempty)
                     <*> (x .:? "AvailabilityZones" .!= mempty))

instance Hashable DirectoryConnectSettingsDescription

instance NFData DirectoryConnectSettingsDescription

-- | Contains information about an AWS Directory Service directory.
--
-- /See:/ 'directoryDescription' smart constructor.
data DirectoryDescription = DirectoryDescription'
    { _ddRadiusStatus             :: !(Maybe RadiusStatus)
    , _ddStage                    :: !(Maybe DirectoryStage)
    , _ddDirectoryId              :: !(Maybe Text)
    , _ddAccessURL                :: !(Maybe Text)
    , _ddShortName                :: !(Maybe Text)
    , _ddSize                     :: !(Maybe DirectorySize)
    , _ddRadiusSettings           :: !(Maybe RadiusSettings)
    , _ddLaunchTime               :: !(Maybe POSIX)
    , _ddAlias                    :: !(Maybe Text)
    , _ddName                     :: !(Maybe Text)
    , _ddStageLastUpdatedDateTime :: !(Maybe POSIX)
    , _ddSSOEnabled               :: !(Maybe Bool)
    , _ddDNSIPAddrs               :: !(Maybe [Text])
    , _ddVPCSettings              :: !(Maybe DirectoryVPCSettingsDescription)
    , _ddType                     :: !(Maybe DirectoryType)
    , _ddStageReason              :: !(Maybe Text)
    , _ddConnectSettings          :: !(Maybe DirectoryConnectSettingsDescription)
    , _ddDescription              :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DirectoryDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddRadiusStatus'
--
-- * 'ddStage'
--
-- * 'ddDirectoryId'
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
-- * 'ddStageLastUpdatedDateTime'
--
-- * 'ddSSOEnabled'
--
-- * 'ddDNSIPAddrs'
--
-- * 'ddVPCSettings'
--
-- * 'ddType'
--
-- * 'ddStageReason'
--
-- * 'ddConnectSettings'
--
-- * 'ddDescription'
directoryDescription
    :: DirectoryDescription
directoryDescription =
    DirectoryDescription'
    { _ddRadiusStatus = Nothing
    , _ddStage = Nothing
    , _ddDirectoryId = Nothing
    , _ddAccessURL = Nothing
    , _ddShortName = Nothing
    , _ddSize = Nothing
    , _ddRadiusSettings = Nothing
    , _ddLaunchTime = Nothing
    , _ddAlias = Nothing
    , _ddName = Nothing
    , _ddStageLastUpdatedDateTime = Nothing
    , _ddSSOEnabled = Nothing
    , _ddDNSIPAddrs = Nothing
    , _ddVPCSettings = Nothing
    , _ddType = Nothing
    , _ddStageReason = Nothing
    , _ddConnectSettings = Nothing
    , _ddDescription = Nothing
    }

-- | The status of the RADIUS MFA server connection.
ddRadiusStatus :: Lens' DirectoryDescription (Maybe RadiusStatus)
ddRadiusStatus = lens _ddRadiusStatus (\ s a -> s{_ddRadiusStatus = a});

-- | The current stage of the directory.
ddStage :: Lens' DirectoryDescription (Maybe DirectoryStage)
ddStage = lens _ddStage (\ s a -> s{_ddStage = a});

-- | The directory identifier.
ddDirectoryId :: Lens' DirectoryDescription (Maybe Text)
ddDirectoryId = lens _ddDirectoryId (\ s a -> s{_ddDirectoryId = a});

-- | The access URL for the directory, such as 'http:\/\/&#x3C;alias&#x3E;.awsapps.com'. If no alias has been created for the directory, '&#x3C;alias&#x3E;' is the directory identifier, such as 'd-XXXXXXXXXX'.
ddAccessURL :: Lens' DirectoryDescription (Maybe Text)
ddAccessURL = lens _ddAccessURL (\ s a -> s{_ddAccessURL = a});

-- | The short name of the directory.
ddShortName :: Lens' DirectoryDescription (Maybe Text)
ddShortName = lens _ddShortName (\ s a -> s{_ddShortName = a});

-- | The directory size.
ddSize :: Lens' DirectoryDescription (Maybe DirectorySize)
ddSize = lens _ddSize (\ s a -> s{_ddSize = a});

-- | A < RadiusSettings> object that contains information about the RADIUS server configured for this directory.
ddRadiusSettings :: Lens' DirectoryDescription (Maybe RadiusSettings)
ddRadiusSettings = lens _ddRadiusSettings (\ s a -> s{_ddRadiusSettings = a});

-- | Specifies when the directory was created.
ddLaunchTime :: Lens' DirectoryDescription (Maybe UTCTime)
ddLaunchTime = lens _ddLaunchTime (\ s a -> s{_ddLaunchTime = a}) . mapping _Time;

-- | The alias for the directory. If no alias has been created for the directory, the alias is the directory identifier, such as 'd-XXXXXXXXXX'.
ddAlias :: Lens' DirectoryDescription (Maybe Text)
ddAlias = lens _ddAlias (\ s a -> s{_ddAlias = a});

-- | The fully-qualified name of the directory.
ddName :: Lens' DirectoryDescription (Maybe Text)
ddName = lens _ddName (\ s a -> s{_ddName = a});

-- | The date and time that the stage was last updated.
ddStageLastUpdatedDateTime :: Lens' DirectoryDescription (Maybe UTCTime)
ddStageLastUpdatedDateTime = lens _ddStageLastUpdatedDateTime (\ s a -> s{_ddStageLastUpdatedDateTime = a}) . mapping _Time;

-- | Indicates if single-sign on is enabled for the directory. For more information, see < EnableSso> and < DisableSso>.
ddSSOEnabled :: Lens' DirectoryDescription (Maybe Bool)
ddSSOEnabled = lens _ddSSOEnabled (\ s a -> s{_ddSSOEnabled = a});

-- | The IP addresses of the DNS servers for the directory. For a Simple AD or Microsoft AD directory, these are the IP addresses of the Simple AD or Microsoft AD directory servers. For an AD Connector directory, these are the IP addresses of the DNS servers or domain controllers in the on-premises directory to which the AD Connector is connected.
ddDNSIPAddrs :: Lens' DirectoryDescription [Text]
ddDNSIPAddrs = lens _ddDNSIPAddrs (\ s a -> s{_ddDNSIPAddrs = a}) . _Default . _Coerce;

-- | A < DirectoryVpcSettingsDescription> object that contains additional information about a directory. This member is only present if the directory is a Simple AD or Managed AD directory.
ddVPCSettings :: Lens' DirectoryDescription (Maybe DirectoryVPCSettingsDescription)
ddVPCSettings = lens _ddVPCSettings (\ s a -> s{_ddVPCSettings = a});

-- | The directory size.
ddType :: Lens' DirectoryDescription (Maybe DirectoryType)
ddType = lens _ddType (\ s a -> s{_ddType = a});

-- | Additional information about the directory stage.
ddStageReason :: Lens' DirectoryDescription (Maybe Text)
ddStageReason = lens _ddStageReason (\ s a -> s{_ddStageReason = a});

-- | A < DirectoryConnectSettingsDescription> object that contains additional information about an AD Connector directory. This member is only present if the directory is an AD Connector directory.
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
                   (x .:? "RadiusStatus") <*> (x .:? "Stage") <*>
                     (x .:? "DirectoryId")
                     <*> (x .:? "AccessUrl")
                     <*> (x .:? "ShortName")
                     <*> (x .:? "Size")
                     <*> (x .:? "RadiusSettings")
                     <*> (x .:? "LaunchTime")
                     <*> (x .:? "Alias")
                     <*> (x .:? "Name")
                     <*> (x .:? "StageLastUpdatedDateTime")
                     <*> (x .:? "SsoEnabled")
                     <*> (x .:? "DnsIpAddrs" .!= mempty)
                     <*> (x .:? "VpcSettings")
                     <*> (x .:? "Type")
                     <*> (x .:? "StageReason")
                     <*> (x .:? "ConnectSettings")
                     <*> (x .:? "Description"))

instance Hashable DirectoryDescription

instance NFData DirectoryDescription

-- | Contains directory limit information for a region.
--
-- /See:/ 'directoryLimits' smart constructor.
data DirectoryLimits = DirectoryLimits'
    { _dlConnectedDirectoriesCurrentCount :: !(Maybe Nat)
    , _dlCloudOnlyMicrosoftADLimitReached :: !(Maybe Bool)
    , _dlConnectedDirectoriesLimit        :: !(Maybe Nat)
    , _dlConnectedDirectoriesLimitReached :: !(Maybe Bool)
    , _dlCloudOnlyMicrosoftADLimit        :: !(Maybe Nat)
    , _dlCloudOnlyDirectoriesLimit        :: !(Maybe Nat)
    , _dlCloudOnlyDirectoriesCurrentCount :: !(Maybe Nat)
    , _dlCloudOnlyDirectoriesLimitReached :: !(Maybe Bool)
    , _dlCloudOnlyMicrosoftADCurrentCount :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DirectoryLimits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlConnectedDirectoriesCurrentCount'
--
-- * 'dlCloudOnlyMicrosoftADLimitReached'
--
-- * 'dlConnectedDirectoriesLimit'
--
-- * 'dlConnectedDirectoriesLimitReached'
--
-- * 'dlCloudOnlyMicrosoftADLimit'
--
-- * 'dlCloudOnlyDirectoriesLimit'
--
-- * 'dlCloudOnlyDirectoriesCurrentCount'
--
-- * 'dlCloudOnlyDirectoriesLimitReached'
--
-- * 'dlCloudOnlyMicrosoftADCurrentCount'
directoryLimits
    :: DirectoryLimits
directoryLimits =
    DirectoryLimits'
    { _dlConnectedDirectoriesCurrentCount = Nothing
    , _dlCloudOnlyMicrosoftADLimitReached = Nothing
    , _dlConnectedDirectoriesLimit = Nothing
    , _dlConnectedDirectoriesLimitReached = Nothing
    , _dlCloudOnlyMicrosoftADLimit = Nothing
    , _dlCloudOnlyDirectoriesLimit = Nothing
    , _dlCloudOnlyDirectoriesCurrentCount = Nothing
    , _dlCloudOnlyDirectoriesLimitReached = Nothing
    , _dlCloudOnlyMicrosoftADCurrentCount = Nothing
    }

-- | The current number of connected directories in the region.
dlConnectedDirectoriesCurrentCount :: Lens' DirectoryLimits (Maybe Natural)
dlConnectedDirectoriesCurrentCount = lens _dlConnectedDirectoriesCurrentCount (\ s a -> s{_dlConnectedDirectoriesCurrentCount = a}) . mapping _Nat;

-- | Indicates if the Microsoft AD directory limit has been reached.
dlCloudOnlyMicrosoftADLimitReached :: Lens' DirectoryLimits (Maybe Bool)
dlCloudOnlyMicrosoftADLimitReached = lens _dlCloudOnlyMicrosoftADLimitReached (\ s a -> s{_dlCloudOnlyMicrosoftADLimitReached = a});

-- | The maximum number of connected directories allowed in the region.
dlConnectedDirectoriesLimit :: Lens' DirectoryLimits (Maybe Natural)
dlConnectedDirectoriesLimit = lens _dlConnectedDirectoriesLimit (\ s a -> s{_dlConnectedDirectoriesLimit = a}) . mapping _Nat;

-- | Indicates if the connected directory limit has been reached.
dlConnectedDirectoriesLimitReached :: Lens' DirectoryLimits (Maybe Bool)
dlConnectedDirectoriesLimitReached = lens _dlConnectedDirectoriesLimitReached (\ s a -> s{_dlConnectedDirectoriesLimitReached = a});

-- | The maximum number of Microsoft AD directories allowed in the region.
dlCloudOnlyMicrosoftADLimit :: Lens' DirectoryLimits (Maybe Natural)
dlCloudOnlyMicrosoftADLimit = lens _dlCloudOnlyMicrosoftADLimit (\ s a -> s{_dlCloudOnlyMicrosoftADLimit = a}) . mapping _Nat;

-- | The maximum number of cloud directories allowed in the region.
dlCloudOnlyDirectoriesLimit :: Lens' DirectoryLimits (Maybe Natural)
dlCloudOnlyDirectoriesLimit = lens _dlCloudOnlyDirectoriesLimit (\ s a -> s{_dlCloudOnlyDirectoriesLimit = a}) . mapping _Nat;

-- | The current number of cloud directories in the region.
dlCloudOnlyDirectoriesCurrentCount :: Lens' DirectoryLimits (Maybe Natural)
dlCloudOnlyDirectoriesCurrentCount = lens _dlCloudOnlyDirectoriesCurrentCount (\ s a -> s{_dlCloudOnlyDirectoriesCurrentCount = a}) . mapping _Nat;

-- | Indicates if the cloud directory limit has been reached.
dlCloudOnlyDirectoriesLimitReached :: Lens' DirectoryLimits (Maybe Bool)
dlCloudOnlyDirectoriesLimitReached = lens _dlCloudOnlyDirectoriesLimitReached (\ s a -> s{_dlCloudOnlyDirectoriesLimitReached = a});

-- | The current number of Microsoft AD directories in the region.
dlCloudOnlyMicrosoftADCurrentCount :: Lens' DirectoryLimits (Maybe Natural)
dlCloudOnlyMicrosoftADCurrentCount = lens _dlCloudOnlyMicrosoftADCurrentCount (\ s a -> s{_dlCloudOnlyMicrosoftADCurrentCount = a}) . mapping _Nat;

instance FromJSON DirectoryLimits where
        parseJSON
          = withObject "DirectoryLimits"
              (\ x ->
                 DirectoryLimits' <$>
                   (x .:? "ConnectedDirectoriesCurrentCount") <*>
                     (x .:? "CloudOnlyMicrosoftADLimitReached")
                     <*> (x .:? "ConnectedDirectoriesLimit")
                     <*> (x .:? "ConnectedDirectoriesLimitReached")
                     <*> (x .:? "CloudOnlyMicrosoftADLimit")
                     <*> (x .:? "CloudOnlyDirectoriesLimit")
                     <*> (x .:? "CloudOnlyDirectoriesCurrentCount")
                     <*> (x .:? "CloudOnlyDirectoriesLimitReached")
                     <*> (x .:? "CloudOnlyMicrosoftADCurrentCount"))

instance Hashable DirectoryLimits

instance NFData DirectoryLimits

-- | Contains VPC information for the < CreateDirectory> or < CreateMicrosoftAD> operation.
--
-- /See:/ 'directoryVPCSettings' smart constructor.
data DirectoryVPCSettings = DirectoryVPCSettings'
    { _dvsVPCId     :: !Text
    , _dvsSubnetIds :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DirectoryVPCSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvsVPCId'
--
-- * 'dvsSubnetIds'
directoryVPCSettings
    :: Text -- ^ 'dvsVPCId'
    -> DirectoryVPCSettings
directoryVPCSettings pVPCId_ =
    DirectoryVPCSettings'
    { _dvsVPCId = pVPCId_
    , _dvsSubnetIds = mempty
    }

-- | The identifier of the VPC in which to create the directory.
dvsVPCId :: Lens' DirectoryVPCSettings Text
dvsVPCId = lens _dvsVPCId (\ s a -> s{_dvsVPCId = a});

-- | The identifiers of the subnets for the directory servers. The two subnets must be in different Availability Zones. AWS Directory Service creates a directory server and a DNS server in each of these subnets.
dvsSubnetIds :: Lens' DirectoryVPCSettings [Text]
dvsSubnetIds = lens _dvsSubnetIds (\ s a -> s{_dvsSubnetIds = a}) . _Coerce;

instance Hashable DirectoryVPCSettings

instance NFData DirectoryVPCSettings

instance ToJSON DirectoryVPCSettings where
        toJSON DirectoryVPCSettings'{..}
          = object
              (catMaybes
                 [Just ("VpcId" .= _dvsVPCId),
                  Just ("SubnetIds" .= _dvsSubnetIds)])

-- | Contains information about the directory.
--
-- /See:/ 'directoryVPCSettingsDescription' smart constructor.
data DirectoryVPCSettingsDescription = DirectoryVPCSettingsDescription'
    { _dvsdSubnetIds         :: !(Maybe [Text])
    , _dvsdVPCId             :: !(Maybe Text)
    , _dvsdSecurityGroupId   :: !(Maybe Text)
    , _dvsdAvailabilityZones :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DirectoryVPCSettingsDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvsdSubnetIds'
--
-- * 'dvsdVPCId'
--
-- * 'dvsdSecurityGroupId'
--
-- * 'dvsdAvailabilityZones'
directoryVPCSettingsDescription
    :: DirectoryVPCSettingsDescription
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

-- | The security group identifier for the directory. If the directory was created before 8\/1\/2014, this is the identifier of the directory members security group that was created when the directory was created. If the directory was created after this date, this value is null.
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

instance Hashable DirectoryVPCSettingsDescription

instance NFData DirectoryVPCSettingsDescription

-- | Information about SNS topic and AWS Directory Service directory associations.
--
-- /See:/ 'eventTopic' smart constructor.
data EventTopic = EventTopic'
    { _etStatus          :: !(Maybe TopicStatus)
    , _etDirectoryId     :: !(Maybe Text)
    , _etTopicName       :: !(Maybe Text)
    , _etTopicARN        :: !(Maybe Text)
    , _etCreatedDateTime :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EventTopic' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etStatus'
--
-- * 'etDirectoryId'
--
-- * 'etTopicName'
--
-- * 'etTopicARN'
--
-- * 'etCreatedDateTime'
eventTopic
    :: EventTopic
eventTopic =
    EventTopic'
    { _etStatus = Nothing
    , _etDirectoryId = Nothing
    , _etTopicName = Nothing
    , _etTopicARN = Nothing
    , _etCreatedDateTime = Nothing
    }

-- | The topic registration status.
etStatus :: Lens' EventTopic (Maybe TopicStatus)
etStatus = lens _etStatus (\ s a -> s{_etStatus = a});

-- | The Directory ID of an AWS Directory Service directory that will publish status messages to an SNS topic.
etDirectoryId :: Lens' EventTopic (Maybe Text)
etDirectoryId = lens _etDirectoryId (\ s a -> s{_etDirectoryId = a});

-- | The name of an AWS SNS topic the receives status messages from the directory.
etTopicName :: Lens' EventTopic (Maybe Text)
etTopicName = lens _etTopicName (\ s a -> s{_etTopicName = a});

-- | The SNS topic ARN (Amazon Resource Name).
etTopicARN :: Lens' EventTopic (Maybe Text)
etTopicARN = lens _etTopicARN (\ s a -> s{_etTopicARN = a});

-- | The date and time of when you associated your directory with the SNS topic.
etCreatedDateTime :: Lens' EventTopic (Maybe UTCTime)
etCreatedDateTime = lens _etCreatedDateTime (\ s a -> s{_etCreatedDateTime = a}) . mapping _Time;

instance FromJSON EventTopic where
        parseJSON
          = withObject "EventTopic"
              (\ x ->
                 EventTopic' <$>
                   (x .:? "Status") <*> (x .:? "DirectoryId") <*>
                     (x .:? "TopicName")
                     <*> (x .:? "TopicArn")
                     <*> (x .:? "CreatedDateTime"))

instance Hashable EventTopic

instance NFData EventTopic

-- | Contains information about a Remote Authentication Dial In User Service (RADIUS) server.
--
-- /See:/ 'radiusSettings' smart constructor.
data RadiusSettings = RadiusSettings'
    { _rsDisplayLabel           :: !(Maybe Text)
    , _rsRadiusRetries          :: !(Maybe Nat)
    , _rsAuthenticationProtocol :: !(Maybe RadiusAuthenticationProtocol)
    , _rsRadiusServers          :: !(Maybe [Text])
    , _rsUseSameUsername        :: !(Maybe Bool)
    , _rsSharedSecret           :: !(Maybe (Sensitive Text))
    , _rsRadiusTimeout          :: !(Maybe Nat)
    , _rsRadiusPort             :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RadiusSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsDisplayLabel'
--
-- * 'rsRadiusRetries'
--
-- * 'rsAuthenticationProtocol'
--
-- * 'rsRadiusServers'
--
-- * 'rsUseSameUsername'
--
-- * 'rsSharedSecret'
--
-- * 'rsRadiusTimeout'
--
-- * 'rsRadiusPort'
radiusSettings
    :: RadiusSettings
radiusSettings =
    RadiusSettings'
    { _rsDisplayLabel = Nothing
    , _rsRadiusRetries = Nothing
    , _rsAuthenticationProtocol = Nothing
    , _rsRadiusServers = Nothing
    , _rsUseSameUsername = Nothing
    , _rsSharedSecret = Nothing
    , _rsRadiusTimeout = Nothing
    , _rsRadiusPort = Nothing
    }

-- | Not currently used.
rsDisplayLabel :: Lens' RadiusSettings (Maybe Text)
rsDisplayLabel = lens _rsDisplayLabel (\ s a -> s{_rsDisplayLabel = a});

-- | The maximum number of times that communication with the RADIUS server is attempted.
rsRadiusRetries :: Lens' RadiusSettings (Maybe Natural)
rsRadiusRetries = lens _rsRadiusRetries (\ s a -> s{_rsRadiusRetries = a}) . mapping _Nat;

-- | The protocol specified for your RADIUS endpoints.
rsAuthenticationProtocol :: Lens' RadiusSettings (Maybe RadiusAuthenticationProtocol)
rsAuthenticationProtocol = lens _rsAuthenticationProtocol (\ s a -> s{_rsAuthenticationProtocol = a});

-- | An array of strings that contains the IP addresses of the RADIUS server endpoints, or the IP addresses of your RADIUS server load balancer.
rsRadiusServers :: Lens' RadiusSettings [Text]
rsRadiusServers = lens _rsRadiusServers (\ s a -> s{_rsRadiusServers = a}) . _Default . _Coerce;

-- | Not currently used.
rsUseSameUsername :: Lens' RadiusSettings (Maybe Bool)
rsUseSameUsername = lens _rsUseSameUsername (\ s a -> s{_rsUseSameUsername = a});

-- | The shared secret code that was specified when your RADIUS endpoints were created.
rsSharedSecret :: Lens' RadiusSettings (Maybe Text)
rsSharedSecret = lens _rsSharedSecret (\ s a -> s{_rsSharedSecret = a}) . mapping _Sensitive;

-- | The amount of time, in seconds, to wait for the RADIUS server to respond.
rsRadiusTimeout :: Lens' RadiusSettings (Maybe Natural)
rsRadiusTimeout = lens _rsRadiusTimeout (\ s a -> s{_rsRadiusTimeout = a}) . mapping _Nat;

-- | The port that your RADIUS server is using for communications. Your on-premises network must allow inbound traffic over this port from the AWS Directory Service servers.
rsRadiusPort :: Lens' RadiusSettings (Maybe Natural)
rsRadiusPort = lens _rsRadiusPort (\ s a -> s{_rsRadiusPort = a}) . mapping _Nat;

instance FromJSON RadiusSettings where
        parseJSON
          = withObject "RadiusSettings"
              (\ x ->
                 RadiusSettings' <$>
                   (x .:? "DisplayLabel") <*> (x .:? "RadiusRetries")
                     <*> (x .:? "AuthenticationProtocol")
                     <*> (x .:? "RadiusServers" .!= mempty)
                     <*> (x .:? "UseSameUsername")
                     <*> (x .:? "SharedSecret")
                     <*> (x .:? "RadiusTimeout")
                     <*> (x .:? "RadiusPort"))

instance Hashable RadiusSettings

instance NFData RadiusSettings

instance ToJSON RadiusSettings where
        toJSON RadiusSettings'{..}
          = object
              (catMaybes
                 [("DisplayLabel" .=) <$> _rsDisplayLabel,
                  ("RadiusRetries" .=) <$> _rsRadiusRetries,
                  ("AuthenticationProtocol" .=) <$>
                    _rsAuthenticationProtocol,
                  ("RadiusServers" .=) <$> _rsRadiusServers,
                  ("UseSameUsername" .=) <$> _rsUseSameUsername,
                  ("SharedSecret" .=) <$> _rsSharedSecret,
                  ("RadiusTimeout" .=) <$> _rsRadiusTimeout,
                  ("RadiusPort" .=) <$> _rsRadiusPort])

-- | Describes a directory snapshot.
--
-- /See:/ 'snapshot' smart constructor.
data Snapshot = Snapshot'
    { _sStatus      :: !(Maybe SnapshotStatus)
    , _sDirectoryId :: !(Maybe Text)
    , _sStartTime   :: !(Maybe POSIX)
    , _sName        :: !(Maybe Text)
    , _sType        :: !(Maybe SnapshotType)
    , _sSnapshotId  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Snapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sStatus'
--
-- * 'sDirectoryId'
--
-- * 'sStartTime'
--
-- * 'sName'
--
-- * 'sType'
--
-- * 'sSnapshotId'
snapshot
    :: Snapshot
snapshot =
    Snapshot'
    { _sStatus = Nothing
    , _sDirectoryId = Nothing
    , _sStartTime = Nothing
    , _sName = Nothing
    , _sType = Nothing
    , _sSnapshotId = Nothing
    }

-- | The snapshot status.
sStatus :: Lens' Snapshot (Maybe SnapshotStatus)
sStatus = lens _sStatus (\ s a -> s{_sStatus = a});

-- | The directory identifier.
sDirectoryId :: Lens' Snapshot (Maybe Text)
sDirectoryId = lens _sDirectoryId (\ s a -> s{_sDirectoryId = a});

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
                   (x .:? "Status") <*> (x .:? "DirectoryId") <*>
                     (x .:? "StartTime")
                     <*> (x .:? "Name")
                     <*> (x .:? "Type")
                     <*> (x .:? "SnapshotId"))

instance Hashable Snapshot

instance NFData Snapshot

-- | Contains manual snapshot limit information for a directory.
--
-- /See:/ 'snapshotLimits' smart constructor.
data SnapshotLimits = SnapshotLimits'
    { _slManualSnapshotsLimitReached :: !(Maybe Bool)
    , _slManualSnapshotsCurrentCount :: !(Maybe Nat)
    , _slManualSnapshotsLimit        :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SnapshotLimits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slManualSnapshotsLimitReached'
--
-- * 'slManualSnapshotsCurrentCount'
--
-- * 'slManualSnapshotsLimit'
snapshotLimits
    :: SnapshotLimits
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

instance Hashable SnapshotLimits

instance NFData SnapshotLimits

-- | Describes a trust relationship between an Microsoft AD in the AWS cloud and an external domain.
--
-- /See:/ 'trust' smart constructor.
data Trust = Trust'
    { _tDirectoryId              :: !(Maybe Text)
    , _tTrustState               :: !(Maybe TrustState)
    , _tLastUpdatedDateTime      :: !(Maybe POSIX)
    , _tTrustDirection           :: !(Maybe TrustDirection)
    , _tStateLastUpdatedDateTime :: !(Maybe POSIX)
    , _tTrustType                :: !(Maybe TrustType)
    , _tTrustStateReason         :: !(Maybe Text)
    , _tRemoteDomainName         :: !(Maybe Text)
    , _tTrustId                  :: !(Maybe Text)
    , _tCreatedDateTime          :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Trust' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tDirectoryId'
--
-- * 'tTrustState'
--
-- * 'tLastUpdatedDateTime'
--
-- * 'tTrustDirection'
--
-- * 'tStateLastUpdatedDateTime'
--
-- * 'tTrustType'
--
-- * 'tTrustStateReason'
--
-- * 'tRemoteDomainName'
--
-- * 'tTrustId'
--
-- * 'tCreatedDateTime'
trust
    :: Trust
trust =
    Trust'
    { _tDirectoryId = Nothing
    , _tTrustState = Nothing
    , _tLastUpdatedDateTime = Nothing
    , _tTrustDirection = Nothing
    , _tStateLastUpdatedDateTime = Nothing
    , _tTrustType = Nothing
    , _tTrustStateReason = Nothing
    , _tRemoteDomainName = Nothing
    , _tTrustId = Nothing
    , _tCreatedDateTime = Nothing
    }

-- | The Directory ID of the AWS directory involved in the trust relationship.
tDirectoryId :: Lens' Trust (Maybe Text)
tDirectoryId = lens _tDirectoryId (\ s a -> s{_tDirectoryId = a});

-- | The trust relationship state.
tTrustState :: Lens' Trust (Maybe TrustState)
tTrustState = lens _tTrustState (\ s a -> s{_tTrustState = a});

-- | The date and time that the trust relationship was last updated.
tLastUpdatedDateTime :: Lens' Trust (Maybe UTCTime)
tLastUpdatedDateTime = lens _tLastUpdatedDateTime (\ s a -> s{_tLastUpdatedDateTime = a}) . mapping _Time;

-- | The trust relationship direction.
tTrustDirection :: Lens' Trust (Maybe TrustDirection)
tTrustDirection = lens _tTrustDirection (\ s a -> s{_tTrustDirection = a});

-- | The date and time that the TrustState was last updated.
tStateLastUpdatedDateTime :: Lens' Trust (Maybe UTCTime)
tStateLastUpdatedDateTime = lens _tStateLastUpdatedDateTime (\ s a -> s{_tStateLastUpdatedDateTime = a}) . mapping _Time;

-- | The trust relationship type.
tTrustType :: Lens' Trust (Maybe TrustType)
tTrustType = lens _tTrustType (\ s a -> s{_tTrustType = a});

-- | The reason for the TrustState.
tTrustStateReason :: Lens' Trust (Maybe Text)
tTrustStateReason = lens _tTrustStateReason (\ s a -> s{_tTrustStateReason = a});

-- | The Fully Qualified Domain Name (FQDN) of the external domain involved in the trust relationship.
tRemoteDomainName :: Lens' Trust (Maybe Text)
tRemoteDomainName = lens _tRemoteDomainName (\ s a -> s{_tRemoteDomainName = a});

-- | The unique ID of the trust relationship.
tTrustId :: Lens' Trust (Maybe Text)
tTrustId = lens _tTrustId (\ s a -> s{_tTrustId = a});

-- | The date and time that the trust relationship was created.
tCreatedDateTime :: Lens' Trust (Maybe UTCTime)
tCreatedDateTime = lens _tCreatedDateTime (\ s a -> s{_tCreatedDateTime = a}) . mapping _Time;

instance FromJSON Trust where
        parseJSON
          = withObject "Trust"
              (\ x ->
                 Trust' <$>
                   (x .:? "DirectoryId") <*> (x .:? "TrustState") <*>
                     (x .:? "LastUpdatedDateTime")
                     <*> (x .:? "TrustDirection")
                     <*> (x .:? "StateLastUpdatedDateTime")
                     <*> (x .:? "TrustType")
                     <*> (x .:? "TrustStateReason")
                     <*> (x .:? "RemoteDomainName")
                     <*> (x .:? "TrustId")
                     <*> (x .:? "CreatedDateTime"))

instance Hashable Trust

instance NFData Trust
