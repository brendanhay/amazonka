{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.Product where

import Network.AWS.DirectoryService.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a named directory attribute.
--
--
--
-- /See:/ 'attribute' smart constructor.
data Attribute = Attribute'
  { _aValue :: !(Maybe Text)
  , _aName  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Attribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aValue' - The value of the attribute.
--
-- * 'aName' - The name of the attribute.
attribute
    :: Attribute
attribute = Attribute' {_aValue = Nothing, _aName = Nothing}


-- | The value of the attribute.
aValue :: Lens' Attribute (Maybe Text)
aValue = lens _aValue (\ s a -> s{_aValue = a})

-- | The name of the attribute.
aName :: Lens' Attribute (Maybe Text)
aName = lens _aName (\ s a -> s{_aName = a})

instance FromJSON Attribute where
        parseJSON
          = withObject "Attribute"
              (\ x ->
                 Attribute' <$> (x .:? "Value") <*> (x .:? "Name"))

instance Hashable Attribute where

instance NFData Attribute where

instance ToJSON Attribute where
        toJSON Attribute'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _aValue, ("Name" .=) <$> _aName])

-- | Contains information about a computer account in a directory.
--
--
--
-- /See:/ 'computer' smart constructor.
data Computer = Computer'
  { _cComputerId         :: !(Maybe Text)
  , _cComputerAttributes :: !(Maybe [Attribute])
  , _cComputerName       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Computer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cComputerId' - The identifier of the computer.
--
-- * 'cComputerAttributes' - An array of 'Attribute' objects containing the LDAP attributes that belong to the computer account.
--
-- * 'cComputerName' - The computer name.
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
cComputerId = lens _cComputerId (\ s a -> s{_cComputerId = a})

-- | An array of 'Attribute' objects containing the LDAP attributes that belong to the computer account.
cComputerAttributes :: Lens' Computer [Attribute]
cComputerAttributes = lens _cComputerAttributes (\ s a -> s{_cComputerAttributes = a}) . _Default . _Coerce

-- | The computer name.
cComputerName :: Lens' Computer (Maybe Text)
cComputerName = lens _cComputerName (\ s a -> s{_cComputerName = a})

instance FromJSON Computer where
        parseJSON
          = withObject "Computer"
              (\ x ->
                 Computer' <$>
                   (x .:? "ComputerId") <*>
                     (x .:? "ComputerAttributes" .!= mempty)
                     <*> (x .:? "ComputerName"))

instance Hashable Computer where

instance NFData Computer where

-- | Points to a remote domain with which you are setting up a trust relationship. Conditional forwarders are required in order to set up a trust relationship with another domain.
--
--
--
-- /See:/ 'conditionalForwarder' smart constructor.
data ConditionalForwarder = ConditionalForwarder'
  { _cfDNSIPAddrs       :: !(Maybe [Text])
  , _cfRemoteDomainName :: !(Maybe Text)
  , _cfReplicationScope :: !(Maybe ReplicationScope)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConditionalForwarder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfDNSIPAddrs' - The IP addresses of the remote DNS server associated with RemoteDomainName. This is the IP address of the DNS server that your conditional forwarder points to.
--
-- * 'cfRemoteDomainName' - The fully qualified domain name (FQDN) of the remote domains pointed to by the conditional forwarder.
--
-- * 'cfReplicationScope' - The replication scope of the conditional forwarder. The only allowed value is @Domain@ , which will replicate the conditional forwarder to all of the domain controllers for your AWS directory.
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
cfDNSIPAddrs = lens _cfDNSIPAddrs (\ s a -> s{_cfDNSIPAddrs = a}) . _Default . _Coerce

-- | The fully qualified domain name (FQDN) of the remote domains pointed to by the conditional forwarder.
cfRemoteDomainName :: Lens' ConditionalForwarder (Maybe Text)
cfRemoteDomainName = lens _cfRemoteDomainName (\ s a -> s{_cfRemoteDomainName = a})

-- | The replication scope of the conditional forwarder. The only allowed value is @Domain@ , which will replicate the conditional forwarder to all of the domain controllers for your AWS directory.
cfReplicationScope :: Lens' ConditionalForwarder (Maybe ReplicationScope)
cfReplicationScope = lens _cfReplicationScope (\ s a -> s{_cfReplicationScope = a})

instance FromJSON ConditionalForwarder where
        parseJSON
          = withObject "ConditionalForwarder"
              (\ x ->
                 ConditionalForwarder' <$>
                   (x .:? "DnsIpAddrs" .!= mempty) <*>
                     (x .:? "RemoteDomainName")
                     <*> (x .:? "ReplicationScope"))

instance Hashable ConditionalForwarder where

instance NFData ConditionalForwarder where

-- | Contains information for the 'ConnectDirectory' operation when an AD Connector directory is being created.
--
--
--
-- /See:/ 'directoryConnectSettings' smart constructor.
data DirectoryConnectSettings = DirectoryConnectSettings'
  { _dcsVPCId            :: !Text
  , _dcsSubnetIds        :: ![Text]
  , _dcsCustomerDNSIPs   :: ![Text]
  , _dcsCustomerUserName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DirectoryConnectSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsVPCId' - The identifier of the VPC in which the AD Connector is created.
--
-- * 'dcsSubnetIds' - A list of subnet identifiers in the VPC in which the AD Connector is created.
--
-- * 'dcsCustomerDNSIPs' - A list of one or more IP addresses of DNS servers or domain controllers in the on-premises directory.
--
-- * 'dcsCustomerUserName' - The user name of an account in the on-premises directory that is used to connect to the directory. This account must have the following permissions:     * Read users and groups     * Create computer objects     * Join computers to the domain
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
dcsVPCId = lens _dcsVPCId (\ s a -> s{_dcsVPCId = a})

-- | A list of subnet identifiers in the VPC in which the AD Connector is created.
dcsSubnetIds :: Lens' DirectoryConnectSettings [Text]
dcsSubnetIds = lens _dcsSubnetIds (\ s a -> s{_dcsSubnetIds = a}) . _Coerce

-- | A list of one or more IP addresses of DNS servers or domain controllers in the on-premises directory.
dcsCustomerDNSIPs :: Lens' DirectoryConnectSettings [Text]
dcsCustomerDNSIPs = lens _dcsCustomerDNSIPs (\ s a -> s{_dcsCustomerDNSIPs = a}) . _Coerce

-- | The user name of an account in the on-premises directory that is used to connect to the directory. This account must have the following permissions:     * Read users and groups     * Create computer objects     * Join computers to the domain
dcsCustomerUserName :: Lens' DirectoryConnectSettings Text
dcsCustomerUserName = lens _dcsCustomerUserName (\ s a -> s{_dcsCustomerUserName = a})

instance Hashable DirectoryConnectSettings where

instance NFData DirectoryConnectSettings where

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
--
--
-- /See:/ 'directoryConnectSettingsDescription' smart constructor.
data DirectoryConnectSettingsDescription = DirectoryConnectSettingsDescription'
  { _dcsdCustomerUserName  :: !(Maybe Text)
  , _dcsdSubnetIds         :: !(Maybe [Text])
  , _dcsdVPCId             :: !(Maybe Text)
  , _dcsdSecurityGroupId   :: !(Maybe Text)
  , _dcsdConnectIPs        :: !(Maybe [Text])
  , _dcsdAvailabilityZones :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DirectoryConnectSettingsDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsdCustomerUserName' - The user name of the service account in the on-premises directory.
--
-- * 'dcsdSubnetIds' - A list of subnet identifiers in the VPC that the AD connector is in.
--
-- * 'dcsdVPCId' - The identifier of the VPC that the AD Connector is in.
--
-- * 'dcsdSecurityGroupId' - The security group identifier for the AD Connector directory.
--
-- * 'dcsdConnectIPs' - The IP addresses of the AD Connector servers.
--
-- * 'dcsdAvailabilityZones' - A list of the Availability Zones that the directory is in.
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


-- | The user name of the service account in the on-premises directory.
dcsdCustomerUserName :: Lens' DirectoryConnectSettingsDescription (Maybe Text)
dcsdCustomerUserName = lens _dcsdCustomerUserName (\ s a -> s{_dcsdCustomerUserName = a})

-- | A list of subnet identifiers in the VPC that the AD connector is in.
dcsdSubnetIds :: Lens' DirectoryConnectSettingsDescription [Text]
dcsdSubnetIds = lens _dcsdSubnetIds (\ s a -> s{_dcsdSubnetIds = a}) . _Default . _Coerce

-- | The identifier of the VPC that the AD Connector is in.
dcsdVPCId :: Lens' DirectoryConnectSettingsDescription (Maybe Text)
dcsdVPCId = lens _dcsdVPCId (\ s a -> s{_dcsdVPCId = a})

-- | The security group identifier for the AD Connector directory.
dcsdSecurityGroupId :: Lens' DirectoryConnectSettingsDescription (Maybe Text)
dcsdSecurityGroupId = lens _dcsdSecurityGroupId (\ s a -> s{_dcsdSecurityGroupId = a})

-- | The IP addresses of the AD Connector servers.
dcsdConnectIPs :: Lens' DirectoryConnectSettingsDescription [Text]
dcsdConnectIPs = lens _dcsdConnectIPs (\ s a -> s{_dcsdConnectIPs = a}) . _Default . _Coerce

-- | A list of the Availability Zones that the directory is in.
dcsdAvailabilityZones :: Lens' DirectoryConnectSettingsDescription [Text]
dcsdAvailabilityZones = lens _dcsdAvailabilityZones (\ s a -> s{_dcsdAvailabilityZones = a}) . _Default . _Coerce

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
         where

instance NFData DirectoryConnectSettingsDescription
         where

-- | Contains information about an AWS Directory Service directory.
--
--
--
-- /See:/ 'directoryDescription' smart constructor.
data DirectoryDescription = DirectoryDescription'
  { _ddEdition :: !(Maybe DirectoryEdition)
  , _ddRadiusStatus :: !(Maybe RadiusStatus)
  , _ddStage :: !(Maybe DirectoryStage)
  , _ddDirectoryId :: !(Maybe Text)
  , _ddAccessURL :: !(Maybe Text)
  , _ddShortName :: !(Maybe Text)
  , _ddSize :: !(Maybe DirectorySize)
  , _ddDesiredNumberOfDomainControllers :: !(Maybe Nat)
  , _ddRadiusSettings :: !(Maybe RadiusSettings)
  , _ddLaunchTime :: !(Maybe POSIX)
  , _ddAlias :: !(Maybe Text)
  , _ddShareStatus :: !(Maybe ShareStatus)
  , _ddName :: !(Maybe Text)
  , _ddShareMethod :: !(Maybe ShareMethod)
  , _ddStageLastUpdatedDateTime :: !(Maybe POSIX)
  , _ddSSOEnabled :: !(Maybe Bool)
  , _ddDNSIPAddrs :: !(Maybe [Text])
  , _ddVPCSettings :: !(Maybe DirectoryVPCSettingsDescription)
  , _ddType :: !(Maybe DirectoryType)
  , _ddStageReason :: !(Maybe Text)
  , _ddConnectSettings :: !(Maybe DirectoryConnectSettingsDescription)
  , _ddOwnerDirectoryDescription :: !(Maybe OwnerDirectoryDescription)
  , _ddDescription :: !(Maybe Text)
  , _ddShareNotes :: !(Maybe (Sensitive Text))
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DirectoryDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddEdition' - The edition associated with this directory.
--
-- * 'ddRadiusStatus' - The status of the RADIUS MFA server connection.
--
-- * 'ddStage' - The current stage of the directory.
--
-- * 'ddDirectoryId' - The directory identifier.
--
-- * 'ddAccessURL' - The access URL for the directory, such as @http://<alias>.awsapps.com@ . If no alias has been created for the directory, @<alias>@ is the directory identifier, such as @d-XXXXXXXXXX@ .
--
-- * 'ddShortName' - The short name of the directory.
--
-- * 'ddSize' - The directory size.
--
-- * 'ddDesiredNumberOfDomainControllers' - The desired number of domain controllers in the directory if the directory is Microsoft AD.
--
-- * 'ddRadiusSettings' - A 'RadiusSettings' object that contains information about the RADIUS server configured for this directory.
--
-- * 'ddLaunchTime' - Specifies when the directory was created.
--
-- * 'ddAlias' - The alias for the directory. If no alias has been created for the directory, the alias is the directory identifier, such as @d-XXXXXXXXXX@ .
--
-- * 'ddShareStatus' - Current directory status of the shared AWS Managed Microsoft AD directory.
--
-- * 'ddName' - The fully qualified name of the directory.
--
-- * 'ddShareMethod' - The method used when sharing a directory to determine whether the directory should be shared within your AWS organization (@ORGANIZATIONS@ ) or with any AWS account by sending a shared directory request (@HANDSHAKE@ ).
--
-- * 'ddStageLastUpdatedDateTime' - The date and time that the stage was last updated.
--
-- * 'ddSSOEnabled' - Indicates if single sign-on is enabled for the directory. For more information, see 'EnableSso' and 'DisableSso' .
--
-- * 'ddDNSIPAddrs' - The IP addresses of the DNS servers for the directory. For a Simple AD or Microsoft AD directory, these are the IP addresses of the Simple AD or Microsoft AD directory servers. For an AD Connector directory, these are the IP addresses of the DNS servers or domain controllers in the on-premises directory to which the AD Connector is connected.
--
-- * 'ddVPCSettings' - A 'DirectoryVpcSettingsDescription' object that contains additional information about a directory. This member is only present if the directory is a Simple AD or Managed AD directory.
--
-- * 'ddType' - The directory size.
--
-- * 'ddStageReason' - Additional information about the directory stage.
--
-- * 'ddConnectSettings' - A 'DirectoryConnectSettingsDescription' object that contains additional information about an AD Connector directory. This member is only present if the directory is an AD Connector directory.
--
-- * 'ddOwnerDirectoryDescription' - Describes the AWS Managed Microsoft AD directory in the directory owner account.
--
-- * 'ddDescription' - The textual description for the directory.
--
-- * 'ddShareNotes' - A directory share request that is sent by the directory owner to the directory consumer. The request includes a typed message to help the directory consumer administrator determine whether to approve or reject the share invitation.
directoryDescription
    :: DirectoryDescription
directoryDescription =
  DirectoryDescription'
    { _ddEdition = Nothing
    , _ddRadiusStatus = Nothing
    , _ddStage = Nothing
    , _ddDirectoryId = Nothing
    , _ddAccessURL = Nothing
    , _ddShortName = Nothing
    , _ddSize = Nothing
    , _ddDesiredNumberOfDomainControllers = Nothing
    , _ddRadiusSettings = Nothing
    , _ddLaunchTime = Nothing
    , _ddAlias = Nothing
    , _ddShareStatus = Nothing
    , _ddName = Nothing
    , _ddShareMethod = Nothing
    , _ddStageLastUpdatedDateTime = Nothing
    , _ddSSOEnabled = Nothing
    , _ddDNSIPAddrs = Nothing
    , _ddVPCSettings = Nothing
    , _ddType = Nothing
    , _ddStageReason = Nothing
    , _ddConnectSettings = Nothing
    , _ddOwnerDirectoryDescription = Nothing
    , _ddDescription = Nothing
    , _ddShareNotes = Nothing
    }


-- | The edition associated with this directory.
ddEdition :: Lens' DirectoryDescription (Maybe DirectoryEdition)
ddEdition = lens _ddEdition (\ s a -> s{_ddEdition = a})

-- | The status of the RADIUS MFA server connection.
ddRadiusStatus :: Lens' DirectoryDescription (Maybe RadiusStatus)
ddRadiusStatus = lens _ddRadiusStatus (\ s a -> s{_ddRadiusStatus = a})

-- | The current stage of the directory.
ddStage :: Lens' DirectoryDescription (Maybe DirectoryStage)
ddStage = lens _ddStage (\ s a -> s{_ddStage = a})

-- | The directory identifier.
ddDirectoryId :: Lens' DirectoryDescription (Maybe Text)
ddDirectoryId = lens _ddDirectoryId (\ s a -> s{_ddDirectoryId = a})

-- | The access URL for the directory, such as @http://<alias>.awsapps.com@ . If no alias has been created for the directory, @<alias>@ is the directory identifier, such as @d-XXXXXXXXXX@ .
ddAccessURL :: Lens' DirectoryDescription (Maybe Text)
ddAccessURL = lens _ddAccessURL (\ s a -> s{_ddAccessURL = a})

-- | The short name of the directory.
ddShortName :: Lens' DirectoryDescription (Maybe Text)
ddShortName = lens _ddShortName (\ s a -> s{_ddShortName = a})

-- | The directory size.
ddSize :: Lens' DirectoryDescription (Maybe DirectorySize)
ddSize = lens _ddSize (\ s a -> s{_ddSize = a})

-- | The desired number of domain controllers in the directory if the directory is Microsoft AD.
ddDesiredNumberOfDomainControllers :: Lens' DirectoryDescription (Maybe Natural)
ddDesiredNumberOfDomainControllers = lens _ddDesiredNumberOfDomainControllers (\ s a -> s{_ddDesiredNumberOfDomainControllers = a}) . mapping _Nat

-- | A 'RadiusSettings' object that contains information about the RADIUS server configured for this directory.
ddRadiusSettings :: Lens' DirectoryDescription (Maybe RadiusSettings)
ddRadiusSettings = lens _ddRadiusSettings (\ s a -> s{_ddRadiusSettings = a})

-- | Specifies when the directory was created.
ddLaunchTime :: Lens' DirectoryDescription (Maybe UTCTime)
ddLaunchTime = lens _ddLaunchTime (\ s a -> s{_ddLaunchTime = a}) . mapping _Time

-- | The alias for the directory. If no alias has been created for the directory, the alias is the directory identifier, such as @d-XXXXXXXXXX@ .
ddAlias :: Lens' DirectoryDescription (Maybe Text)
ddAlias = lens _ddAlias (\ s a -> s{_ddAlias = a})

-- | Current directory status of the shared AWS Managed Microsoft AD directory.
ddShareStatus :: Lens' DirectoryDescription (Maybe ShareStatus)
ddShareStatus = lens _ddShareStatus (\ s a -> s{_ddShareStatus = a})

-- | The fully qualified name of the directory.
ddName :: Lens' DirectoryDescription (Maybe Text)
ddName = lens _ddName (\ s a -> s{_ddName = a})

-- | The method used when sharing a directory to determine whether the directory should be shared within your AWS organization (@ORGANIZATIONS@ ) or with any AWS account by sending a shared directory request (@HANDSHAKE@ ).
ddShareMethod :: Lens' DirectoryDescription (Maybe ShareMethod)
ddShareMethod = lens _ddShareMethod (\ s a -> s{_ddShareMethod = a})

-- | The date and time that the stage was last updated.
ddStageLastUpdatedDateTime :: Lens' DirectoryDescription (Maybe UTCTime)
ddStageLastUpdatedDateTime = lens _ddStageLastUpdatedDateTime (\ s a -> s{_ddStageLastUpdatedDateTime = a}) . mapping _Time

-- | Indicates if single sign-on is enabled for the directory. For more information, see 'EnableSso' and 'DisableSso' .
ddSSOEnabled :: Lens' DirectoryDescription (Maybe Bool)
ddSSOEnabled = lens _ddSSOEnabled (\ s a -> s{_ddSSOEnabled = a})

-- | The IP addresses of the DNS servers for the directory. For a Simple AD or Microsoft AD directory, these are the IP addresses of the Simple AD or Microsoft AD directory servers. For an AD Connector directory, these are the IP addresses of the DNS servers or domain controllers in the on-premises directory to which the AD Connector is connected.
ddDNSIPAddrs :: Lens' DirectoryDescription [Text]
ddDNSIPAddrs = lens _ddDNSIPAddrs (\ s a -> s{_ddDNSIPAddrs = a}) . _Default . _Coerce

-- | A 'DirectoryVpcSettingsDescription' object that contains additional information about a directory. This member is only present if the directory is a Simple AD or Managed AD directory.
ddVPCSettings :: Lens' DirectoryDescription (Maybe DirectoryVPCSettingsDescription)
ddVPCSettings = lens _ddVPCSettings (\ s a -> s{_ddVPCSettings = a})

-- | The directory size.
ddType :: Lens' DirectoryDescription (Maybe DirectoryType)
ddType = lens _ddType (\ s a -> s{_ddType = a})

-- | Additional information about the directory stage.
ddStageReason :: Lens' DirectoryDescription (Maybe Text)
ddStageReason = lens _ddStageReason (\ s a -> s{_ddStageReason = a})

-- | A 'DirectoryConnectSettingsDescription' object that contains additional information about an AD Connector directory. This member is only present if the directory is an AD Connector directory.
ddConnectSettings :: Lens' DirectoryDescription (Maybe DirectoryConnectSettingsDescription)
ddConnectSettings = lens _ddConnectSettings (\ s a -> s{_ddConnectSettings = a})

-- | Describes the AWS Managed Microsoft AD directory in the directory owner account.
ddOwnerDirectoryDescription :: Lens' DirectoryDescription (Maybe OwnerDirectoryDescription)
ddOwnerDirectoryDescription = lens _ddOwnerDirectoryDescription (\ s a -> s{_ddOwnerDirectoryDescription = a})

-- | The textual description for the directory.
ddDescription :: Lens' DirectoryDescription (Maybe Text)
ddDescription = lens _ddDescription (\ s a -> s{_ddDescription = a})

-- | A directory share request that is sent by the directory owner to the directory consumer. The request includes a typed message to help the directory consumer administrator determine whether to approve or reject the share invitation.
ddShareNotes :: Lens' DirectoryDescription (Maybe Text)
ddShareNotes = lens _ddShareNotes (\ s a -> s{_ddShareNotes = a}) . mapping _Sensitive

instance FromJSON DirectoryDescription where
        parseJSON
          = withObject "DirectoryDescription"
              (\ x ->
                 DirectoryDescription' <$>
                   (x .:? "Edition") <*> (x .:? "RadiusStatus") <*>
                     (x .:? "Stage")
                     <*> (x .:? "DirectoryId")
                     <*> (x .:? "AccessUrl")
                     <*> (x .:? "ShortName")
                     <*> (x .:? "Size")
                     <*> (x .:? "DesiredNumberOfDomainControllers")
                     <*> (x .:? "RadiusSettings")
                     <*> (x .:? "LaunchTime")
                     <*> (x .:? "Alias")
                     <*> (x .:? "ShareStatus")
                     <*> (x .:? "Name")
                     <*> (x .:? "ShareMethod")
                     <*> (x .:? "StageLastUpdatedDateTime")
                     <*> (x .:? "SsoEnabled")
                     <*> (x .:? "DnsIpAddrs" .!= mempty)
                     <*> (x .:? "VpcSettings")
                     <*> (x .:? "Type")
                     <*> (x .:? "StageReason")
                     <*> (x .:? "ConnectSettings")
                     <*> (x .:? "OwnerDirectoryDescription")
                     <*> (x .:? "Description")
                     <*> (x .:? "ShareNotes"))

instance Hashable DirectoryDescription where

instance NFData DirectoryDescription where

-- | Contains directory limit information for a region.
--
--
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DirectoryLimits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlConnectedDirectoriesCurrentCount' - The current number of connected directories in the region.
--
-- * 'dlCloudOnlyMicrosoftADLimitReached' - Indicates if the AWS Managed Microsoft AD directory limit has been reached.
--
-- * 'dlConnectedDirectoriesLimit' - The maximum number of connected directories allowed in the region.
--
-- * 'dlConnectedDirectoriesLimitReached' - Indicates if the connected directory limit has been reached.
--
-- * 'dlCloudOnlyMicrosoftADLimit' - The maximum number of AWS Managed Microsoft AD directories allowed in the region.
--
-- * 'dlCloudOnlyDirectoriesLimit' - The maximum number of cloud directories allowed in the region.
--
-- * 'dlCloudOnlyDirectoriesCurrentCount' - The current number of cloud directories in the region.
--
-- * 'dlCloudOnlyDirectoriesLimitReached' - Indicates if the cloud directory limit has been reached.
--
-- * 'dlCloudOnlyMicrosoftADCurrentCount' - The current number of AWS Managed Microsoft AD directories in the region.
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
dlConnectedDirectoriesCurrentCount = lens _dlConnectedDirectoriesCurrentCount (\ s a -> s{_dlConnectedDirectoriesCurrentCount = a}) . mapping _Nat

-- | Indicates if the AWS Managed Microsoft AD directory limit has been reached.
dlCloudOnlyMicrosoftADLimitReached :: Lens' DirectoryLimits (Maybe Bool)
dlCloudOnlyMicrosoftADLimitReached = lens _dlCloudOnlyMicrosoftADLimitReached (\ s a -> s{_dlCloudOnlyMicrosoftADLimitReached = a})

-- | The maximum number of connected directories allowed in the region.
dlConnectedDirectoriesLimit :: Lens' DirectoryLimits (Maybe Natural)
dlConnectedDirectoriesLimit = lens _dlConnectedDirectoriesLimit (\ s a -> s{_dlConnectedDirectoriesLimit = a}) . mapping _Nat

-- | Indicates if the connected directory limit has been reached.
dlConnectedDirectoriesLimitReached :: Lens' DirectoryLimits (Maybe Bool)
dlConnectedDirectoriesLimitReached = lens _dlConnectedDirectoriesLimitReached (\ s a -> s{_dlConnectedDirectoriesLimitReached = a})

-- | The maximum number of AWS Managed Microsoft AD directories allowed in the region.
dlCloudOnlyMicrosoftADLimit :: Lens' DirectoryLimits (Maybe Natural)
dlCloudOnlyMicrosoftADLimit = lens _dlCloudOnlyMicrosoftADLimit (\ s a -> s{_dlCloudOnlyMicrosoftADLimit = a}) . mapping _Nat

-- | The maximum number of cloud directories allowed in the region.
dlCloudOnlyDirectoriesLimit :: Lens' DirectoryLimits (Maybe Natural)
dlCloudOnlyDirectoriesLimit = lens _dlCloudOnlyDirectoriesLimit (\ s a -> s{_dlCloudOnlyDirectoriesLimit = a}) . mapping _Nat

-- | The current number of cloud directories in the region.
dlCloudOnlyDirectoriesCurrentCount :: Lens' DirectoryLimits (Maybe Natural)
dlCloudOnlyDirectoriesCurrentCount = lens _dlCloudOnlyDirectoriesCurrentCount (\ s a -> s{_dlCloudOnlyDirectoriesCurrentCount = a}) . mapping _Nat

-- | Indicates if the cloud directory limit has been reached.
dlCloudOnlyDirectoriesLimitReached :: Lens' DirectoryLimits (Maybe Bool)
dlCloudOnlyDirectoriesLimitReached = lens _dlCloudOnlyDirectoriesLimitReached (\ s a -> s{_dlCloudOnlyDirectoriesLimitReached = a})

-- | The current number of AWS Managed Microsoft AD directories in the region.
dlCloudOnlyMicrosoftADCurrentCount :: Lens' DirectoryLimits (Maybe Natural)
dlCloudOnlyMicrosoftADCurrentCount = lens _dlCloudOnlyMicrosoftADCurrentCount (\ s a -> s{_dlCloudOnlyMicrosoftADCurrentCount = a}) . mapping _Nat

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

instance Hashable DirectoryLimits where

instance NFData DirectoryLimits where

-- | Contains VPC information for the 'CreateDirectory' or 'CreateMicrosoftAD' operation.
--
--
--
-- /See:/ 'directoryVPCSettings' smart constructor.
data DirectoryVPCSettings = DirectoryVPCSettings'
  { _dvsVPCId     :: !Text
  , _dvsSubnetIds :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DirectoryVPCSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvsVPCId' - The identifier of the VPC in which to create the directory.
--
-- * 'dvsSubnetIds' - The identifiers of the subnets for the directory servers. The two subnets must be in different Availability Zones. AWS Directory Service creates a directory server and a DNS server in each of these subnets.
directoryVPCSettings
    :: Text -- ^ 'dvsVPCId'
    -> DirectoryVPCSettings
directoryVPCSettings pVPCId_ =
  DirectoryVPCSettings' {_dvsVPCId = pVPCId_, _dvsSubnetIds = mempty}


-- | The identifier of the VPC in which to create the directory.
dvsVPCId :: Lens' DirectoryVPCSettings Text
dvsVPCId = lens _dvsVPCId (\ s a -> s{_dvsVPCId = a})

-- | The identifiers of the subnets for the directory servers. The two subnets must be in different Availability Zones. AWS Directory Service creates a directory server and a DNS server in each of these subnets.
dvsSubnetIds :: Lens' DirectoryVPCSettings [Text]
dvsSubnetIds = lens _dvsSubnetIds (\ s a -> s{_dvsSubnetIds = a}) . _Coerce

instance Hashable DirectoryVPCSettings where

instance NFData DirectoryVPCSettings where

instance ToJSON DirectoryVPCSettings where
        toJSON DirectoryVPCSettings'{..}
          = object
              (catMaybes
                 [Just ("VpcId" .= _dvsVPCId),
                  Just ("SubnetIds" .= _dvsSubnetIds)])

-- | Contains information about the directory.
--
--
--
-- /See:/ 'directoryVPCSettingsDescription' smart constructor.
data DirectoryVPCSettingsDescription = DirectoryVPCSettingsDescription'
  { _dvsdSubnetIds         :: !(Maybe [Text])
  , _dvsdVPCId             :: !(Maybe Text)
  , _dvsdSecurityGroupId   :: !(Maybe Text)
  , _dvsdAvailabilityZones :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DirectoryVPCSettingsDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvsdSubnetIds' - The identifiers of the subnets for the directory servers.
--
-- * 'dvsdVPCId' - The identifier of the VPC that the directory is in.
--
-- * 'dvsdSecurityGroupId' - The domain controller security group identifier for the directory.
--
-- * 'dvsdAvailabilityZones' - The list of Availability Zones that the directory is in.
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
dvsdSubnetIds = lens _dvsdSubnetIds (\ s a -> s{_dvsdSubnetIds = a}) . _Default . _Coerce

-- | The identifier of the VPC that the directory is in.
dvsdVPCId :: Lens' DirectoryVPCSettingsDescription (Maybe Text)
dvsdVPCId = lens _dvsdVPCId (\ s a -> s{_dvsdVPCId = a})

-- | The domain controller security group identifier for the directory.
dvsdSecurityGroupId :: Lens' DirectoryVPCSettingsDescription (Maybe Text)
dvsdSecurityGroupId = lens _dvsdSecurityGroupId (\ s a -> s{_dvsdSecurityGroupId = a})

-- | The list of Availability Zones that the directory is in.
dvsdAvailabilityZones :: Lens' DirectoryVPCSettingsDescription [Text]
dvsdAvailabilityZones = lens _dvsdAvailabilityZones (\ s a -> s{_dvsdAvailabilityZones = a}) . _Default . _Coerce

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
         where

instance NFData DirectoryVPCSettingsDescription where

-- | Contains information about the domain controllers for a specified directory.
--
--
--
-- /See:/ 'domainController' smart constructor.
data DomainController = DomainController'
  { _dcStatus                    :: !(Maybe DomainControllerStatus)
  , _dcDirectoryId               :: !(Maybe Text)
  , _dcVPCId                     :: !(Maybe Text)
  , _dcLaunchTime                :: !(Maybe POSIX)
  , _dcSubnetId                  :: !(Maybe Text)
  , _dcAvailabilityZone          :: !(Maybe Text)
  , _dcStatusLastUpdatedDateTime :: !(Maybe POSIX)
  , _dcStatusReason              :: !(Maybe Text)
  , _dcDNSIPAddr                 :: !(Maybe Text)
  , _dcDomainControllerId        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DomainController' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcStatus' - The status of the domain controller.
--
-- * 'dcDirectoryId' - Identifier of the directory where the domain controller resides.
--
-- * 'dcVPCId' - The identifier of the VPC that contains the domain controller.
--
-- * 'dcLaunchTime' - Specifies when the domain controller was created.
--
-- * 'dcSubnetId' - Identifier of the subnet in the VPC that contains the domain controller.
--
-- * 'dcAvailabilityZone' - The Availability Zone where the domain controller is located.
--
-- * 'dcStatusLastUpdatedDateTime' - The date and time that the status was last updated.
--
-- * 'dcStatusReason' - A description of the domain controller state.
--
-- * 'dcDNSIPAddr' - The IP address of the domain controller.
--
-- * 'dcDomainControllerId' - Identifies a specific domain controller in the directory.
domainController
    :: DomainController
domainController =
  DomainController'
    { _dcStatus = Nothing
    , _dcDirectoryId = Nothing
    , _dcVPCId = Nothing
    , _dcLaunchTime = Nothing
    , _dcSubnetId = Nothing
    , _dcAvailabilityZone = Nothing
    , _dcStatusLastUpdatedDateTime = Nothing
    , _dcStatusReason = Nothing
    , _dcDNSIPAddr = Nothing
    , _dcDomainControllerId = Nothing
    }


-- | The status of the domain controller.
dcStatus :: Lens' DomainController (Maybe DomainControllerStatus)
dcStatus = lens _dcStatus (\ s a -> s{_dcStatus = a})

-- | Identifier of the directory where the domain controller resides.
dcDirectoryId :: Lens' DomainController (Maybe Text)
dcDirectoryId = lens _dcDirectoryId (\ s a -> s{_dcDirectoryId = a})

-- | The identifier of the VPC that contains the domain controller.
dcVPCId :: Lens' DomainController (Maybe Text)
dcVPCId = lens _dcVPCId (\ s a -> s{_dcVPCId = a})

-- | Specifies when the domain controller was created.
dcLaunchTime :: Lens' DomainController (Maybe UTCTime)
dcLaunchTime = lens _dcLaunchTime (\ s a -> s{_dcLaunchTime = a}) . mapping _Time

-- | Identifier of the subnet in the VPC that contains the domain controller.
dcSubnetId :: Lens' DomainController (Maybe Text)
dcSubnetId = lens _dcSubnetId (\ s a -> s{_dcSubnetId = a})

-- | The Availability Zone where the domain controller is located.
dcAvailabilityZone :: Lens' DomainController (Maybe Text)
dcAvailabilityZone = lens _dcAvailabilityZone (\ s a -> s{_dcAvailabilityZone = a})

-- | The date and time that the status was last updated.
dcStatusLastUpdatedDateTime :: Lens' DomainController (Maybe UTCTime)
dcStatusLastUpdatedDateTime = lens _dcStatusLastUpdatedDateTime (\ s a -> s{_dcStatusLastUpdatedDateTime = a}) . mapping _Time

-- | A description of the domain controller state.
dcStatusReason :: Lens' DomainController (Maybe Text)
dcStatusReason = lens _dcStatusReason (\ s a -> s{_dcStatusReason = a})

-- | The IP address of the domain controller.
dcDNSIPAddr :: Lens' DomainController (Maybe Text)
dcDNSIPAddr = lens _dcDNSIPAddr (\ s a -> s{_dcDNSIPAddr = a})

-- | Identifies a specific domain controller in the directory.
dcDomainControllerId :: Lens' DomainController (Maybe Text)
dcDomainControllerId = lens _dcDomainControllerId (\ s a -> s{_dcDomainControllerId = a})

instance FromJSON DomainController where
        parseJSON
          = withObject "DomainController"
              (\ x ->
                 DomainController' <$>
                   (x .:? "Status") <*> (x .:? "DirectoryId") <*>
                     (x .:? "VpcId")
                     <*> (x .:? "LaunchTime")
                     <*> (x .:? "SubnetId")
                     <*> (x .:? "AvailabilityZone")
                     <*> (x .:? "StatusLastUpdatedDateTime")
                     <*> (x .:? "StatusReason")
                     <*> (x .:? "DnsIpAddr")
                     <*> (x .:? "DomainControllerId"))

instance Hashable DomainController where

instance NFData DomainController where

-- | Information about SNS topic and AWS Directory Service directory associations.
--
--
--
-- /See:/ 'eventTopic' smart constructor.
data EventTopic = EventTopic'
  { _etStatus          :: !(Maybe TopicStatus)
  , _etDirectoryId     :: !(Maybe Text)
  , _etTopicName       :: !(Maybe Text)
  , _etTopicARN        :: !(Maybe Text)
  , _etCreatedDateTime :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EventTopic' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etStatus' - The topic registration status.
--
-- * 'etDirectoryId' - The Directory ID of an AWS Directory Service directory that will publish status messages to an SNS topic.
--
-- * 'etTopicName' - The name of an AWS SNS topic the receives status messages from the directory.
--
-- * 'etTopicARN' - The SNS topic ARN (Amazon Resource Name).
--
-- * 'etCreatedDateTime' - The date and time of when you associated your directory with the SNS topic.
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
etStatus = lens _etStatus (\ s a -> s{_etStatus = a})

-- | The Directory ID of an AWS Directory Service directory that will publish status messages to an SNS topic.
etDirectoryId :: Lens' EventTopic (Maybe Text)
etDirectoryId = lens _etDirectoryId (\ s a -> s{_etDirectoryId = a})

-- | The name of an AWS SNS topic the receives status messages from the directory.
etTopicName :: Lens' EventTopic (Maybe Text)
etTopicName = lens _etTopicName (\ s a -> s{_etTopicName = a})

-- | The SNS topic ARN (Amazon Resource Name).
etTopicARN :: Lens' EventTopic (Maybe Text)
etTopicARN = lens _etTopicARN (\ s a -> s{_etTopicARN = a})

-- | The date and time of when you associated your directory with the SNS topic.
etCreatedDateTime :: Lens' EventTopic (Maybe UTCTime)
etCreatedDateTime = lens _etCreatedDateTime (\ s a -> s{_etCreatedDateTime = a}) . mapping _Time

instance FromJSON EventTopic where
        parseJSON
          = withObject "EventTopic"
              (\ x ->
                 EventTopic' <$>
                   (x .:? "Status") <*> (x .:? "DirectoryId") <*>
                     (x .:? "TopicName")
                     <*> (x .:? "TopicArn")
                     <*> (x .:? "CreatedDateTime"))

instance Hashable EventTopic where

instance NFData EventTopic where

-- | IP address block. This is often the address block of the DNS server used for your on-premises domain.
--
--
--
-- /See:/ 'ipRoute' smart constructor.
data IPRoute = IPRoute'
  { _irCidrIP      :: !(Maybe Text)
  , _irDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IPRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'irCidrIP' - IP address block using CIDR format, for example 10.0.0.0/24. This is often the address block of the DNS server used for your on-premises domain. For a single IP address use a CIDR address block with /32. For example 10.0.0.0/32.
--
-- * 'irDescription' - Description of the address block.
ipRoute
    :: IPRoute
ipRoute = IPRoute' {_irCidrIP = Nothing, _irDescription = Nothing}


-- | IP address block using CIDR format, for example 10.0.0.0/24. This is often the address block of the DNS server used for your on-premises domain. For a single IP address use a CIDR address block with /32. For example 10.0.0.0/32.
irCidrIP :: Lens' IPRoute (Maybe Text)
irCidrIP = lens _irCidrIP (\ s a -> s{_irCidrIP = a})

-- | Description of the address block.
irDescription :: Lens' IPRoute (Maybe Text)
irDescription = lens _irDescription (\ s a -> s{_irDescription = a})

instance Hashable IPRoute where

instance NFData IPRoute where

instance ToJSON IPRoute where
        toJSON IPRoute'{..}
          = object
              (catMaybes
                 [("CidrIp" .=) <$> _irCidrIP,
                  ("Description" .=) <$> _irDescription])

-- | Information about one or more IP address blocks.
--
--
--
-- /See:/ 'ipRouteInfo' smart constructor.
data IPRouteInfo = IPRouteInfo'
  { _iriDirectoryId         :: !(Maybe Text)
  , _iriIPRouteStatusReason :: !(Maybe Text)
  , _iriAddedDateTime       :: !(Maybe POSIX)
  , _iriCidrIP              :: !(Maybe Text)
  , _iriIPRouteStatusMsg    :: !(Maybe IPRouteStatusMsg)
  , _iriDescription         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IPRouteInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iriDirectoryId' - Identifier (ID) of the directory associated with the IP addresses.
--
-- * 'iriIPRouteStatusReason' - The reason for the IpRouteStatusMsg.
--
-- * 'iriAddedDateTime' - The date and time the address block was added to the directory.
--
-- * 'iriCidrIP' - IP address block in the 'IpRoute' .
--
-- * 'iriIPRouteStatusMsg' - The status of the IP address block.
--
-- * 'iriDescription' - Description of the 'IpRouteInfo' .
ipRouteInfo
    :: IPRouteInfo
ipRouteInfo =
  IPRouteInfo'
    { _iriDirectoryId = Nothing
    , _iriIPRouteStatusReason = Nothing
    , _iriAddedDateTime = Nothing
    , _iriCidrIP = Nothing
    , _iriIPRouteStatusMsg = Nothing
    , _iriDescription = Nothing
    }


-- | Identifier (ID) of the directory associated with the IP addresses.
iriDirectoryId :: Lens' IPRouteInfo (Maybe Text)
iriDirectoryId = lens _iriDirectoryId (\ s a -> s{_iriDirectoryId = a})

-- | The reason for the IpRouteStatusMsg.
iriIPRouteStatusReason :: Lens' IPRouteInfo (Maybe Text)
iriIPRouteStatusReason = lens _iriIPRouteStatusReason (\ s a -> s{_iriIPRouteStatusReason = a})

-- | The date and time the address block was added to the directory.
iriAddedDateTime :: Lens' IPRouteInfo (Maybe UTCTime)
iriAddedDateTime = lens _iriAddedDateTime (\ s a -> s{_iriAddedDateTime = a}) . mapping _Time

-- | IP address block in the 'IpRoute' .
iriCidrIP :: Lens' IPRouteInfo (Maybe Text)
iriCidrIP = lens _iriCidrIP (\ s a -> s{_iriCidrIP = a})

-- | The status of the IP address block.
iriIPRouteStatusMsg :: Lens' IPRouteInfo (Maybe IPRouteStatusMsg)
iriIPRouteStatusMsg = lens _iriIPRouteStatusMsg (\ s a -> s{_iriIPRouteStatusMsg = a})

-- | Description of the 'IpRouteInfo' .
iriDescription :: Lens' IPRouteInfo (Maybe Text)
iriDescription = lens _iriDescription (\ s a -> s{_iriDescription = a})

instance FromJSON IPRouteInfo where
        parseJSON
          = withObject "IPRouteInfo"
              (\ x ->
                 IPRouteInfo' <$>
                   (x .:? "DirectoryId") <*>
                     (x .:? "IpRouteStatusReason")
                     <*> (x .:? "AddedDateTime")
                     <*> (x .:? "CidrIp")
                     <*> (x .:? "IpRouteStatusMsg")
                     <*> (x .:? "Description"))

instance Hashable IPRouteInfo where

instance NFData IPRouteInfo where

-- | Represents a log subscription, which tracks real-time data from a chosen log group to a specified destination.
--
--
--
-- /See:/ 'logSubscription' smart constructor.
data LogSubscription = LogSubscription'
  { _lsDirectoryId                 :: !(Maybe Text)
  , _lsLogGroupName                :: !(Maybe Text)
  , _lsSubscriptionCreatedDateTime :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LogSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsDirectoryId' - Identifier (ID) of the directory that you want to associate with the log subscription.
--
-- * 'lsLogGroupName' - The name of the log group.
--
-- * 'lsSubscriptionCreatedDateTime' - The date and time that the log subscription was created.
logSubscription
    :: LogSubscription
logSubscription =
  LogSubscription'
    { _lsDirectoryId = Nothing
    , _lsLogGroupName = Nothing
    , _lsSubscriptionCreatedDateTime = Nothing
    }


-- | Identifier (ID) of the directory that you want to associate with the log subscription.
lsDirectoryId :: Lens' LogSubscription (Maybe Text)
lsDirectoryId = lens _lsDirectoryId (\ s a -> s{_lsDirectoryId = a})

-- | The name of the log group.
lsLogGroupName :: Lens' LogSubscription (Maybe Text)
lsLogGroupName = lens _lsLogGroupName (\ s a -> s{_lsLogGroupName = a})

-- | The date and time that the log subscription was created.
lsSubscriptionCreatedDateTime :: Lens' LogSubscription (Maybe UTCTime)
lsSubscriptionCreatedDateTime = lens _lsSubscriptionCreatedDateTime (\ s a -> s{_lsSubscriptionCreatedDateTime = a}) . mapping _Time

instance FromJSON LogSubscription where
        parseJSON
          = withObject "LogSubscription"
              (\ x ->
                 LogSubscription' <$>
                   (x .:? "DirectoryId") <*> (x .:? "LogGroupName") <*>
                     (x .:? "SubscriptionCreatedDateTime"))

instance Hashable LogSubscription where

instance NFData LogSubscription where

-- | Describes the directory owner account details that have been shared to the directory consumer account.
--
--
--
-- /See:/ 'ownerDirectoryDescription' smart constructor.
data OwnerDirectoryDescription = OwnerDirectoryDescription'
  { _oddRadiusStatus   :: !(Maybe RadiusStatus)
  , _oddDirectoryId    :: !(Maybe Text)
  , _oddRadiusSettings :: !(Maybe RadiusSettings)
  , _oddAccountId      :: !(Maybe Text)
  , _oddDNSIPAddrs     :: !(Maybe [Text])
  , _oddVPCSettings    :: !(Maybe DirectoryVPCSettingsDescription)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'OwnerDirectoryDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oddRadiusStatus' - Information about the status of the RADIUS server.
--
-- * 'oddDirectoryId' - Identifier of the AWS Managed Microsoft AD directory in the directory owner account.
--
-- * 'oddRadiusSettings' - A 'RadiusSettings' object that contains information about the RADIUS server.
--
-- * 'oddAccountId' - Identifier of the directory owner account.
--
-- * 'oddDNSIPAddrs' - IP address of the directory
