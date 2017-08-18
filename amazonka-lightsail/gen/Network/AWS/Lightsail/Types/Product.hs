{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.Product where

import           Network.AWS.Lens
import           Network.AWS.Lightsail.Types.Sum
import           Network.AWS.Prelude

-- | Describes an Availability Zone.
--
--
--
-- /See:/ 'availabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'
    { _azState    :: !(Maybe Text)
    , _azZoneName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AvailabilityZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'azState' - The state of the Availability Zone.
--
-- * 'azZoneName' - The name of the Availability Zone. The format is @us-east-1a@ (case-sensitive).
availabilityZone
    :: AvailabilityZone
availabilityZone =
    AvailabilityZone'
    { _azState = Nothing
    , _azZoneName = Nothing
    }

-- | The state of the Availability Zone.
azState :: Lens' AvailabilityZone (Maybe Text)
azState = lens _azState (\ s a -> s{_azState = a});

-- | The name of the Availability Zone. The format is @us-east-1a@ (case-sensitive).
azZoneName :: Lens' AvailabilityZone (Maybe Text)
azZoneName = lens _azZoneName (\ s a -> s{_azZoneName = a});

instance FromJSON AvailabilityZone where
        parseJSON
          = withObject "AvailabilityZone"
              (\ x ->
                 AvailabilityZone' <$>
                   (x .:? "state") <*> (x .:? "zoneName"))

instance Hashable AvailabilityZone

instance NFData AvailabilityZone

-- | Describes a blueprint (a virtual private server image).
--
--
--
-- /See:/ 'blueprint' smart constructor.
data Blueprint = Blueprint'
    { _bVersionCode :: !(Maybe Text)
    , _bGroup       :: !(Maybe Text)
    , _bMinPower    :: !(Maybe Int)
    , _bProductURL  :: !(Maybe Text)
    , _bLicenseURL  :: !(Maybe Text)
    , _bName        :: !(Maybe Text)
    , _bVersion     :: !(Maybe Text)
    , _bBlueprintId :: !(Maybe Text)
    , _bType        :: !(Maybe BlueprintType)
    , _bIsActive    :: !(Maybe Bool)
    , _bDescription :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Blueprint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bVersionCode' - The version code.
--
-- * 'bGroup' - The group name of the blueprint (e.g., @amazon-linux@ ).
--
-- * 'bMinPower' - The minimum machine size required to run this blueprint. @0@ indicates that the blueprint runs on all instances.
--
-- * 'bProductURL' - The product URL to learn more about the image or blueprint.
--
-- * 'bLicenseURL' - The end-user license agreement URL for the image or blueprint.
--
-- * 'bName' - The friendly name of the blueprint (e.g., @Amazon Linux@ ).
--
-- * 'bVersion' - The version number of the operating system, application, or stack (e.g., @2016.03.0@ ).
--
-- * 'bBlueprintId' - The ID for the virtual private server image (e.g., @app_wordpress_4_4@ or @app_lamp_7_0@ ).
--
-- * 'bType' - The type of the blueprint (e.g., @os@ or @app@ ).
--
-- * 'bIsActive' - A Boolean value indicating whether the blueprint is active. When you update your blueprints, you will inactivate old blueprints and keep the most recent versions active.
--
-- * 'bDescription' - The description of the blueprint.
blueprint
    :: Blueprint
blueprint =
    Blueprint'
    { _bVersionCode = Nothing
    , _bGroup = Nothing
    , _bMinPower = Nothing
    , _bProductURL = Nothing
    , _bLicenseURL = Nothing
    , _bName = Nothing
    , _bVersion = Nothing
    , _bBlueprintId = Nothing
    , _bType = Nothing
    , _bIsActive = Nothing
    , _bDescription = Nothing
    }

-- | The version code.
bVersionCode :: Lens' Blueprint (Maybe Text)
bVersionCode = lens _bVersionCode (\ s a -> s{_bVersionCode = a});

-- | The group name of the blueprint (e.g., @amazon-linux@ ).
bGroup :: Lens' Blueprint (Maybe Text)
bGroup = lens _bGroup (\ s a -> s{_bGroup = a});

-- | The minimum machine size required to run this blueprint. @0@ indicates that the blueprint runs on all instances.
bMinPower :: Lens' Blueprint (Maybe Int)
bMinPower = lens _bMinPower (\ s a -> s{_bMinPower = a});

-- | The product URL to learn more about the image or blueprint.
bProductURL :: Lens' Blueprint (Maybe Text)
bProductURL = lens _bProductURL (\ s a -> s{_bProductURL = a});

-- | The end-user license agreement URL for the image or blueprint.
bLicenseURL :: Lens' Blueprint (Maybe Text)
bLicenseURL = lens _bLicenseURL (\ s a -> s{_bLicenseURL = a});

-- | The friendly name of the blueprint (e.g., @Amazon Linux@ ).
bName :: Lens' Blueprint (Maybe Text)
bName = lens _bName (\ s a -> s{_bName = a});

-- | The version number of the operating system, application, or stack (e.g., @2016.03.0@ ).
bVersion :: Lens' Blueprint (Maybe Text)
bVersion = lens _bVersion (\ s a -> s{_bVersion = a});

-- | The ID for the virtual private server image (e.g., @app_wordpress_4_4@ or @app_lamp_7_0@ ).
bBlueprintId :: Lens' Blueprint (Maybe Text)
bBlueprintId = lens _bBlueprintId (\ s a -> s{_bBlueprintId = a});

-- | The type of the blueprint (e.g., @os@ or @app@ ).
bType :: Lens' Blueprint (Maybe BlueprintType)
bType = lens _bType (\ s a -> s{_bType = a});

-- | A Boolean value indicating whether the blueprint is active. When you update your blueprints, you will inactivate old blueprints and keep the most recent versions active.
bIsActive :: Lens' Blueprint (Maybe Bool)
bIsActive = lens _bIsActive (\ s a -> s{_bIsActive = a});

-- | The description of the blueprint.
bDescription :: Lens' Blueprint (Maybe Text)
bDescription = lens _bDescription (\ s a -> s{_bDescription = a});

instance FromJSON Blueprint where
        parseJSON
          = withObject "Blueprint"
              (\ x ->
                 Blueprint' <$>
                   (x .:? "versionCode") <*> (x .:? "group") <*>
                     (x .:? "minPower")
                     <*> (x .:? "productUrl")
                     <*> (x .:? "licenseUrl")
                     <*> (x .:? "name")
                     <*> (x .:? "version")
                     <*> (x .:? "blueprintId")
                     <*> (x .:? "type")
                     <*> (x .:? "isActive")
                     <*> (x .:? "description"))

instance Hashable Blueprint

instance NFData Blueprint

-- | Describes a bundle, which is a set of specs describing your virtual private server (or /instance/ ).
--
--
--
-- /See:/ 'bundle' smart constructor.
data Bundle = Bundle'
    { _bunCpuCount             :: !(Maybe Int)
    , _bunTransferPerMonthInGb :: !(Maybe Int)
    , _bunBundleId             :: !(Maybe Text)
    , _bunInstanceType         :: !(Maybe Text)
    , _bunName                 :: !(Maybe Text)
    , _bunPower                :: !(Maybe Int)
    , _bunDiskSizeInGb         :: !(Maybe Int)
    , _bunPrice                :: !(Maybe Double)
    , _bunIsActive             :: !(Maybe Bool)
    , _bunRamSizeInGb          :: !(Maybe Double)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Bundle' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bunCpuCount' - The number of vCPUs included in the bundle (e.g., @2@ ).
--
-- * 'bunTransferPerMonthInGb' - The data transfer rate per month in GB (e.g., @2000@ ).
--
-- * 'bunBundleId' - The bundle ID (e.g., @micro_1_0@ ).
--
-- * 'bunInstanceType' - The Amazon EC2 instance type (e.g., @t2.micro@ ).
--
-- * 'bunName' - A friendly name for the bundle (e.g., @Micro@ ).
--
-- * 'bunPower' - The power of the bundle (e.g., @500@ ).
--
-- * 'bunDiskSizeInGb' - The size of the SSD (e.g., @30@ ).
--
-- * 'bunPrice' - The price in US dollars (e.g., @5.0@ ).
--
-- * 'bunIsActive' - A Boolean value indicating whether the bundle is active.
--
-- * 'bunRamSizeInGb' - The amount of RAM in GB (e.g., @2.0@ ).
bundle
    :: Bundle
bundle =
    Bundle'
    { _bunCpuCount = Nothing
    , _bunTransferPerMonthInGb = Nothing
    , _bunBundleId = Nothing
    , _bunInstanceType = Nothing
    , _bunName = Nothing
    , _bunPower = Nothing
    , _bunDiskSizeInGb = Nothing
    , _bunPrice = Nothing
    , _bunIsActive = Nothing
    , _bunRamSizeInGb = Nothing
    }

-- | The number of vCPUs included in the bundle (e.g., @2@ ).
bunCpuCount :: Lens' Bundle (Maybe Int)
bunCpuCount = lens _bunCpuCount (\ s a -> s{_bunCpuCount = a});

-- | The data transfer rate per month in GB (e.g., @2000@ ).
bunTransferPerMonthInGb :: Lens' Bundle (Maybe Int)
bunTransferPerMonthInGb = lens _bunTransferPerMonthInGb (\ s a -> s{_bunTransferPerMonthInGb = a});

-- | The bundle ID (e.g., @micro_1_0@ ).
bunBundleId :: Lens' Bundle (Maybe Text)
bunBundleId = lens _bunBundleId (\ s a -> s{_bunBundleId = a});

-- | The Amazon EC2 instance type (e.g., @t2.micro@ ).
bunInstanceType :: Lens' Bundle (Maybe Text)
bunInstanceType = lens _bunInstanceType (\ s a -> s{_bunInstanceType = a});

-- | A friendly name for the bundle (e.g., @Micro@ ).
bunName :: Lens' Bundle (Maybe Text)
bunName = lens _bunName (\ s a -> s{_bunName = a});

-- | The power of the bundle (e.g., @500@ ).
bunPower :: Lens' Bundle (Maybe Int)
bunPower = lens _bunPower (\ s a -> s{_bunPower = a});

-- | The size of the SSD (e.g., @30@ ).
bunDiskSizeInGb :: Lens' Bundle (Maybe Int)
bunDiskSizeInGb = lens _bunDiskSizeInGb (\ s a -> s{_bunDiskSizeInGb = a});

-- | The price in US dollars (e.g., @5.0@ ).
bunPrice :: Lens' Bundle (Maybe Double)
bunPrice = lens _bunPrice (\ s a -> s{_bunPrice = a});

-- | A Boolean value indicating whether the bundle is active.
bunIsActive :: Lens' Bundle (Maybe Bool)
bunIsActive = lens _bunIsActive (\ s a -> s{_bunIsActive = a});

-- | The amount of RAM in GB (e.g., @2.0@ ).
bunRamSizeInGb :: Lens' Bundle (Maybe Double)
bunRamSizeInGb = lens _bunRamSizeInGb (\ s a -> s{_bunRamSizeInGb = a});

instance FromJSON Bundle where
        parseJSON
          = withObject "Bundle"
              (\ x ->
                 Bundle' <$>
                   (x .:? "cpuCount") <*> (x .:? "transferPerMonthInGb")
                     <*> (x .:? "bundleId")
                     <*> (x .:? "instanceType")
                     <*> (x .:? "name")
                     <*> (x .:? "power")
                     <*> (x .:? "diskSizeInGb")
                     <*> (x .:? "price")
                     <*> (x .:? "isActive")
                     <*> (x .:? "ramSizeInGb"))

instance Hashable Bundle

instance NFData Bundle

-- | Describes the hard disk (an SSD).
--
--
--
-- /See:/ 'disk' smart constructor.
data Disk = Disk'
    { _dResourceType    :: !(Maybe ResourceType)
    , _dArn             :: !(Maybe Text)
    , _dPath            :: !(Maybe Text)
    , _dCreatedAt       :: !(Maybe POSIX)
    , _dLocation        :: !(Maybe ResourceLocation)
    , _dIops            :: !(Maybe Int)
    , _dIsAttached      :: !(Maybe Bool)
    , _dAttachmentState :: !(Maybe Text)
    , _dName            :: !(Maybe Text)
    , _dSizeInGb        :: !(Maybe Int)
    , _dSupportCode     :: !(Maybe Text)
    , _dIsSystemDisk    :: !(Maybe Bool)
    , _dAttachedTo      :: !(Maybe Text)
    , _dGbInUse         :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Disk' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dResourceType' - The resource type of the disk.
--
-- * 'dArn' - The Amazon Resource Name (ARN) of the disk.
--
-- * 'dPath' - The disk path.
--
-- * 'dCreatedAt' - The date when the disk was created.
--
-- * 'dLocation' - The region and Availability Zone where the disk is located.
--
-- * 'dIops' - The input/output operations per second (IOPS) of the disk.
--
-- * 'dIsAttached' - A Boolean value indicating whether the disk is attached.
--
-- * 'dAttachmentState' - The attachment state of the disk.
--
-- * 'dName' - The name of the disk.
--
-- * 'dSizeInGb' - The size of the disk in GB.
--
-- * 'dSupportCode' - The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- * 'dIsSystemDisk' - A Boolean value indicating whether this disk is a system disk (has an operating system loaded on it).
--
-- * 'dAttachedTo' - The resources to which the disk is attached.
--
-- * 'dGbInUse' - The number of GB in use by the disk.
disk
    :: Disk
disk =
    Disk'
    { _dResourceType = Nothing
    , _dArn = Nothing
    , _dPath = Nothing
    , _dCreatedAt = Nothing
    , _dLocation = Nothing
    , _dIops = Nothing
    , _dIsAttached = Nothing
    , _dAttachmentState = Nothing
    , _dName = Nothing
    , _dSizeInGb = Nothing
    , _dSupportCode = Nothing
    , _dIsSystemDisk = Nothing
    , _dAttachedTo = Nothing
    , _dGbInUse = Nothing
    }

-- | The resource type of the disk.
dResourceType :: Lens' Disk (Maybe ResourceType)
dResourceType = lens _dResourceType (\ s a -> s{_dResourceType = a});

-- | The Amazon Resource Name (ARN) of the disk.
dArn :: Lens' Disk (Maybe Text)
dArn = lens _dArn (\ s a -> s{_dArn = a});

-- | The disk path.
dPath :: Lens' Disk (Maybe Text)
dPath = lens _dPath (\ s a -> s{_dPath = a});

-- | The date when the disk was created.
dCreatedAt :: Lens' Disk (Maybe UTCTime)
dCreatedAt = lens _dCreatedAt (\ s a -> s{_dCreatedAt = a}) . mapping _Time;

-- | The region and Availability Zone where the disk is located.
dLocation :: Lens' Disk (Maybe ResourceLocation)
dLocation = lens _dLocation (\ s a -> s{_dLocation = a});

-- | The input/output operations per second (IOPS) of the disk.
dIops :: Lens' Disk (Maybe Int)
dIops = lens _dIops (\ s a -> s{_dIops = a});

-- | A Boolean value indicating whether the disk is attached.
dIsAttached :: Lens' Disk (Maybe Bool)
dIsAttached = lens _dIsAttached (\ s a -> s{_dIsAttached = a});

-- | The attachment state of the disk.
dAttachmentState :: Lens' Disk (Maybe Text)
dAttachmentState = lens _dAttachmentState (\ s a -> s{_dAttachmentState = a});

-- | The name of the disk.
dName :: Lens' Disk (Maybe Text)
dName = lens _dName (\ s a -> s{_dName = a});

-- | The size of the disk in GB.
dSizeInGb :: Lens' Disk (Maybe Int)
dSizeInGb = lens _dSizeInGb (\ s a -> s{_dSizeInGb = a});

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
dSupportCode :: Lens' Disk (Maybe Text)
dSupportCode = lens _dSupportCode (\ s a -> s{_dSupportCode = a});

-- | A Boolean value indicating whether this disk is a system disk (has an operating system loaded on it).
dIsSystemDisk :: Lens' Disk (Maybe Bool)
dIsSystemDisk = lens _dIsSystemDisk (\ s a -> s{_dIsSystemDisk = a});

-- | The resources to which the disk is attached.
dAttachedTo :: Lens' Disk (Maybe Text)
dAttachedTo = lens _dAttachedTo (\ s a -> s{_dAttachedTo = a});

-- | The number of GB in use by the disk.
dGbInUse :: Lens' Disk (Maybe Int)
dGbInUse = lens _dGbInUse (\ s a -> s{_dGbInUse = a});

instance FromJSON Disk where
        parseJSON
          = withObject "Disk"
              (\ x ->
                 Disk' <$>
                   (x .:? "resourceType") <*> (x .:? "arn") <*>
                     (x .:? "path")
                     <*> (x .:? "createdAt")
                     <*> (x .:? "location")
                     <*> (x .:? "iops")
                     <*> (x .:? "isAttached")
                     <*> (x .:? "attachmentState")
                     <*> (x .:? "name")
                     <*> (x .:? "sizeInGb")
                     <*> (x .:? "supportCode")
                     <*> (x .:? "isSystemDisk")
                     <*> (x .:? "attachedTo")
                     <*> (x .:? "gbInUse"))

instance Hashable Disk

instance NFData Disk

-- | Describes a domain where you are storing recordsets in Lightsail.
--
--
--
-- /See:/ 'domain' smart constructor.
data Domain = Domain'
    { _domResourceType  :: !(Maybe ResourceType)
    , _domDomainEntries :: !(Maybe [DomainEntry])
    , _domArn           :: !(Maybe Text)
    , _domCreatedAt     :: !(Maybe POSIX)
    , _domLocation      :: !(Maybe ResourceLocation)
    , _domName          :: !(Maybe Text)
    , _domSupportCode   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Domain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'domResourceType' - The resource type.
--
-- * 'domDomainEntries' - An array of key-value pairs containing information about the domain entries.
--
-- * 'domArn' - The Amazon Resource Name (ARN) of the domain recordset (e.g., @arn:aws:lightsail:global:123456789101:Domain/824cede0-abc7-4f84-8dbc-12345EXAMPLE@ ).
--
-- * 'domCreatedAt' - The date when the domain recordset was created.
--
-- * 'domLocation' - The AWS Region and Availability Zones where the domain recordset was created.
--
-- * 'domName' - The name of the domain.
--
-- * 'domSupportCode' - The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
domain
    :: Domain
domain =
    Domain'
    { _domResourceType = Nothing
    , _domDomainEntries = Nothing
    , _domArn = Nothing
    , _domCreatedAt = Nothing
    , _domLocation = Nothing
    , _domName = Nothing
    , _domSupportCode = Nothing
    }

-- | The resource type.
domResourceType :: Lens' Domain (Maybe ResourceType)
domResourceType = lens _domResourceType (\ s a -> s{_domResourceType = a});

-- | An array of key-value pairs containing information about the domain entries.
domDomainEntries :: Lens' Domain [DomainEntry]
domDomainEntries = lens _domDomainEntries (\ s a -> s{_domDomainEntries = a}) . _Default . _Coerce;

-- | The Amazon Resource Name (ARN) of the domain recordset (e.g., @arn:aws:lightsail:global:123456789101:Domain/824cede0-abc7-4f84-8dbc-12345EXAMPLE@ ).
domArn :: Lens' Domain (Maybe Text)
domArn = lens _domArn (\ s a -> s{_domArn = a});

-- | The date when the domain recordset was created.
domCreatedAt :: Lens' Domain (Maybe UTCTime)
domCreatedAt = lens _domCreatedAt (\ s a -> s{_domCreatedAt = a}) . mapping _Time;

-- | The AWS Region and Availability Zones where the domain recordset was created.
domLocation :: Lens' Domain (Maybe ResourceLocation)
domLocation = lens _domLocation (\ s a -> s{_domLocation = a});

-- | The name of the domain.
domName :: Lens' Domain (Maybe Text)
domName = lens _domName (\ s a -> s{_domName = a});

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
domSupportCode :: Lens' Domain (Maybe Text)
domSupportCode = lens _domSupportCode (\ s a -> s{_domSupportCode = a});

instance FromJSON Domain where
        parseJSON
          = withObject "Domain"
              (\ x ->
                 Domain' <$>
                   (x .:? "resourceType") <*>
                     (x .:? "domainEntries" .!= mempty)
                     <*> (x .:? "arn")
                     <*> (x .:? "createdAt")
                     <*> (x .:? "location")
                     <*> (x .:? "name")
                     <*> (x .:? "supportCode"))

instance Hashable Domain

instance NFData Domain

-- | Describes a domain recordset entry.
--
--
--
-- /See:/ 'domainEntry' smart constructor.
data DomainEntry = DomainEntry'
    { _deName    :: !(Maybe Text)
    , _deId      :: !(Maybe Text)
    , _deOptions :: !(Maybe (Map Text Text))
    , _deType    :: !(Maybe Text)
    , _deTarget  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DomainEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deName' - The name of the domain.
--
-- * 'deId' - The ID of the domain recordset entry.
--
-- * 'deOptions' - The options for the domain entry.
--
-- * 'deType' - The type of domain entry (e.g., @SOA@ or @NS@ ).
--
-- * 'deTarget' - The target AWS name server (e.g., @ns-111.awsdns-22.com.@ ).
domainEntry
    :: DomainEntry
domainEntry =
    DomainEntry'
    { _deName = Nothing
    , _deId = Nothing
    , _deOptions = Nothing
    , _deType = Nothing
    , _deTarget = Nothing
    }

-- | The name of the domain.
deName :: Lens' DomainEntry (Maybe Text)
deName = lens _deName (\ s a -> s{_deName = a});

-- | The ID of the domain recordset entry.
deId :: Lens' DomainEntry (Maybe Text)
deId = lens _deId (\ s a -> s{_deId = a});

-- | The options for the domain entry.
deOptions :: Lens' DomainEntry (HashMap Text Text)
deOptions = lens _deOptions (\ s a -> s{_deOptions = a}) . _Default . _Map;

-- | The type of domain entry (e.g., @SOA@ or @NS@ ).
deType :: Lens' DomainEntry (Maybe Text)
deType = lens _deType (\ s a -> s{_deType = a});

-- | The target AWS name server (e.g., @ns-111.awsdns-22.com.@ ).
deTarget :: Lens' DomainEntry (Maybe Text)
deTarget = lens _deTarget (\ s a -> s{_deTarget = a});

instance FromJSON DomainEntry where
        parseJSON
          = withObject "DomainEntry"
              (\ x ->
                 DomainEntry' <$>
                   (x .:? "name") <*> (x .:? "id") <*>
                     (x .:? "options" .!= mempty)
                     <*> (x .:? "type")
                     <*> (x .:? "target"))

instance Hashable DomainEntry

instance NFData DomainEntry

instance ToJSON DomainEntry where
        toJSON DomainEntry'{..}
          = object
              (catMaybes
                 [("name" .=) <$> _deName, ("id" .=) <$> _deId,
                  ("options" .=) <$> _deOptions,
                  ("type" .=) <$> _deType,
                  ("target" .=) <$> _deTarget])

-- | Describes an instance (a virtual private server).
--
--
--
-- /See:/ 'instance'' smart constructor.
data Instance = Instance'
    { _iState            :: !(Maybe InstanceState)
    , _iIpv6Address      :: !(Maybe Text)
    , _iResourceType     :: !(Maybe ResourceType)
    , _iArn              :: !(Maybe Text)
    , _iCreatedAt        :: !(Maybe POSIX)
    , _iLocation         :: !(Maybe ResourceLocation)
    , _iSshKeyName       :: !(Maybe Text)
    , _iUsername         :: !(Maybe Text)
    , _iNetworking       :: !(Maybe InstanceNetworking)
    , _iBundleId         :: !(Maybe Text)
    , _iName             :: !(Maybe Text)
    , _iSupportCode      :: !(Maybe Text)
    , _iBlueprintId      :: !(Maybe Text)
    , _iPrivateIPAddress :: !(Maybe Text)
    , _iBlueprintName    :: !(Maybe Text)
    , _iIsStaticIP       :: !(Maybe Bool)
    , _iPublicIPAddress  :: !(Maybe Text)
    , _iHardware         :: !(Maybe InstanceHardware)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iState' - The status code and the state (e.g., @running@ ) for the instance.
--
-- * 'iIpv6Address' - The IPv6 address of the instance.
--
-- * 'iResourceType' - The type of resource (usually @Instance@ ).
--
-- * 'iArn' - The Amazon Resource Name (ARN) of the instance (e.g., @arn:aws:lightsail:us-east-1:123456789101:Instance/244ad76f-8aad-4741-809f-12345EXAMPLE@ ).
--
-- * 'iCreatedAt' - The timestamp when the instance was created (e.g., @1479734909.17@ ).
--
-- * 'iLocation' - The region name and availability zone where the instance is located.
--
-- * 'iSshKeyName' - The name of the SSH key being used to connect to the instance (e.g., @LightsailDefaultKeyPair@ ).
--
-- * 'iUsername' - The user name for connecting to the instance (e.g., @ec2-user@ ).
--
-- * 'iNetworking' - Information about the public ports and monthly data transfer rates for the instance.
--
-- * 'iBundleId' - The bundle for the instance (e.g., @micro_1_0@ ).
--
-- * 'iName' - The name the user gave the instance (e.g., @Amazon_Linux-1GB-Virginia-1@ ).
--
-- * 'iSupportCode' - The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- * 'iBlueprintId' - The blueprint ID (e.g., @os_amlinux_2016_03@ ).
--
-- * 'iPrivateIPAddress' - The private IP address of the instance.
--
-- * 'iBlueprintName' - The friendly name of the blueprint (e.g., @Amazon Linux@ ).
--
-- * 'iIsStaticIP' - A Boolean value indicating whether this instance has a static IP assigned to it.
--
-- * 'iPublicIPAddress' - The public IP address of the instance.
--
-- * 'iHardware' - The size of the vCPU and the amount of RAM for the instance.
instance'
    :: Instance
instance' =
    Instance'
    { _iState = Nothing
    , _iIpv6Address = Nothing
    , _iResourceType = Nothing
    , _iArn = Nothing
    , _iCreatedAt = Nothing
    , _iLocation = Nothing
    , _iSshKeyName = Nothing
    , _iUsername = Nothing
    , _iNetworking = Nothing
    , _iBundleId = Nothing
    , _iName = Nothing
    , _iSupportCode = Nothing
    , _iBlueprintId = Nothing
    , _iPrivateIPAddress = Nothing
    , _iBlueprintName = Nothing
    , _iIsStaticIP = Nothing
    , _iPublicIPAddress = Nothing
    , _iHardware = Nothing
    }

-- | The status code and the state (e.g., @running@ ) for the instance.
iState :: Lens' Instance (Maybe InstanceState)
iState = lens _iState (\ s a -> s{_iState = a});

-- | The IPv6 address of the instance.
iIpv6Address :: Lens' Instance (Maybe Text)
iIpv6Address = lens _iIpv6Address (\ s a -> s{_iIpv6Address = a});

-- | The type of resource (usually @Instance@ ).
iResourceType :: Lens' Instance (Maybe ResourceType)
iResourceType = lens _iResourceType (\ s a -> s{_iResourceType = a});

-- | The Amazon Resource Name (ARN) of the instance (e.g., @arn:aws:lightsail:us-east-1:123456789101:Instance/244ad76f-8aad-4741-809f-12345EXAMPLE@ ).
iArn :: Lens' Instance (Maybe Text)
iArn = lens _iArn (\ s a -> s{_iArn = a});

-- | The timestamp when the instance was created (e.g., @1479734909.17@ ).
iCreatedAt :: Lens' Instance (Maybe UTCTime)
iCreatedAt = lens _iCreatedAt (\ s a -> s{_iCreatedAt = a}) . mapping _Time;

-- | The region name and availability zone where the instance is located.
iLocation :: Lens' Instance (Maybe ResourceLocation)
iLocation = lens _iLocation (\ s a -> s{_iLocation = a});

-- | The name of the SSH key being used to connect to the instance (e.g., @LightsailDefaultKeyPair@ ).
iSshKeyName :: Lens' Instance (Maybe Text)
iSshKeyName = lens _iSshKeyName (\ s a -> s{_iSshKeyName = a});

-- | The user name for connecting to the instance (e.g., @ec2-user@ ).
iUsername :: Lens' Instance (Maybe Text)
iUsername = lens _iUsername (\ s a -> s{_iUsername = a});

-- | Information about the public ports and monthly data transfer rates for the instance.
iNetworking :: Lens' Instance (Maybe InstanceNetworking)
iNetworking = lens _iNetworking (\ s a -> s{_iNetworking = a});

-- | The bundle for the instance (e.g., @micro_1_0@ ).
iBundleId :: Lens' Instance (Maybe Text)
iBundleId = lens _iBundleId (\ s a -> s{_iBundleId = a});

-- | The name the user gave the instance (e.g., @Amazon_Linux-1GB-Virginia-1@ ).
iName :: Lens' Instance (Maybe Text)
iName = lens _iName (\ s a -> s{_iName = a});

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
iSupportCode :: Lens' Instance (Maybe Text)
iSupportCode = lens _iSupportCode (\ s a -> s{_iSupportCode = a});

-- | The blueprint ID (e.g., @os_amlinux_2016_03@ ).
iBlueprintId :: Lens' Instance (Maybe Text)
iBlueprintId = lens _iBlueprintId (\ s a -> s{_iBlueprintId = a});

-- | The private IP address of the instance.
iPrivateIPAddress :: Lens' Instance (Maybe Text)
iPrivateIPAddress = lens _iPrivateIPAddress (\ s a -> s{_iPrivateIPAddress = a});

-- | The friendly name of the blueprint (e.g., @Amazon Linux@ ).
iBlueprintName :: Lens' Instance (Maybe Text)
iBlueprintName = lens _iBlueprintName (\ s a -> s{_iBlueprintName = a});

-- | A Boolean value indicating whether this instance has a static IP assigned to it.
iIsStaticIP :: Lens' Instance (Maybe Bool)
iIsStaticIP = lens _iIsStaticIP (\ s a -> s{_iIsStaticIP = a});

-- | The public IP address of the instance.
iPublicIPAddress :: Lens' Instance (Maybe Text)
iPublicIPAddress = lens _iPublicIPAddress (\ s a -> s{_iPublicIPAddress = a});

-- | The size of the vCPU and the amount of RAM for the instance.
iHardware :: Lens' Instance (Maybe InstanceHardware)
iHardware = lens _iHardware (\ s a -> s{_iHardware = a});

instance FromJSON Instance where
        parseJSON
          = withObject "Instance"
              (\ x ->
                 Instance' <$>
                   (x .:? "state") <*> (x .:? "ipv6Address") <*>
                     (x .:? "resourceType")
                     <*> (x .:? "arn")
                     <*> (x .:? "createdAt")
                     <*> (x .:? "location")
                     <*> (x .:? "sshKeyName")
                     <*> (x .:? "username")
                     <*> (x .:? "networking")
                     <*> (x .:? "bundleId")
                     <*> (x .:? "name")
                     <*> (x .:? "supportCode")
                     <*> (x .:? "blueprintId")
                     <*> (x .:? "privateIpAddress")
                     <*> (x .:? "blueprintName")
                     <*> (x .:? "isStaticIp")
                     <*> (x .:? "publicIpAddress")
                     <*> (x .:? "hardware"))

instance Hashable Instance

instance NFData Instance

-- | The parameters for gaining temporary access to one of your Amazon Lightsail instances.
--
--
--
-- /See:/ 'instanceAccessDetails' smart constructor.
data InstanceAccessDetails = InstanceAccessDetails'
    { _iadCertKey      :: !(Maybe Text)
    , _iadIpAddress    :: !(Maybe Text)
    , _iadPrivateKey   :: !(Maybe Text)
    , _iadExpiresAt    :: !(Maybe POSIX)
    , _iadUsername     :: !(Maybe Text)
    , _iadProtocol     :: !(Maybe InstanceAccessProtocol)
    , _iadPassword     :: !(Maybe Text)
    , _iadInstanceName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceAccessDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iadCertKey' - For SSH access, the public key to use when accessing your instance For OpenSSH clients (e.g., command line SSH), you should save this value to @tempkey-cert.pub@ .
--
-- * 'iadIpAddress' - The public IP address of the Amazon Lightsail instance.
--
-- * 'iadPrivateKey' - For SSH access, the temporary private key. For OpenSSH clients (e.g., command line SSH), you should save this value to @tempkey@ ).
--
-- * 'iadExpiresAt' - For SSH access, the date on which the temporary keys expire.
--
-- * 'iadUsername' - The user name to use when logging in to the Amazon Lightsail instance.
--
-- * 'iadProtocol' - The protocol for these Amazon Lightsail instance access details.
--
-- * 'iadPassword' - For RDP access, the temporary password of the Amazon EC2 instance.
--
-- * 'iadInstanceName' - The name of this Amazon Lightsail instance.
instanceAccessDetails
    :: InstanceAccessDetails
instanceAccessDetails =
    InstanceAccessDetails'
    { _iadCertKey = Nothing
    , _iadIpAddress = Nothing
    , _iadPrivateKey = Nothing
    , _iadExpiresAt = Nothing
    , _iadUsername = Nothing
    , _iadProtocol = Nothing
    , _iadPassword = Nothing
    , _iadInstanceName = Nothing
    }

-- | For SSH access, the public key to use when accessing your instance For OpenSSH clients (e.g., command line SSH), you should save this value to @tempkey-cert.pub@ .
iadCertKey :: Lens' InstanceAccessDetails (Maybe Text)
iadCertKey = lens _iadCertKey (\ s a -> s{_iadCertKey = a});

-- | The public IP address of the Amazon Lightsail instance.
iadIpAddress :: Lens' InstanceAccessDetails (Maybe Text)
iadIpAddress = lens _iadIpAddress (\ s a -> s{_iadIpAddress = a});

-- | For SSH access, the temporary private key. For OpenSSH clients (e.g., command line SSH), you should save this value to @tempkey@ ).
iadPrivateKey :: Lens' InstanceAccessDetails (Maybe Text)
iadPrivateKey = lens _iadPrivateKey (\ s a -> s{_iadPrivateKey = a});

-- | For SSH access, the date on which the temporary keys expire.
iadExpiresAt :: Lens' InstanceAccessDetails (Maybe UTCTime)
iadExpiresAt = lens _iadExpiresAt (\ s a -> s{_iadExpiresAt = a}) . mapping _Time;

-- | The user name to use when logging in to the Amazon Lightsail instance.
iadUsername :: Lens' InstanceAccessDetails (Maybe Text)
iadUsername = lens _iadUsername (\ s a -> s{_iadUsername = a});

-- | The protocol for these Amazon Lightsail instance access details.
iadProtocol :: Lens' InstanceAccessDetails (Maybe InstanceAccessProtocol)
iadProtocol = lens _iadProtocol (\ s a -> s{_iadProtocol = a});

-- | For RDP access, the temporary password of the Amazon EC2 instance.
iadPassword :: Lens' InstanceAccessDetails (Maybe Text)
iadPassword = lens _iadPassword (\ s a -> s{_iadPassword = a});

-- | The name of this Amazon Lightsail instance.
iadInstanceName :: Lens' InstanceAccessDetails (Maybe Text)
iadInstanceName = lens _iadInstanceName (\ s a -> s{_iadInstanceName = a});

instance FromJSON InstanceAccessDetails where
        parseJSON
          = withObject "InstanceAccessDetails"
              (\ x ->
                 InstanceAccessDetails' <$>
                   (x .:? "certKey") <*> (x .:? "ipAddress") <*>
                     (x .:? "privateKey")
                     <*> (x .:? "expiresAt")
                     <*> (x .:? "username")
                     <*> (x .:? "protocol")
                     <*> (x .:? "password")
                     <*> (x .:? "instanceName"))

instance Hashable InstanceAccessDetails

instance NFData InstanceAccessDetails

-- | Describes the hardware for the instance.
--
--
--
-- /See:/ 'instanceHardware' smart constructor.
data InstanceHardware = InstanceHardware'
    { _ihCpuCount    :: !(Maybe Int)
    , _ihDisks       :: !(Maybe [Disk])
    , _ihRamSizeInGb :: !(Maybe Double)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceHardware' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ihCpuCount' - The number of vCPUs the instance has.
--
-- * 'ihDisks' - The disks attached to the instance.
--
-- * 'ihRamSizeInGb' - The amount of RAM in GB on the instance (e.g., @1.0@ ).
instanceHardware
    :: InstanceHardware
instanceHardware =
    InstanceHardware'
    { _ihCpuCount = Nothing
    , _ihDisks = Nothing
    , _ihRamSizeInGb = Nothing
    }

-- | The number of vCPUs the instance has.
ihCpuCount :: Lens' InstanceHardware (Maybe Int)
ihCpuCount = lens _ihCpuCount (\ s a -> s{_ihCpuCount = a});

-- | The disks attached to the instance.
ihDisks :: Lens' InstanceHardware [Disk]
ihDisks = lens _ihDisks (\ s a -> s{_ihDisks = a}) . _Default . _Coerce;

-- | The amount of RAM in GB on the instance (e.g., @1.0@ ).
ihRamSizeInGb :: Lens' InstanceHardware (Maybe Double)
ihRamSizeInGb = lens _ihRamSizeInGb (\ s a -> s{_ihRamSizeInGb = a});

instance FromJSON InstanceHardware where
        parseJSON
          = withObject "InstanceHardware"
              (\ x ->
                 InstanceHardware' <$>
                   (x .:? "cpuCount") <*> (x .:? "disks" .!= mempty) <*>
                     (x .:? "ramSizeInGb"))

instance Hashable InstanceHardware

instance NFData InstanceHardware

-- | Describes monthly data transfer rates and port information for an instance.
--
--
--
-- /See:/ 'instanceNetworking' smart constructor.
data InstanceNetworking = InstanceNetworking'
    { _inMonthlyTransfer :: !(Maybe MonthlyTransfer)
    , _inPorts           :: !(Maybe [InstancePortInfo])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceNetworking' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'inMonthlyTransfer' - The amount of data in GB allocated for monthly data transfers.
--
-- * 'inPorts' - An array of key-value pairs containing information about the ports on the instance.
instanceNetworking
    :: InstanceNetworking
instanceNetworking =
    InstanceNetworking'
    { _inMonthlyTransfer = Nothing
    , _inPorts = Nothing
    }

-- | The amount of data in GB allocated for monthly data transfers.
inMonthlyTransfer :: Lens' InstanceNetworking (Maybe MonthlyTransfer)
inMonthlyTransfer = lens _inMonthlyTransfer (\ s a -> s{_inMonthlyTransfer = a});

-- | An array of key-value pairs containing information about the ports on the instance.
inPorts :: Lens' InstanceNetworking [InstancePortInfo]
inPorts = lens _inPorts (\ s a -> s{_inPorts = a}) . _Default . _Coerce;

instance FromJSON InstanceNetworking where
        parseJSON
          = withObject "InstanceNetworking"
              (\ x ->
                 InstanceNetworking' <$>
                   (x .:? "monthlyTransfer") <*>
                     (x .:? "ports" .!= mempty))

instance Hashable InstanceNetworking

instance NFData InstanceNetworking

-- | Describes information about the instance ports.
--
--
--
-- /See:/ 'instancePortInfo' smart constructor.
data InstancePortInfo = InstancePortInfo'
    { _ipiFromPort        :: !(Maybe Nat)
    , _ipiCommonName      :: !(Maybe Text)
    , _ipiProtocol        :: !(Maybe NetworkProtocol)
    , _ipiAccessDirection :: !(Maybe AccessDirection)
    , _ipiAccessType      :: !(Maybe PortAccessType)
    , _ipiToPort          :: !(Maybe Nat)
    , _ipiAccessFrom      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstancePortInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipiFromPort' - The first port in the range.
--
-- * 'ipiCommonName' - The common name.
--
-- * 'ipiProtocol' - The protocol being used. Can be one of the following.     * @tcp@ - Transmission Control Protocol (TCP) provides reliable, ordered, and error-checked delivery of streamed data between applications running on hosts communicating by an IP network. If you have an application that doesn't require reliable data stream service, use UDP instead.     * @all@ - All transport layer protocol types. For more general information, see <https://en.wikipedia.org/wiki/Transport_layer Transport layer> on Wikipedia.     * @udp@ - With User Datagram Protocol (UDP), computer applications can send messages (or datagrams) to other hosts on an Internet Protocol (IP) network. Prior communications are not required to set up transmission channels or data paths. Applications that don't require reliable data stream service can use UDP, which provides a connectionless datagram service that emphasizes reduced latency over reliability. If you do require reliable data stream service, use TCP instead.
--
-- * 'ipiAccessDirection' - The access direction (@inbound@ or @outbound@ ).
--
-- * 'ipiAccessType' - The type of access (@Public@ or @Private@ ).
--
-- * 'ipiToPort' - The last port in the range.
--
-- * 'ipiAccessFrom' - The location from which access is allowed (e.g., @Anywhere (0.0.0.0/0)@ ).
instancePortInfo
    :: InstancePortInfo
instancePortInfo =
    InstancePortInfo'
    { _ipiFromPort = Nothing
    , _ipiCommonName = Nothing
    , _ipiProtocol = Nothing
    , _ipiAccessDirection = Nothing
    , _ipiAccessType = Nothing
    , _ipiToPort = Nothing
    , _ipiAccessFrom = Nothing
    }

-- | The first port in the range.
ipiFromPort :: Lens' InstancePortInfo (Maybe Natural)
ipiFromPort = lens _ipiFromPort (\ s a -> s{_ipiFromPort = a}) . mapping _Nat;

-- | The common name.
ipiCommonName :: Lens' InstancePortInfo (Maybe Text)
ipiCommonName = lens _ipiCommonName (\ s a -> s{_ipiCommonName = a});

-- | The protocol being used. Can be one of the following.     * @tcp@ - Transmission Control Protocol (TCP) provides reliable, ordered, and error-checked delivery of streamed data between applications running on hosts communicating by an IP network. If you have an application that doesn't require reliable data stream service, use UDP instead.     * @all@ - All transport layer protocol types. For more general information, see <https://en.wikipedia.org/wiki/Transport_layer Transport layer> on Wikipedia.     * @udp@ - With User Datagram Protocol (UDP), computer applications can send messages (or datagrams) to other hosts on an Internet Protocol (IP) network. Prior communications are not required to set up transmission channels or data paths. Applications that don't require reliable data stream service can use UDP, which provides a connectionless datagram service that emphasizes reduced latency over reliability. If you do require reliable data stream service, use TCP instead.
ipiProtocol :: Lens' InstancePortInfo (Maybe NetworkProtocol)
ipiProtocol = lens _ipiProtocol (\ s a -> s{_ipiProtocol = a});

-- | The access direction (@inbound@ or @outbound@ ).
ipiAccessDirection :: Lens' InstancePortInfo (Maybe AccessDirection)
ipiAccessDirection = lens _ipiAccessDirection (\ s a -> s{_ipiAccessDirection = a});

-- | The type of access (@Public@ or @Private@ ).
ipiAccessType :: Lens' InstancePortInfo (Maybe PortAccessType)
ipiAccessType = lens _ipiAccessType (\ s a -> s{_ipiAccessType = a});

-- | The last port in the range.
ipiToPort :: Lens' InstancePortInfo (Maybe Natural)
ipiToPort = lens _ipiToPort (\ s a -> s{_ipiToPort = a}) . mapping _Nat;

-- | The location from which access is allowed (e.g., @Anywhere (0.0.0.0/0)@ ).
ipiAccessFrom :: Lens' InstancePortInfo (Maybe Text)
ipiAccessFrom = lens _ipiAccessFrom (\ s a -> s{_ipiAccessFrom = a});

instance FromJSON InstancePortInfo where
        parseJSON
          = withObject "InstancePortInfo"
              (\ x ->
                 InstancePortInfo' <$>
                   (x .:? "fromPort") <*> (x .:? "commonName") <*>
                     (x .:? "protocol")
                     <*> (x .:? "accessDirection")
                     <*> (x .:? "accessType")
                     <*> (x .:? "toPort")
                     <*> (x .:? "accessFrom"))

instance Hashable InstancePortInfo

instance NFData InstancePortInfo

-- | Describes the port state.
--
--
--
-- /See:/ 'instancePortState' smart constructor.
data InstancePortState = InstancePortState'
    { _ipsFromPort :: !(Maybe Nat)
    , _ipsState    :: !(Maybe PortState)
    , _ipsProtocol :: !(Maybe NetworkProtocol)
    , _ipsToPort   :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstancePortState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipsFromPort' - The first port in the range.
--
-- * 'ipsState' - Specifies whether the instance port is @open@ or @closed@ .
--
-- * 'ipsProtocol' - The protocol being used. Can be one of the following.     * @tcp@ - Transmission Control Protocol (TCP) provides reliable, ordered, and error-checked delivery of streamed data between applications running on hosts communicating by an IP network. If you have an application that doesn't require reliable data stream service, use UDP instead.     * @all@ - All transport layer protocol types. For more general information, see <https://en.wikipedia.org/wiki/Transport_layer Transport layer> on Wikipedia.     * @udp@ - With User Datagram Protocol (UDP), computer applications can send messages (or datagrams) to other hosts on an Internet Protocol (IP) network. Prior communications are not required to set up transmission channels or data paths. Applications that don't require reliable data stream service can use UDP, which provides a connectionless datagram service that emphasizes reduced latency over reliability. If you do require reliable data stream service, use TCP instead.
--
-- * 'ipsToPort' - The last port in the range.
instancePortState
    :: InstancePortState
instancePortState =
    InstancePortState'
    { _ipsFromPort = Nothing
    , _ipsState = Nothing
    , _ipsProtocol = Nothing
    , _ipsToPort = Nothing
    }

-- | The first port in the range.
ipsFromPort :: Lens' InstancePortState (Maybe Natural)
ipsFromPort = lens _ipsFromPort (\ s a -> s{_ipsFromPort = a}) . mapping _Nat;

-- | Specifies whether the instance port is @open@ or @closed@ .
ipsState :: Lens' InstancePortState (Maybe PortState)
ipsState = lens _ipsState (\ s a -> s{_ipsState = a});

-- | The protocol being used. Can be one of the following.     * @tcp@ - Transmission Control Protocol (TCP) provides reliable, ordered, and error-checked delivery of streamed data between applications running on hosts communicating by an IP network. If you have an application that doesn't require reliable data stream service, use UDP instead.     * @all@ - All transport layer protocol types. For more general information, see <https://en.wikipedia.org/wiki/Transport_layer Transport layer> on Wikipedia.     * @udp@ - With User Datagram Protocol (UDP), computer applications can send messages (or datagrams) to other hosts on an Internet Protocol (IP) network. Prior communications are not required to set up transmission channels or data paths. Applications that don't require reliable data stream service can use UDP, which provides a connectionless datagram service that emphasizes reduced latency over reliability. If you do require reliable data stream service, use TCP instead.
ipsProtocol :: Lens' InstancePortState (Maybe NetworkProtocol)
ipsProtocol = lens _ipsProtocol (\ s a -> s{_ipsProtocol = a});

-- | The last port in the range.
ipsToPort :: Lens' InstancePortState (Maybe Natural)
ipsToPort = lens _ipsToPort (\ s a -> s{_ipsToPort = a}) . mapping _Nat;

instance FromJSON InstancePortState where
        parseJSON
          = withObject "InstancePortState"
              (\ x ->
                 InstancePortState' <$>
                   (x .:? "fromPort") <*> (x .:? "state") <*>
                     (x .:? "protocol")
                     <*> (x .:? "toPort"))

instance Hashable InstancePortState

instance NFData InstancePortState

-- | Describes the snapshot of the virtual private server, or /instance/ .
--
--
--
-- /See:/ 'instanceSnapshot' smart constructor.
data InstanceSnapshot = InstanceSnapshot'
    { _insFromBlueprintId  :: !(Maybe Text)
    , _insState            :: !(Maybe InstanceSnapshotState)
    , _insResourceType     :: !(Maybe ResourceType)
    , _insArn              :: !(Maybe Text)
    , _insCreatedAt        :: !(Maybe POSIX)
    , _insLocation         :: !(Maybe ResourceLocation)
    , _insProgress         :: !(Maybe Text)
    , _insName             :: !(Maybe Text)
    , _insFromBundleId     :: !(Maybe Text)
    , _insSizeInGb         :: !(Maybe Int)
    , _insSupportCode      :: !(Maybe Text)
    , _insFromInstanceARN  :: !(Maybe Text)
    , _insFromInstanceName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'insFromBlueprintId' - The blueprint ID from which you created the snapshot (e.g., @os_debian_8_3@ ). A blueprint is a virtual private server (or /instance/ ) image used to create instances quickly.
--
-- * 'insState' - The state the snapshot is in.
--
-- * 'insResourceType' - The type of resource (usually @InstanceSnapshot@ ).
--
-- * 'insArn' - The Amazon Resource Name (ARN) of the snapshot (e.g., @arn:aws:lightsail:us-east-1:123456789101:InstanceSnapshot/d23b5706-3322-4d83-81e5-12345EXAMPLE@ ).
--
-- * 'insCreatedAt' - The timestamp when the snapshot was created (e.g., @1479907467.024@ ).
--
-- * 'insLocation' - The region name and availability zone where you created the snapshot.
--
-- * 'insProgress' - The progress of the snapshot.
--
-- * 'insName' - The name of the snapshot.
--
-- * 'insFromBundleId' - The bundle ID from which you created the snapshot (e.g., @micro_1_0@ ).
--
-- * 'insSizeInGb' - The size in GB of the SSD.
--
-- * 'insSupportCode' - The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- * 'insFromInstanceARN' - The Amazon Resource Name (ARN) of the instance from which the snapshot was created (e.g., @arn:aws:lightsail:us-east-1:123456789101:Instance/64b8404c-ccb1-430b-8daf-12345EXAMPLE@ ).
--
-- * 'insFromInstanceName' - The instance from which the snapshot was created.
instanceSnapshot
    :: InstanceSnapshot
instanceSnapshot =
    InstanceSnapshot'
    { _insFromBlueprintId = Nothing
    , _insState = Nothing
    , _insResourceType = Nothing
    , _insArn = Nothing
    , _insCreatedAt = Nothing
    , _insLocation = Nothing
    , _insProgress = Nothing
    , _insName = Nothing
    , _insFromBundleId = Nothing
    , _insSizeInGb = Nothing
    , _insSupportCode = Nothing
    , _insFromInstanceARN = Nothing
    , _insFromInstanceName = Nothing
    }

-- | The blueprint ID from which you created the snapshot (e.g., @os_debian_8_3@ ). A blueprint is a virtual private server (or /instance/ ) image used to create instances quickly.
insFromBlueprintId :: Lens' InstanceSnapshot (Maybe Text)
insFromBlueprintId = lens _insFromBlueprintId (\ s a -> s{_insFromBlueprintId = a});

-- | The state the snapshot is in.
insState :: Lens' InstanceSnapshot (Maybe InstanceSnapshotState)
insState = lens _insState (\ s a -> s{_insState = a});

-- | The type of resource (usually @InstanceSnapshot@ ).
insResourceType :: Lens' InstanceSnapshot (Maybe ResourceType)
insResourceType = lens _insResourceType (\ s a -> s{_insResourceType = a});

-- | The Amazon Resource Name (ARN) of the snapshot (e.g., @arn:aws:lightsail:us-east-1:123456789101:InstanceSnapshot/d23b5706-3322-4d83-81e5-12345EXAMPLE@ ).
insArn :: Lens' InstanceSnapshot (Maybe Text)
insArn = lens _insArn (\ s a -> s{_insArn = a});

-- | The timestamp when the snapshot was created (e.g., @1479907467.024@ ).
insCreatedAt :: Lens' InstanceSnapshot (Maybe UTCTime)
insCreatedAt = lens _insCreatedAt (\ s a -> s{_insCreatedAt = a}) . mapping _Time;

-- | The region name and availability zone where you created the snapshot.
insLocation :: Lens' InstanceSnapshot (Maybe ResourceLocation)
insLocation = lens _insLocation (\ s a -> s{_insLocation = a});

-- | The progress of the snapshot.
insProgress :: Lens' InstanceSnapshot (Maybe Text)
insProgress = lens _insProgress (\ s a -> s{_insProgress = a});

-- | The name of the snapshot.
insName :: Lens' InstanceSnapshot (Maybe Text)
insName = lens _insName (\ s a -> s{_insName = a});

-- | The bundle ID from which you created the snapshot (e.g., @micro_1_0@ ).
insFromBundleId :: Lens' InstanceSnapshot (Maybe Text)
insFromBundleId = lens _insFromBundleId (\ s a -> s{_insFromBundleId = a});

-- | The size in GB of the SSD.
insSizeInGb :: Lens' InstanceSnapshot (Maybe Int)
insSizeInGb = lens _insSizeInGb (\ s a -> s{_insSizeInGb = a});

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
insSupportCode :: Lens' InstanceSnapshot (Maybe Text)
insSupportCode = lens _insSupportCode (\ s a -> s{_insSupportCode = a});

-- | The Amazon Resource Name (ARN) of the instance from which the snapshot was created (e.g., @arn:aws:lightsail:us-east-1:123456789101:Instance/64b8404c-ccb1-430b-8daf-12345EXAMPLE@ ).
insFromInstanceARN :: Lens' InstanceSnapshot (Maybe Text)
insFromInstanceARN = lens _insFromInstanceARN (\ s a -> s{_insFromInstanceARN = a});

-- | The instance from which the snapshot was created.
insFromInstanceName :: Lens' InstanceSnapshot (Maybe Text)
insFromInstanceName = lens _insFromInstanceName (\ s a -> s{_insFromInstanceName = a});

instance FromJSON InstanceSnapshot where
        parseJSON
          = withObject "InstanceSnapshot"
              (\ x ->
                 InstanceSnapshot' <$>
                   (x .:? "fromBlueprintId") <*> (x .:? "state") <*>
                     (x .:? "resourceType")
                     <*> (x .:? "arn")
                     <*> (x .:? "createdAt")
                     <*> (x .:? "location")
                     <*> (x .:? "progress")
                     <*> (x .:? "name")
                     <*> (x .:? "fromBundleId")
                     <*> (x .:? "sizeInGb")
                     <*> (x .:? "supportCode")
                     <*> (x .:? "fromInstanceArn")
                     <*> (x .:? "fromInstanceName"))

instance Hashable InstanceSnapshot

instance NFData InstanceSnapshot

-- | Describes the virtual private server (or /instance/ ) status.
--
--
--
-- /See:/ 'instanceState' smart constructor.
data InstanceState = InstanceState'
    { _isName :: !(Maybe Text)
    , _isCode :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isName' - The state of the instance (e.g., @running@ or @pending@ ).
--
-- * 'isCode' - The status code for the instance.
instanceState
    :: InstanceState
instanceState =
    InstanceState'
    { _isName = Nothing
    , _isCode = Nothing
    }

-- | The state of the instance (e.g., @running@ or @pending@ ).
isName :: Lens' InstanceState (Maybe Text)
isName = lens _isName (\ s a -> s{_isName = a});

-- | The status code for the instance.
isCode :: Lens' InstanceState (Maybe Int)
isCode = lens _isCode (\ s a -> s{_isCode = a});

instance FromJSON InstanceState where
        parseJSON
          = withObject "InstanceState"
              (\ x ->
                 InstanceState' <$> (x .:? "name") <*> (x .:? "code"))

instance Hashable InstanceState

instance NFData InstanceState

-- | Describes the SSH key pair.
--
--
--
-- /See:/ 'keyPair' smart constructor.
data KeyPair = KeyPair'
    { _kpResourceType :: !(Maybe ResourceType)
    , _kpArn          :: !(Maybe Text)
    , _kpCreatedAt    :: !(Maybe POSIX)
    , _kpLocation     :: !(Maybe ResourceLocation)
    , _kpFingerprint  :: !(Maybe Text)
    , _kpName         :: !(Maybe Text)
    , _kpSupportCode  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'KeyPair' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kpResourceType' - The resource type (usually @KeyPair@ ).
--
-- * 'kpArn' - The Amazon Resource Name (ARN) of the key pair (e.g., @arn:aws:lightsail:us-east-1:123456789101:KeyPair/05859e3d-331d-48ba-9034-12345EXAMPLE@ ).
--
-- * 'kpCreatedAt' - The timestamp when the key pair was created (e.g., @1479816991.349@ ).
--
-- * 'kpLocation' - The region name and Availability Zone where the key pair was created.
--
-- * 'kpFingerprint' - The RSA fingerprint of the key pair.
--
-- * 'kpName' - The friendly name of the SSH key pair.
--
-- * 'kpSupportCode' - The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
keyPair
    :: KeyPair
keyPair =
    KeyPair'
    { _kpResourceType = Nothing
    , _kpArn = Nothing
    , _kpCreatedAt = Nothing
    , _kpLocation = Nothing
    , _kpFingerprint = Nothing
    , _kpName = Nothing
    , _kpSupportCode = Nothing
    }

-- | The resource type (usually @KeyPair@ ).
kpResourceType :: Lens' KeyPair (Maybe ResourceType)
kpResourceType = lens _kpResourceType (\ s a -> s{_kpResourceType = a});

-- | The Amazon Resource Name (ARN) of the key pair (e.g., @arn:aws:lightsail:us-east-1:123456789101:KeyPair/05859e3d-331d-48ba-9034-12345EXAMPLE@ ).
kpArn :: Lens' KeyPair (Maybe Text)
kpArn = lens _kpArn (\ s a -> s{_kpArn = a});

-- | The timestamp when the key pair was created (e.g., @1479816991.349@ ).
kpCreatedAt :: Lens' KeyPair (Maybe UTCTime)
kpCreatedAt = lens _kpCreatedAt (\ s a -> s{_kpCreatedAt = a}) . mapping _Time;

-- | The region name and Availability Zone where the key pair was created.
kpLocation :: Lens' KeyPair (Maybe ResourceLocation)
kpLocation = lens _kpLocation (\ s a -> s{_kpLocation = a});

-- | The RSA fingerprint of the key pair.
kpFingerprint :: Lens' KeyPair (Maybe Text)
kpFingerprint = lens _kpFingerprint (\ s a -> s{_kpFingerprint = a});

-- | The friendly name of the SSH key pair.
kpName :: Lens' KeyPair (Maybe Text)
kpName = lens _kpName (\ s a -> s{_kpName = a});

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
kpSupportCode :: Lens' KeyPair (Maybe Text)
kpSupportCode = lens _kpSupportCode (\ s a -> s{_kpSupportCode = a});

instance FromJSON KeyPair where
        parseJSON
          = withObject "KeyPair"
              (\ x ->
                 KeyPair' <$>
                   (x .:? "resourceType") <*> (x .:? "arn") <*>
                     (x .:? "createdAt")
                     <*> (x .:? "location")
                     <*> (x .:? "fingerprint")
                     <*> (x .:? "name")
                     <*> (x .:? "supportCode"))

instance Hashable KeyPair

instance NFData KeyPair

-- | Describes the metric data point.
--
--
--
-- /See:/ 'metricDatapoint' smart constructor.
data MetricDatapoint = MetricDatapoint'
    { _mdSampleCount :: !(Maybe Double)
    , _mdMaximum     :: !(Maybe Double)
    , _mdAverage     :: !(Maybe Double)
    , _mdMinimum     :: !(Maybe Double)
    , _mdSum         :: !(Maybe Double)
    , _mdTimestamp   :: !(Maybe POSIX)
    , _mdUnit        :: !(Maybe MetricUnit)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MetricDatapoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdSampleCount' - The sample count.
--
-- * 'mdMaximum' - The maximum.
--
-- * 'mdAverage' - The average.
--
-- * 'mdMinimum' - The minimum.
--
-- * 'mdSum' - The sum.
--
-- * 'mdTimestamp' - The timestamp (e.g., @1479816991.349@ ).
--
-- * 'mdUnit' - The unit.
metricDatapoint
    :: MetricDatapoint
metricDatapoint =
    MetricDatapoint'
    { _mdSampleCount = Nothing
    , _mdMaximum = Nothing
    , _mdAverage = Nothing
    , _mdMinimum = Nothing
    , _mdSum = Nothing
    , _mdTimestamp = Nothing
    , _mdUnit = Nothing
    }

-- | The sample count.
mdSampleCount :: Lens' MetricDatapoint (Maybe Double)
mdSampleCount = lens _mdSampleCount (\ s a -> s{_mdSampleCount = a});

-- | The maximum.
mdMaximum :: Lens' MetricDatapoint (Maybe Double)
mdMaximum = lens _mdMaximum (\ s a -> s{_mdMaximum = a});

-- | The average.
mdAverage :: Lens' MetricDatapoint (Maybe Double)
mdAverage = lens _mdAverage (\ s a -> s{_mdAverage = a});

-- | The minimum.
mdMinimum :: Lens' MetricDatapoint (Maybe Double)
mdMinimum = lens _mdMinimum (\ s a -> s{_mdMinimum = a});

-- | The sum.
mdSum :: Lens' MetricDatapoint (Maybe Double)
mdSum = lens _mdSum (\ s a -> s{_mdSum = a});

-- | The timestamp (e.g., @1479816991.349@ ).
mdTimestamp :: Lens' MetricDatapoint (Maybe UTCTime)
mdTimestamp = lens _mdTimestamp (\ s a -> s{_mdTimestamp = a}) . mapping _Time;

-- | The unit.
mdUnit :: Lens' MetricDatapoint (Maybe MetricUnit)
mdUnit = lens _mdUnit (\ s a -> s{_mdUnit = a});

instance FromJSON MetricDatapoint where
        parseJSON
          = withObject "MetricDatapoint"
              (\ x ->
                 MetricDatapoint' <$>
                   (x .:? "sampleCount") <*> (x .:? "maximum") <*>
                     (x .:? "average")
                     <*> (x .:? "minimum")
                     <*> (x .:? "sum")
                     <*> (x .:? "timestamp")
                     <*> (x .:? "unit"))

instance Hashable MetricDatapoint

instance NFData MetricDatapoint

-- | Describes the monthly data transfer in and out of your virtual private server (or /instance/ ).
--
--
--
-- /See:/ 'monthlyTransfer' smart constructor.
newtype MonthlyTransfer = MonthlyTransfer'
    { _mtGbPerMonthAllocated :: Maybe Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MonthlyTransfer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtGbPerMonthAllocated' - The amount allocated per month (in GB).
monthlyTransfer
    :: MonthlyTransfer
monthlyTransfer =
    MonthlyTransfer'
    { _mtGbPerMonthAllocated = Nothing
    }

-- | The amount allocated per month (in GB).
mtGbPerMonthAllocated :: Lens' MonthlyTransfer (Maybe Int)
mtGbPerMonthAllocated = lens _mtGbPerMonthAllocated (\ s a -> s{_mtGbPerMonthAllocated = a});

instance FromJSON MonthlyTransfer where
        parseJSON
          = withObject "MonthlyTransfer"
              (\ x ->
                 MonthlyTransfer' <$> (x .:? "gbPerMonthAllocated"))

instance Hashable MonthlyTransfer

instance NFData MonthlyTransfer

-- | Describes the API operation.
--
--
--
-- /See:/ 'operation' smart constructor.
data Operation = Operation'
    { _oStatus           :: !(Maybe OperationStatus)
    , _oOperationDetails :: !(Maybe Text)
    , _oResourceType     :: !(Maybe ResourceType)
    , _oCreatedAt        :: !(Maybe POSIX)
    , _oResourceName     :: !(Maybe Text)
    , _oLocation         :: !(Maybe ResourceLocation)
    , _oStatusChangedAt  :: !(Maybe POSIX)
    , _oErrorDetails     :: !(Maybe Text)
    , _oErrorCode        :: !(Maybe Text)
    , _oId               :: !(Maybe Text)
    , _oOperationType    :: !(Maybe OperationType)
    , _oIsTerminal       :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Operation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oStatus' - The status of the operation.
--
-- * 'oOperationDetails' - Details about the operation (e.g., @Debian-1GB-Virginia-1@ ).
--
-- * 'oResourceType' - The resource type.
--
-- * 'oCreatedAt' - The timestamp when the operation was initialized (e.g., @1479816991.349@ ).
--
-- * 'oResourceName' - The resource name.
--
-- * 'oLocation' - The region and Availability Zone.
--
-- * 'oStatusChangedAt' - The timestamp when the status was changed (e.g., @1479816991.349@ ).
--
-- * 'oErrorDetails' - The error details.
--
-- * 'oErrorCode' - The error code.
--
-- * 'oId' - The ID of the operation.
--
-- * 'oOperationType' - The type of operation.
--
-- * 'oIsTerminal' - A Boolean value indicating whether the operation is terminal.
operation
    :: Operation
operation =
    Operation'
    { _oStatus = Nothing
    , _oOperationDetails = Nothing
    , _oResourceType = Nothing
    , _oCreatedAt = Nothing
    , _oResourceName = Nothing
    , _oLocation = Nothing
    , _oStatusChangedAt = Nothing
    , _oErrorDetails = Nothing
    , _oErrorCode = Nothing
    , _oId = Nothing
    , _oOperationType = Nothing
    , _oIsTerminal = Nothing
    }

-- | The status of the operation.
oStatus :: Lens' Operation (Maybe OperationStatus)
oStatus = lens _oStatus (\ s a -> s{_oStatus = a});

-- | Details about the operation (e.g., @Debian-1GB-Virginia-1@ ).
oOperationDetails :: Lens' Operation (Maybe Text)
oOperationDetails = lens _oOperationDetails (\ s a -> s{_oOperationDetails = a});

-- | The resource type.
oResourceType :: Lens' Operation (Maybe ResourceType)
oResourceType = lens _oResourceType (\ s a -> s{_oResourceType = a});

-- | The timestamp when the operation was initialized (e.g., @1479816991.349@ ).
oCreatedAt :: Lens' Operation (Maybe UTCTime)
oCreatedAt = lens _oCreatedAt (\ s a -> s{_oCreatedAt = a}) . mapping _Time;

-- | The resource name.
oResourceName :: Lens' Operation (Maybe Text)
oResourceName = lens _oResourceName (\ s a -> s{_oResourceName = a});

-- | The region and Availability Zone.
oLocation :: Lens' Operation (Maybe ResourceLocation)
oLocation = lens _oLocation (\ s a -> s{_oLocation = a});

-- | The timestamp when the status was changed (e.g., @1479816991.349@ ).
oStatusChangedAt :: Lens' Operation (Maybe UTCTime)
oStatusChangedAt = lens _oStatusChangedAt (\ s a -> s{_oStatusChangedAt = a}) . mapping _Time;

-- | The error details.
oErrorDetails :: Lens' Operation (Maybe Text)
oErrorDetails = lens _oErrorDetails (\ s a -> s{_oErrorDetails = a});

-- | The error code.
oErrorCode :: Lens' Operation (Maybe Text)
oErrorCode = lens _oErrorCode (\ s a -> s{_oErrorCode = a});

-- | The ID of the operation.
oId :: Lens' Operation (Maybe Text)
oId = lens _oId (\ s a -> s{_oId = a});

-- | The type of operation.
oOperationType :: Lens' Operation (Maybe OperationType)
oOperationType = lens _oOperationType (\ s a -> s{_oOperationType = a});

-- | A Boolean value indicating whether the operation is terminal.
oIsTerminal :: Lens' Operation (Maybe Bool)
oIsTerminal = lens _oIsTerminal (\ s a -> s{_oIsTerminal = a});

instance FromJSON Operation where
        parseJSON
          = withObject "Operation"
              (\ x ->
                 Operation' <$>
                   (x .:? "status") <*> (x .:? "operationDetails") <*>
                     (x .:? "resourceType")
                     <*> (x .:? "createdAt")
                     <*> (x .:? "resourceName")
                     <*> (x .:? "location")
                     <*> (x .:? "statusChangedAt")
                     <*> (x .:? "errorDetails")
                     <*> (x .:? "errorCode")
                     <*> (x .:? "id")
                     <*> (x .:? "operationType")
                     <*> (x .:? "isTerminal"))

instance Hashable Operation

instance NFData Operation

-- | Describes information about the ports on your virtual private server (or /instance/ ).
--
--
--
-- /See:/ 'portInfo' smart constructor.
data PortInfo = PortInfo'
    { _piFromPort :: !(Maybe Nat)
    , _piProtocol :: !(Maybe NetworkProtocol)
    , _piToPort   :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PortInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piFromPort' - The first port in the range.
--
-- * 'piProtocol' - The protocol.
--
-- * 'piToPort' - The last port in the range.
portInfo
    :: PortInfo
portInfo =
    PortInfo'
    { _piFromPort = Nothing
    , _piProtocol = Nothing
    , _piToPort = Nothing
    }

-- | The first port in the range.
piFromPort :: Lens' PortInfo (Maybe Natural)
piFromPort = lens _piFromPort (\ s a -> s{_piFromPort = a}) . mapping _Nat;

-- | The protocol.
piProtocol :: Lens' PortInfo (Maybe NetworkProtocol)
piProtocol = lens _piProtocol (\ s a -> s{_piProtocol = a});

-- | The last port in the range.
piToPort :: Lens' PortInfo (Maybe Natural)
piToPort = lens _piToPort (\ s a -> s{_piToPort = a}) . mapping _Nat;

instance Hashable PortInfo

instance NFData PortInfo

instance ToJSON PortInfo where
        toJSON PortInfo'{..}
          = object
              (catMaybes
                 [("fromPort" .=) <$> _piFromPort,
                  ("protocol" .=) <$> _piProtocol,
                  ("toPort" .=) <$> _piToPort])

-- | Describes the AWS Region.
--
--
--
-- /See:/ 'regionInfo' smart constructor.
data RegionInfo = RegionInfo'
    { _riAvailabilityZones :: !(Maybe [AvailabilityZone])
    , _riName              :: !(Maybe RegionName)
    , _riDisplayName       :: !(Maybe Text)
    , _riContinentCode     :: !(Maybe Text)
    , _riDescription       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RegionInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riAvailabilityZones' - The Availability Zones. Follows the format @us-east-1a@ (case-sensitive).
--
-- * 'riName' - The region name (e.g., @us-east-1@ ).
--
-- * 'riDisplayName' - The display name (e.g., @Virginia@ ).
--
-- * 'riContinentCode' - The continent code (e.g., @NA@ , meaning North America).
--
-- * 'riDescription' - The description of the AWS Region (e.g., @This region is recommended to serve users in the eastern United States and eastern Canada@ ).
regionInfo
    :: RegionInfo
regionInfo =
    RegionInfo'
    { _riAvailabilityZones = Nothing
    , _riName = Nothing
    , _riDisplayName = Nothing
    , _riContinentCode = Nothing
    , _riDescription = Nothing
    }

-- | The Availability Zones. Follows the format @us-east-1a@ (case-sensitive).
riAvailabilityZones :: Lens' RegionInfo [AvailabilityZone]
riAvailabilityZones = lens _riAvailabilityZones (\ s a -> s{_riAvailabilityZones = a}) . _Default . _Coerce;

-- | The region name (e.g., @us-east-1@ ).
riName :: Lens' RegionInfo (Maybe RegionName)
riName = lens _riName (\ s a -> s{_riName = a});

-- | The display name (e.g., @Virginia@ ).
riDisplayName :: Lens' RegionInfo (Maybe Text)
riDisplayName = lens _riDisplayName (\ s a -> s{_riDisplayName = a});

-- | The continent code (e.g., @NA@ , meaning North America).
riContinentCode :: Lens' RegionInfo (Maybe Text)
riContinentCode = lens _riContinentCode (\ s a -> s{_riContinentCode = a});

-- | The description of the AWS Region (e.g., @This region is recommended to serve users in the eastern United States and eastern Canada@ ).
riDescription :: Lens' RegionInfo (Maybe Text)
riDescription = lens _riDescription (\ s a -> s{_riDescription = a});

instance FromJSON RegionInfo where
        parseJSON
          = withObject "RegionInfo"
              (\ x ->
                 RegionInfo' <$>
                   (x .:? "availabilityZones" .!= mempty) <*>
                     (x .:? "name")
                     <*> (x .:? "displayName")
                     <*> (x .:? "continentCode")
                     <*> (x .:? "description"))

instance Hashable RegionInfo

instance NFData RegionInfo

-- | Describes the resource location.
--
--
--
-- /See:/ 'resourceLocation' smart constructor.
data ResourceLocation = ResourceLocation'
    { _rlRegionName       :: !(Maybe RegionName)
    , _rlAvailabilityZone :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ResourceLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rlRegionName' - The AWS Region name.
--
-- * 'rlAvailabilityZone' - The Availability Zone. Follows the format @us-east-1a@ (case-sensitive).
resourceLocation
    :: ResourceLocation
resourceLocation =
    ResourceLocation'
    { _rlRegionName = Nothing
    , _rlAvailabilityZone = Nothing
    }

-- | The AWS Region name.
rlRegionName :: Lens' ResourceLocation (Maybe RegionName)
rlRegionName = lens _rlRegionName (\ s a -> s{_rlRegionName = a});

-- | The Availability Zone. Follows the format @us-east-1a@ (case-sensitive).
rlAvailabilityZone :: Lens' ResourceLocation (Maybe Text)
rlAvailabilityZone = lens _rlAvailabilityZone (\ s a -> s{_rlAvailabilityZone = a});

instance FromJSON ResourceLocation where
        parseJSON
          = withObject "ResourceLocation"
              (\ x ->
                 ResourceLocation' <$>
                   (x .:? "regionName") <*> (x .:? "availabilityZone"))

instance Hashable ResourceLocation

instance NFData ResourceLocation

-- | Describes the static IP.
--
--
--
-- /See:/ 'staticIP' smart constructor.
data StaticIP = StaticIP'
    { _siIpAddress    :: !(Maybe Text)
    , _siResourceType :: !(Maybe ResourceType)
    , _siArn          :: !(Maybe Text)
    , _siCreatedAt    :: !(Maybe POSIX)
    , _siLocation     :: !(Maybe ResourceLocation)
    , _siIsAttached   :: !(Maybe Bool)
    , _siName         :: !(Maybe Text)
    , _siSupportCode  :: !(Maybe Text)
    , _siAttachedTo   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StaticIP' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siIpAddress' - The static IP address.
--
-- * 'siResourceType' - The resource type (usually @StaticIp@ ).
--
-- * 'siArn' - The Amazon Resource Name (ARN) of the static IP (e.g., @arn:aws:lightsail:us-east-1:123456789101:StaticIp/9cbb4a9e-f8e3-4dfe-b57e-12345EXAMPLE@ ).
--
-- * 'siCreatedAt' - The timestamp when the static IP was created (e.g., @1479735304.222@ ).
--
-- * 'siLocation' - The region and Availability Zone where the static IP was created.
--
-- * 'siIsAttached' - A Boolean value indicating whether the static IP is attached.
--
-- * 'siName' - The name of the static IP (e.g., @StaticIP-Virginia-EXAMPLE@ ).
--
-- * 'siSupportCode' - The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- * 'siAttachedTo' - The instance where the static IP is attached (e.g., @Amazon_Linux-1GB-Virginia-1@ ).
staticIP
    :: StaticIP
staticIP =
    StaticIP'
    { _siIpAddress = Nothing
    , _siResourceType = Nothing
    , _siArn = Nothing
    , _siCreatedAt = Nothing
    , _siLocation = Nothing
    , _siIsAttached = Nothing
    , _siName = Nothing
    , _siSupportCode = Nothing
    , _siAttachedTo = Nothing
    }

-- | The static IP address.
siIpAddress :: Lens' StaticIP (Maybe Text)
siIpAddress = lens _siIpAddress (\ s a -> s{_siIpAddress = a});

-- | The resource type (usually @StaticIp@ ).
siResourceType :: Lens' StaticIP (Maybe ResourceType)
siResourceType = lens _siResourceType (\ s a -> s{_siResourceType = a});

-- | The Amazon Resource Name (ARN) of the static IP (e.g., @arn:aws:lightsail:us-east-1:123456789101:StaticIp/9cbb4a9e-f8e3-4dfe-b57e-12345EXAMPLE@ ).
siArn :: Lens' StaticIP (Maybe Text)
siArn = lens _siArn (\ s a -> s{_siArn = a});

-- | The timestamp when the static IP was created (e.g., @1479735304.222@ ).
siCreatedAt :: Lens' StaticIP (Maybe UTCTime)
siCreatedAt = lens _siCreatedAt (\ s a -> s{_siCreatedAt = a}) . mapping _Time;

-- | The region and Availability Zone where the static IP was created.
siLocation :: Lens' StaticIP (Maybe ResourceLocation)
siLocation = lens _siLocation (\ s a -> s{_siLocation = a});

-- | A Boolean value indicating whether the static IP is attached.
siIsAttached :: Lens' StaticIP (Maybe Bool)
siIsAttached = lens _siIsAttached (\ s a -> s{_siIsAttached = a});

-- | The name of the static IP (e.g., @StaticIP-Virginia-EXAMPLE@ ).
siName :: Lens' StaticIP (Maybe Text)
siName = lens _siName (\ s a -> s{_siName = a});

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
siSupportCode :: Lens' StaticIP (Maybe Text)
siSupportCode = lens _siSupportCode (\ s a -> s{_siSupportCode = a});

-- | The instance where the static IP is attached (e.g., @Amazon_Linux-1GB-Virginia-1@ ).
siAttachedTo :: Lens' StaticIP (Maybe Text)
siAttachedTo = lens _siAttachedTo (\ s a -> s{_siAttachedTo = a});

instance FromJSON StaticIP where
        parseJSON
          = withObject "StaticIP"
              (\ x ->
                 StaticIP' <$>
                   (x .:? "ipAddress") <*> (x .:? "resourceType") <*>
                     (x .:? "arn")
                     <*> (x .:? "createdAt")
                     <*> (x .:? "location")
                     <*> (x .:? "isAttached")
                     <*> (x .:? "name")
                     <*> (x .:? "supportCode")
                     <*> (x .:? "attachedTo"))

instance Hashable StaticIP

instance NFData StaticIP
