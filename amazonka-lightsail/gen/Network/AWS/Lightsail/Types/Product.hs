{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.Product where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.Sum
import Network.AWS.Prelude

-- | Describes an Availability Zone.
--
--
--
-- /See:/ 'availabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'
  { _azState    :: !(Maybe Text)
  , _azZoneName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AvailabilityZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'azState' - The state of the Availability Zone.
--
-- * 'azZoneName' - The name of the Availability Zone. The format is @us-east-2a@ (case-sensitive).
availabilityZone
    :: AvailabilityZone
availabilityZone = AvailabilityZone' {_azState = Nothing, _azZoneName = Nothing}


-- | The state of the Availability Zone.
azState :: Lens' AvailabilityZone (Maybe Text)
azState = lens _azState (\ s a -> s{_azState = a})

-- | The name of the Availability Zone. The format is @us-east-2a@ (case-sensitive).
azZoneName :: Lens' AvailabilityZone (Maybe Text)
azZoneName = lens _azZoneName (\ s a -> s{_azZoneName = a})

instance FromJSON AvailabilityZone where
        parseJSON
          = withObject "AvailabilityZone"
              (\ x ->
                 AvailabilityZone' <$>
                   (x .:? "state") <*> (x .:? "zoneName"))

instance Hashable AvailabilityZone where

instance NFData AvailabilityZone where

-- | Describes a blueprint (a virtual private server image).
--
--
--
-- /See:/ 'blueprint' smart constructor.
data Blueprint = Blueprint'
  { _bVersionCode :: !(Maybe Text)
  , _bPlatform    :: !(Maybe InstancePlatform)
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Blueprint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bVersionCode' - The version code.
--
-- * 'bPlatform' - The operating system platform (either Linux/Unix-based or Windows Server-based) of the blueprint.
--
-- * 'bGroup' - The group name of the blueprint (e.g., @amazon-linux@ ).
--
-- * 'bMinPower' - The minimum bundle power required to run this blueprint. For example, you need a bundle with a power value of 500 or more to create an instance that uses a blueprint with a minimum power value of 500. @0@ indicates that the blueprint runs on all instance sizes.
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
-- * 'bIsActive' - A Boolean value indicating whether the blueprint is active. Inactive blueprints are listed to support customers with existing instances but are not necessarily available for launch of new instances. Blueprints are marked inactive when they become outdated due to operating system updates or new application releases.
--
-- * 'bDescription' - The description of the blueprint.
blueprint
    :: Blueprint
blueprint =
  Blueprint'
    { _bVersionCode = Nothing
    , _bPlatform = Nothing
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
bVersionCode = lens _bVersionCode (\ s a -> s{_bVersionCode = a})

-- | The operating system platform (either Linux/Unix-based or Windows Server-based) of the blueprint.
bPlatform :: Lens' Blueprint (Maybe InstancePlatform)
bPlatform = lens _bPlatform (\ s a -> s{_bPlatform = a})

-- | The group name of the blueprint (e.g., @amazon-linux@ ).
bGroup :: Lens' Blueprint (Maybe Text)
bGroup = lens _bGroup (\ s a -> s{_bGroup = a})

-- | The minimum bundle power required to run this blueprint. For example, you need a bundle with a power value of 500 or more to create an instance that uses a blueprint with a minimum power value of 500. @0@ indicates that the blueprint runs on all instance sizes.
bMinPower :: Lens' Blueprint (Maybe Int)
bMinPower = lens _bMinPower (\ s a -> s{_bMinPower = a})

-- | The product URL to learn more about the image or blueprint.
bProductURL :: Lens' Blueprint (Maybe Text)
bProductURL = lens _bProductURL (\ s a -> s{_bProductURL = a})

-- | The end-user license agreement URL for the image or blueprint.
bLicenseURL :: Lens' Blueprint (Maybe Text)
bLicenseURL = lens _bLicenseURL (\ s a -> s{_bLicenseURL = a})

-- | The friendly name of the blueprint (e.g., @Amazon Linux@ ).
bName :: Lens' Blueprint (Maybe Text)
bName = lens _bName (\ s a -> s{_bName = a})

-- | The version number of the operating system, application, or stack (e.g., @2016.03.0@ ).
bVersion :: Lens' Blueprint (Maybe Text)
bVersion = lens _bVersion (\ s a -> s{_bVersion = a})

-- | The ID for the virtual private server image (e.g., @app_wordpress_4_4@ or @app_lamp_7_0@ ).
bBlueprintId :: Lens' Blueprint (Maybe Text)
bBlueprintId = lens _bBlueprintId (\ s a -> s{_bBlueprintId = a})

-- | The type of the blueprint (e.g., @os@ or @app@ ).
bType :: Lens' Blueprint (Maybe BlueprintType)
bType = lens _bType (\ s a -> s{_bType = a})

-- | A Boolean value indicating whether the blueprint is active. Inactive blueprints are listed to support customers with existing instances but are not necessarily available for launch of new instances. Blueprints are marked inactive when they become outdated due to operating system updates or new application releases.
bIsActive :: Lens' Blueprint (Maybe Bool)
bIsActive = lens _bIsActive (\ s a -> s{_bIsActive = a})

-- | The description of the blueprint.
bDescription :: Lens' Blueprint (Maybe Text)
bDescription = lens _bDescription (\ s a -> s{_bDescription = a})

instance FromJSON Blueprint where
        parseJSON
          = withObject "Blueprint"
              (\ x ->
                 Blueprint' <$>
                   (x .:? "versionCode") <*> (x .:? "platform") <*>
                     (x .:? "group")
                     <*> (x .:? "minPower")
                     <*> (x .:? "productUrl")
                     <*> (x .:? "licenseUrl")
                     <*> (x .:? "name")
                     <*> (x .:? "version")
                     <*> (x .:? "blueprintId")
                     <*> (x .:? "type")
                     <*> (x .:? "isActive")
                     <*> (x .:? "description"))

instance Hashable Blueprint where

instance NFData Blueprint where

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
  , _bunSupportedPlatforms   :: !(Maybe [InstancePlatform])
  , _bunPrice                :: !(Maybe Double)
  , _bunIsActive             :: !(Maybe Bool)
  , _bunRamSizeInGb          :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
-- * 'bunPower' - A numeric value that represents the power of the bundle (e.g., @500@ ). You can use the bundle's power value in conjunction with a blueprint's minimum power value to determine whether the blueprint will run on the bundle. For example, you need a bundle with a power value of 500 or more to create an instance that uses a blueprint with a minimum power value of 500.
--
-- * 'bunDiskSizeInGb' - The size of the SSD (e.g., @30@ ).
--
-- * 'bunSupportedPlatforms' - The operating system platform (Linux/Unix-based or Windows Server-based) that the bundle supports. You can only launch a @WINDOWS@ bundle on a blueprint that supports the @WINDOWS@ platform. @LINUX_UNIX@ blueprints require a @LINUX_UNIX@ bundle.
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
    , _bunSupportedPlatforms = Nothing
    , _bunPrice = Nothing
    , _bunIsActive = Nothing
    , _bunRamSizeInGb = Nothing
    }


-- | The number of vCPUs included in the bundle (e.g., @2@ ).
bunCpuCount :: Lens' Bundle (Maybe Int)
bunCpuCount = lens _bunCpuCount (\ s a -> s{_bunCpuCount = a})

-- | The data transfer rate per month in GB (e.g., @2000@ ).
bunTransferPerMonthInGb :: Lens' Bundle (Maybe Int)
bunTransferPerMonthInGb = lens _bunTransferPerMonthInGb (\ s a -> s{_bunTransferPerMonthInGb = a})

-- | The bundle ID (e.g., @micro_1_0@ ).
bunBundleId :: Lens' Bundle (Maybe Text)
bunBundleId = lens _bunBundleId (\ s a -> s{_bunBundleId = a})

-- | The Amazon EC2 instance type (e.g., @t2.micro@ ).
bunInstanceType :: Lens' Bundle (Maybe Text)
bunInstanceType = lens _bunInstanceType (\ s a -> s{_bunInstanceType = a})

-- | A friendly name for the bundle (e.g., @Micro@ ).
bunName :: Lens' Bundle (Maybe Text)
bunName = lens _bunName (\ s a -> s{_bunName = a})

-- | A numeric value that represents the power of the bundle (e.g., @500@ ). You can use the bundle's power value in conjunction with a blueprint's minimum power value to determine whether the blueprint will run on the bundle. For example, you need a bundle with a power value of 500 or more to create an instance that uses a blueprint with a minimum power value of 500.
bunPower :: Lens' Bundle (Maybe Int)
bunPower = lens _bunPower (\ s a -> s{_bunPower = a})

-- | The size of the SSD (e.g., @30@ ).
bunDiskSizeInGb :: Lens' Bundle (Maybe Int)
bunDiskSizeInGb = lens _bunDiskSizeInGb (\ s a -> s{_bunDiskSizeInGb = a})

-- | The operating system platform (Linux/Unix-based or Windows Server-based) that the bundle supports. You can only launch a @WINDOWS@ bundle on a blueprint that supports the @WINDOWS@ platform. @LINUX_UNIX@ blueprints require a @LINUX_UNIX@ bundle.
bunSupportedPlatforms :: Lens' Bundle [InstancePlatform]
bunSupportedPlatforms = lens _bunSupportedPlatforms (\ s a -> s{_bunSupportedPlatforms = a}) . _Default . _Coerce

-- | The price in US dollars (e.g., @5.0@ ).
bunPrice :: Lens' Bundle (Maybe Double)
bunPrice = lens _bunPrice (\ s a -> s{_bunPrice = a})

-- | A Boolean value indicating whether the bundle is active.
bunIsActive :: Lens' Bundle (Maybe Bool)
bunIsActive = lens _bunIsActive (\ s a -> s{_bunIsActive = a})

-- | The amount of RAM in GB (e.g., @2.0@ ).
bunRamSizeInGb :: Lens' Bundle (Maybe Double)
bunRamSizeInGb = lens _bunRamSizeInGb (\ s a -> s{_bunRamSizeInGb = a})

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
                     <*> (x .:? "supportedPlatforms" .!= mempty)
                     <*> (x .:? "price")
                     <*> (x .:? "isActive")
                     <*> (x .:? "ramSizeInGb"))

instance Hashable Bundle where

instance NFData Bundle where

-- | Describes a CloudFormation stack record created as a result of the @create cloud formation stack@ operation.
--
--
-- A CloudFormation stack record provides information about the AWS CloudFormation stack used to create a new Amazon Elastic Compute Cloud instance from an exported Lightsail instance snapshot.
--
--
-- /See:/ 'cloudFormationStackRecord' smart constructor.
data CloudFormationStackRecord = CloudFormationStackRecord'
  { _cfsrState           :: !(Maybe RecordState)
  , _cfsrDestinationInfo :: !(Maybe DestinationInfo)
  , _cfsrResourceType    :: !(Maybe ResourceType)
  , _cfsrArn             :: !(Maybe Text)
  , _cfsrCreatedAt       :: !(Maybe POSIX)
  , _cfsrLocation        :: !(Maybe ResourceLocation)
  , _cfsrName            :: !(Maybe Text)
  , _cfsrSourceInfo      :: !(Maybe [CloudFormationStackRecordSourceInfo])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CloudFormationStackRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfsrState' - The current state of the CloudFormation stack record.
--
-- * 'cfsrDestinationInfo' - A list of objects describing the destination service, which is AWS CloudFormation, and the Amazon Resource Name (ARN) of the AWS CloudFormation stack.
--
-- * 'cfsrResourceType' - The Lightsail resource type (e.g., @CloudFormationStackRecord@ ).
--
-- * 'cfsrArn' - The Amazon Resource Name (ARN) of the CloudFormation stack record.
--
-- * 'cfsrCreatedAt' - The date when the CloudFormation stack record was created.
--
-- * 'cfsrLocation' - A list of objects describing the Availability Zone and AWS Region of the CloudFormation stack record.
--
-- * 'cfsrName' - The name of the CloudFormation stack record. It starts with @CloudFormationStackRecord@ followed by a GUID.
--
-- * 'cfsrSourceInfo' - A list of objects describing the source of the CloudFormation stack record.
cloudFormationStackRecord
    :: CloudFormationStackRecord
cloudFormationStackRecord =
  CloudFormationStackRecord'
    { _cfsrState = Nothing
    , _cfsrDestinationInfo = Nothing
    , _cfsrResourceType = Nothing
    , _cfsrArn = Nothing
    , _cfsrCreatedAt = Nothing
    , _cfsrLocation = Nothing
    , _cfsrName = Nothing
    , _cfsrSourceInfo = Nothing
    }


-- | The current state of the CloudFormation stack record.
cfsrState :: Lens' CloudFormationStackRecord (Maybe RecordState)
cfsrState = lens _cfsrState (\ s a -> s{_cfsrState = a})

-- | A list of objects describing the destination service, which is AWS CloudFormation, and the Amazon Resource Name (ARN) of the AWS CloudFormation stack.
cfsrDestinationInfo :: Lens' CloudFormationStackRecord (Maybe DestinationInfo)
cfsrDestinationInfo = lens _cfsrDestinationInfo (\ s a -> s{_cfsrDestinationInfo = a})

-- | The Lightsail resource type (e.g., @CloudFormationStackRecord@ ).
cfsrResourceType :: Lens' CloudFormationStackRecord (Maybe ResourceType)
cfsrResourceType = lens _cfsrResourceType (\ s a -> s{_cfsrResourceType = a})

-- | The Amazon Resource Name (ARN) of the CloudFormation stack record.
cfsrArn :: Lens' CloudFormationStackRecord (Maybe Text)
cfsrArn = lens _cfsrArn (\ s a -> s{_cfsrArn = a})

-- | The date when the CloudFormation stack record was created.
cfsrCreatedAt :: Lens' CloudFormationStackRecord (Maybe UTCTime)
cfsrCreatedAt = lens _cfsrCreatedAt (\ s a -> s{_cfsrCreatedAt = a}) . mapping _Time

-- | A list of objects describing the Availability Zone and AWS Region of the CloudFormation stack record.
cfsrLocation :: Lens' CloudFormationStackRecord (Maybe ResourceLocation)
cfsrLocation = lens _cfsrLocation (\ s a -> s{_cfsrLocation = a})

-- | The name of the CloudFormation stack record. It starts with @CloudFormationStackRecord@ followed by a GUID.
cfsrName :: Lens' CloudFormationStackRecord (Maybe Text)
cfsrName = lens _cfsrName (\ s a -> s{_cfsrName = a})

-- | A list of objects describing the source of the CloudFormation stack record.
cfsrSourceInfo :: Lens' CloudFormationStackRecord [CloudFormationStackRecordSourceInfo]
cfsrSourceInfo = lens _cfsrSourceInfo (\ s a -> s{_cfsrSourceInfo = a}) . _Default . _Coerce

instance FromJSON CloudFormationStackRecord where
        parseJSON
          = withObject "CloudFormationStackRecord"
              (\ x ->
                 CloudFormationStackRecord' <$>
                   (x .:? "state") <*> (x .:? "destinationInfo") <*>
                     (x .:? "resourceType")
                     <*> (x .:? "arn")
                     <*> (x .:? "createdAt")
                     <*> (x .:? "location")
                     <*> (x .:? "name")
                     <*> (x .:? "sourceInfo" .!= mempty))

instance Hashable CloudFormationStackRecord where

instance NFData CloudFormationStackRecord where

-- | Describes the source of a CloudFormation stack record (i.e., the export snapshot record).
--
--
--
-- /See:/ 'cloudFormationStackRecordSourceInfo' smart constructor.
data CloudFormationStackRecordSourceInfo = CloudFormationStackRecordSourceInfo'
  { _cfsrsiResourceType :: !(Maybe CloudFormationStackRecordSourceType)
  , _cfsrsiArn          :: !(Maybe Text)
  , _cfsrsiName         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CloudFormationStackRecordSourceInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfsrsiResourceType' - The Lightsail resource type (e.g., @ExportSnapshotRecord@ ).
--
-- * 'cfsrsiArn' - The Amazon Resource Name (ARN) of the export snapshot record.
--
-- * 'cfsrsiName' - The name of the record.
cloudFormationStackRecordSourceInfo
    :: CloudFormationStackRecordSourceInfo
cloudFormationStackRecordSourceInfo =
  CloudFormationStackRecordSourceInfo'
    {_cfsrsiResourceType = Nothing, _cfsrsiArn = Nothing, _cfsrsiName = Nothing}


-- | The Lightsail resource type (e.g., @ExportSnapshotRecord@ ).
cfsrsiResourceType :: Lens' CloudFormationStackRecordSourceInfo (Maybe CloudFormationStackRecordSourceType)
cfsrsiResourceType = lens _cfsrsiResourceType (\ s a -> s{_cfsrsiResourceType = a})

-- | The Amazon Resource Name (ARN) of the export snapshot record.
cfsrsiArn :: Lens' CloudFormationStackRecordSourceInfo (Maybe Text)
cfsrsiArn = lens _cfsrsiArn (\ s a -> s{_cfsrsiArn = a})

-- | The name of the record.
cfsrsiName :: Lens' CloudFormationStackRecordSourceInfo (Maybe Text)
cfsrsiName = lens _cfsrsiName (\ s a -> s{_cfsrsiName = a})

instance FromJSON CloudFormationStackRecordSourceInfo
         where
        parseJSON
          = withObject "CloudFormationStackRecordSourceInfo"
              (\ x ->
                 CloudFormationStackRecordSourceInfo' <$>
                   (x .:? "resourceType") <*> (x .:? "arn") <*>
                     (x .:? "name"))

instance Hashable CloudFormationStackRecordSourceInfo
         where

instance NFData CloudFormationStackRecordSourceInfo
         where

-- | Describes the destination of a record.
--
--
--
-- /See:/ 'destinationInfo' smart constructor.
data DestinationInfo = DestinationInfo'
  { _diService :: !(Maybe Text)
  , _diId      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DestinationInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diService' - The destination service of the record.
--
-- * 'diId' - The ID of the resource created at the destination.
destinationInfo
    :: DestinationInfo
destinationInfo = DestinationInfo' {_diService = Nothing, _diId = Nothing}


-- | The destination service of the record.
diService :: Lens' DestinationInfo (Maybe Text)
diService = lens _diService (\ s a -> s{_diService = a})

-- | The ID of the resource created at the destination.
diId :: Lens' DestinationInfo (Maybe Text)
diId = lens _diId (\ s a -> s{_diId = a})

instance FromJSON DestinationInfo where
        parseJSON
          = withObject "DestinationInfo"
              (\ x ->
                 DestinationInfo' <$>
                   (x .:? "service") <*> (x .:? "id"))

instance Hashable DestinationInfo where

instance NFData DestinationInfo where

-- | Describes a system disk or an block storage disk.
--
--
--
-- /See:/ 'disk' smart constructor.
data Disk = Disk'
  { _dState           :: !(Maybe DiskState)
  , _dResourceType    :: !(Maybe ResourceType)
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
  , _dTags            :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Disk' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dState' - Describes the status of the disk.
--
-- * 'dResourceType' - The Lightsail resource type (e.g., @Disk@ ).
--
-- * 'dArn' - The Amazon Resource Name (ARN) of the disk.
--
-- * 'dPath' - The disk path.
--
-- * 'dCreatedAt' - The date when the disk was created.
--
-- * 'dLocation' - The AWS Region and Availability Zone where the disk is located.
--
-- * 'dIops' - The input/output operations per second (IOPS) of the disk.
--
-- * 'dIsAttached' - A Boolean value indicating whether the disk is attached.
--
-- * 'dAttachmentState' - (Deprecated) The attachment state of the disk.
--
-- * 'dName' - The unique name of the disk.
--
-- * 'dSizeInGb' - The size of the disk in GB.
--
-- * 'dSupportCode' - The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- * 'dIsSystemDisk' - A Boolean value indicating whether this disk is a system disk (has an operating system loaded on it).
--
-- * 'dAttachedTo' - The resources to which the disk is attached.
--
-- * 'dGbInUse' - (Deprecated) The number of GB in use by the disk.
--
-- * 'dTags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
disk
    :: Disk
disk =
  Disk'
    { _dState = Nothing
    , _dResourceType = Nothing
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
    , _dTags = Nothing
    }


-- | Describes the status of the disk.
dState :: Lens' Disk (Maybe DiskState)
dState = lens _dState (\ s a -> s{_dState = a})

-- | The Lightsail resource type (e.g., @Disk@ ).
dResourceType :: Lens' Disk (Maybe ResourceType)
dResourceType = lens _dResourceType (\ s a -> s{_dResourceType = a})

-- | The Amazon Resource Name (ARN) of the disk.
dArn :: Lens' Disk (Maybe Text)
dArn = lens _dArn (\ s a -> s{_dArn = a})

-- | The disk path.
dPath :: Lens' Disk (Maybe Text)
dPath = lens _dPath (\ s a -> s{_dPath = a})

-- | The date when the disk was created.
dCreatedAt :: Lens' Disk (Maybe UTCTime)
dCreatedAt = lens _dCreatedAt (\ s a -> s{_dCreatedAt = a}) . mapping _Time

-- | The AWS Region and Availability Zone where the disk is located.
dLocation :: Lens' Disk (Maybe ResourceLocation)
dLocation = lens _dLocation (\ s a -> s{_dLocation = a})

-- | The input/output operations per second (IOPS) of the disk.
dIops :: Lens' Disk (Maybe Int)
dIops = lens _dIops (\ s a -> s{_dIops = a})

-- | A Boolean value indicating whether the disk is attached.
dIsAttached :: Lens' Disk (Maybe Bool)
dIsAttached = lens _dIsAttached (\ s a -> s{_dIsAttached = a})

-- | (Deprecated) The attachment state of the disk.
dAttachmentState :: Lens' Disk (Maybe Text)
dAttachmentState = lens _dAttachmentState (\ s a -> s{_dAttachmentState = a})

-- | The unique name of the disk.
dName :: Lens' Disk (Maybe Text)
dName = lens _dName (\ s a -> s{_dName = a})

-- | The size of the disk in GB.
dSizeInGb :: Lens' Disk (Maybe Int)
dSizeInGb = lens _dSizeInGb (\ s a -> s{_dSizeInGb = a})

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
dSupportCode :: Lens' Disk (Maybe Text)
dSupportCode = lens _dSupportCode (\ s a -> s{_dSupportCode = a})

-- | A Boolean value indicating whether this disk is a system disk (has an operating system loaded on it).
dIsSystemDisk :: Lens' Disk (Maybe Bool)
dIsSystemDisk = lens _dIsSystemDisk (\ s a -> s{_dIsSystemDisk = a})

-- | The resources to which the disk is attached.
dAttachedTo :: Lens' Disk (Maybe Text)
dAttachedTo = lens _dAttachedTo (\ s a -> s{_dAttachedTo = a})

-- | (Deprecated) The number of GB in use by the disk.
dGbInUse :: Lens' Disk (Maybe Int)
dGbInUse = lens _dGbInUse (\ s a -> s{_dGbInUse = a})

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
dTags :: Lens' Disk [Tag]
dTags = lens _dTags (\ s a -> s{_dTags = a}) . _Default . _Coerce

instance FromJSON Disk where
        parseJSON
          = withObject "Disk"
              (\ x ->
                 Disk' <$>
                   (x .:? "state") <*> (x .:? "resourceType") <*>
                     (x .:? "arn")
                     <*> (x .:? "path")
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
                     <*> (x .:? "gbInUse")
                     <*> (x .:? "tags" .!= mempty))

instance Hashable Disk where

instance NFData Disk where

-- | Describes a disk.
--
--
--
-- /See:/ 'diskInfo' smart constructor.
data DiskInfo = DiskInfo'
  { _diPath         :: !(Maybe Text)
  , _diName         :: !(Maybe Text)
  , _diSizeInGb     :: !(Maybe Int)
  , _diIsSystemDisk :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DiskInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diPath' - The disk path.
--
-- * 'diName' - The disk name.
--
-- * 'diSizeInGb' - The size of the disk in GB (e.g., @32@ ).
--
-- * 'diIsSystemDisk' - A Boolean value indicating whether this disk is a system disk (has an operating system loaded on it).
diskInfo
    :: DiskInfo
diskInfo =
  DiskInfo'
    { _diPath = Nothing
    , _diName = Nothing
    , _diSizeInGb = Nothing
    , _diIsSystemDisk = Nothing
    }


-- | The disk path.
diPath :: Lens' DiskInfo (Maybe Text)
diPath = lens _diPath (\ s a -> s{_diPath = a})

-- | The disk name.
diName :: Lens' DiskInfo (Maybe Text)
diName = lens _diName (\ s a -> s{_diName = a})

-- | The size of the disk in GB (e.g., @32@ ).
diSizeInGb :: Lens' DiskInfo (Maybe Int)
diSizeInGb = lens _diSizeInGb (\ s a -> s{_diSizeInGb = a})

-- | A Boolean value indicating whether this disk is a system disk (has an operating system loaded on it).
diIsSystemDisk :: Lens' DiskInfo (Maybe Bool)
diIsSystemDisk = lens _diIsSystemDisk (\ s a -> s{_diIsSystemDisk = a})

instance FromJSON DiskInfo where
        parseJSON
          = withObject "DiskInfo"
              (\ x ->
                 DiskInfo' <$>
                   (x .:? "path") <*> (x .:? "name") <*>
                     (x .:? "sizeInGb")
                     <*> (x .:? "isSystemDisk"))

instance Hashable DiskInfo where

instance NFData DiskInfo where

-- | Describes a block storage disk mapping.
--
--
--
-- /See:/ 'diskMap' smart constructor.
data DiskMap = DiskMap'
  { _dmNewDiskName      :: !(Maybe Text)
  , _dmOriginalDiskPath :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DiskMap' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmNewDiskName' - The new disk name (e.g., @my-new-disk@ ).
--
-- * 'dmOriginalDiskPath' - The original disk path exposed to the instance (for example, @/dev/sdh@ ).
diskMap
    :: DiskMap
diskMap = DiskMap' {_dmNewDiskName = Nothing, _dmOriginalDiskPath = Nothing}


-- | The new disk name (e.g., @my-new-disk@ ).
dmNewDiskName :: Lens' DiskMap (Maybe Text)
dmNewDiskName = lens _dmNewDiskName (\ s a -> s{_dmNewDiskName = a})

-- | The original disk path exposed to the instance (for example, @/dev/sdh@ ).
dmOriginalDiskPath :: Lens' DiskMap (Maybe Text)
dmOriginalDiskPath = lens _dmOriginalDiskPath (\ s a -> s{_dmOriginalDiskPath = a})

instance Hashable DiskMap where

instance NFData DiskMap where

instance ToJSON DiskMap where
        toJSON DiskMap'{..}
          = object
              (catMaybes
                 [("newDiskName" .=) <$> _dmNewDiskName,
                  ("originalDiskPath" .=) <$> _dmOriginalDiskPath])

-- | Describes a block storage disk snapshot.
--
--
--
-- /See:/ 'diskSnapshot' smart constructor.
data DiskSnapshot = DiskSnapshot'
  { _dsFromDiskName     :: !(Maybe Text)
  , _dsState            :: !(Maybe DiskSnapshotState)
  , _dsResourceType     :: !(Maybe ResourceType)
  , _dsArn              :: !(Maybe Text)
  , _dsCreatedAt        :: !(Maybe POSIX)
  , _dsLocation         :: !(Maybe ResourceLocation)
  , _dsProgress         :: !(Maybe Text)
  , _dsName             :: !(Maybe Text)
  , _dsSizeInGb         :: !(Maybe Int)
  , _dsSupportCode      :: !(Maybe Text)
  , _dsFromInstanceARN  :: !(Maybe Text)
  , _dsFromInstanceName :: !(Maybe Text)
  , _dsFromDiskARN      :: !(Maybe Text)
  , _dsTags             :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DiskSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsFromDiskName' - The unique name of the source disk from which the disk snapshot was created.
--
-- * 'dsState' - The status of the disk snapshot operation.
--
-- * 'dsResourceType' - The Lightsail resource type (e.g., @DiskSnapshot@ ).
--
-- * 'dsArn' - The Amazon Resource Name (ARN) of the disk snapshot.
--
-- * 'dsCreatedAt' - The date when the disk snapshot was created.
--
-- * 'dsLocation' - The AWS Region and Availability Zone where the disk snapshot was created.
--
-- * 'dsProgress' - The progress of the disk snapshot operation.
--
-- * 'dsName' - The name of the disk snapshot (e.g., @my-disk-snapshot@ ).
--
-- * 'dsSizeInGb' - The size of the disk in GB.
--
-- * 'dsSupportCode' - The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- * 'dsFromInstanceARN' - The Amazon Resource Name (ARN) of the source instance from which the disk (system volume) snapshot was created.
--
-- * 'dsFromInstanceName' - The unique name of the source instance from which the disk (system volume) snapshot was created.
--
-- * 'dsFromDiskARN' - The Amazon Resource Name (ARN) of the source disk from which the disk snapshot was created.
--
-- * 'dsTags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
diskSnapshot
    :: DiskSnapshot
diskSnapshot =
  DiskSnapshot'
    { _dsFromDiskName = Nothing
    , _dsState = Nothing
    , _dsResourceType = Nothing
    , _dsArn = Nothing
    , _dsCreatedAt = Nothing
    , _dsLocation = Nothing
    , _dsProgress = Nothing
    , _dsName = Nothing
    , _dsSizeInGb = Nothing
    , _dsSupportCode = Nothing
    , _dsFromInstanceARN = Nothing
    , _dsFromInstanceName = Nothing
    , _dsFromDiskARN = Nothing
    , _dsTags = Nothing
    }


-- | The unique name of the source disk from which the disk snapshot was created.
dsFromDiskName :: Lens' DiskSnapshot (Maybe Text)
dsFromDiskName = lens _dsFromDiskName (\ s a -> s{_dsFromDiskName = a})

-- | The status of the disk snapshot operation.
dsState :: Lens' DiskSnapshot (Maybe DiskSnapshotState)
dsState = lens _dsState (\ s a -> s{_dsState = a})

-- | The Lightsail resource type (e.g., @DiskSnapshot@ ).
dsResourceType :: Lens' DiskSnapshot (Maybe ResourceType)
dsResourceType = lens _dsResourceType (\ s a -> s{_dsResourceType = a})

-- | The Amazon Resource Name (ARN) of the disk snapshot.
dsArn :: Lens' DiskSnapshot (Maybe Text)
dsArn = lens _dsArn (\ s a -> s{_dsArn = a})

-- | The date when the disk snapshot was created.
dsCreatedAt :: Lens' DiskSnapshot (Maybe UTCTime)
dsCreatedAt = lens _dsCreatedAt (\ s a -> s{_dsCreatedAt = a}) . mapping _Time

-- | The AWS Region and Availability Zone where the disk snapshot was created.
dsLocation :: Lens' DiskSnapshot (Maybe ResourceLocation)
dsLocation = lens _dsLocation (\ s a -> s{_dsLocation = a})

-- | The progress of the disk snapshot operation.
dsProgress :: Lens' DiskSnapshot (Maybe Text)
dsProgress = lens _dsProgress (\ s a -> s{_dsProgress = a})

-- | The name of the disk snapshot (e.g., @my-disk-snapshot@ ).
dsName :: Lens' DiskSnapshot (Maybe Text)
dsName = lens _dsName (\ s a -> s{_dsName = a})

-- | The size of the disk in GB.
dsSizeInGb :: Lens' DiskSnapshot (Maybe Int)
dsSizeInGb = lens _dsSizeInGb (\ s a -> s{_dsSizeInGb = a})

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
dsSupportCode :: Lens' DiskSnapshot (Maybe Text)
dsSupportCode = lens _dsSupportCode (\ s a -> s{_dsSupportCode = a})

-- | The Amazon Resource Name (ARN) of the source instance from which the disk (system volume) snapshot was created.
dsFromInstanceARN :: Lens' DiskSnapshot (Maybe Text)
dsFromInstanceARN = lens _dsFromInstanceARN (\ s a -> s{_dsFromInstanceARN = a})

-- | The unique name of the source instance from which the disk (system volume) snapshot was created.
dsFromInstanceName :: Lens' DiskSnapshot (Maybe Text)
dsFromInstanceName = lens _dsFromInstanceName (\ s a -> s{_dsFromInstanceName = a})

-- | The Amazon Resource Name (ARN) of the source disk from which the disk snapshot was created.
dsFromDiskARN :: Lens' DiskSnapshot (Maybe Text)
dsFromDiskARN = lens _dsFromDiskARN (\ s a -> s{_dsFromDiskARN = a})

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
dsTags :: Lens' DiskSnapshot [Tag]
dsTags = lens _dsTags (\ s a -> s{_dsTags = a}) . _Default . _Coerce

instance FromJSON DiskSnapshot where
        parseJSON
          = withObject "DiskSnapshot"
              (\ x ->
                 DiskSnapshot' <$>
                   (x .:? "fromDiskName") <*> (x .:? "state") <*>
                     (x .:? "resourceType")
                     <*> (x .:? "arn")
                     <*> (x .:? "createdAt")
                     <*> (x .:? "location")
                     <*> (x .:? "progress")
                     <*> (x .:? "name")
                     <*> (x .:? "sizeInGb")
                     <*> (x .:? "supportCode")
                     <*> (x .:? "fromInstanceArn")
                     <*> (x .:? "fromInstanceName")
                     <*> (x .:? "fromDiskArn")
                     <*> (x .:? "tags" .!= mempty))

instance Hashable DiskSnapshot where

instance NFData DiskSnapshot where

-- | Describes a disk snapshot.
--
--
--
-- /See:/ 'diskSnapshotInfo' smart constructor.
newtype DiskSnapshotInfo = DiskSnapshotInfo'
  { _dsiSizeInGb :: Maybe Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DiskSnapshotInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsiSizeInGb' - The size of the disk in GB (e.g., @32@ ).
diskSnapshotInfo
    :: DiskSnapshotInfo
diskSnapshotInfo = DiskSnapshotInfo' {_dsiSizeInGb = Nothing}


-- | The size of the disk in GB (e.g., @32@ ).
dsiSizeInGb :: Lens' DiskSnapshotInfo (Maybe Int)
dsiSizeInGb = lens _dsiSizeInGb (\ s a -> s{_dsiSizeInGb = a})

instance FromJSON DiskSnapshotInfo where
        parseJSON
          = withObject "DiskSnapshotInfo"
              (\ x -> DiskSnapshotInfo' <$> (x .:? "sizeInGb"))

instance Hashable DiskSnapshotInfo where

instance NFData DiskSnapshotInfo where

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
  , _domTags          :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
--
-- * 'domTags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
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
    , _domTags = Nothing
    }


-- | The resource type.
domResourceType :: Lens' Domain (Maybe ResourceType)
domResourceType = lens _domResourceType (\ s a -> s{_domResourceType = a})

-- | An array of key-value pairs containing information about the domain entries.
domDomainEntries :: Lens' Domain [DomainEntry]
domDomainEntries = lens _domDomainEntries (\ s a -> s{_domDomainEntries = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the domain recordset (e.g., @arn:aws:lightsail:global:123456789101:Domain/824cede0-abc7-4f84-8dbc-12345EXAMPLE@ ).
domArn :: Lens' Domain (Maybe Text)
domArn = lens _domArn (\ s a -> s{_domArn = a})

-- | The date when the domain recordset was created.
domCreatedAt :: Lens' Domain (Maybe UTCTime)
domCreatedAt = lens _domCreatedAt (\ s a -> s{_domCreatedAt = a}) . mapping _Time

-- | The AWS Region and Availability Zones where the domain recordset was created.
domLocation :: Lens' Domain (Maybe ResourceLocation)
domLocation = lens _domLocation (\ s a -> s{_domLocation = a})

-- | The name of the domain.
domName :: Lens' Domain (Maybe Text)
domName = lens _domName (\ s a -> s{_domName = a})

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
domSupportCode :: Lens' Domain (Maybe Text)
domSupportCode = lens _domSupportCode (\ s a -> s{_domSupportCode = a})

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
domTags :: Lens' Domain [Tag]
domTags = lens _domTags (\ s a -> s{_domTags = a}) . _Default . _Coerce

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
                     <*> (x .:? "supportCode")
                     <*> (x .:? "tags" .!= mempty))

instance Hashable Domain where

instance NFData Domain where

-- | Describes a domain recordset entry.
--
--
--
-- /See:/ 'domainEntry' smart constructor.
data DomainEntry = DomainEntry'
  { _deIsAlias :: !(Maybe Bool)
  , _deName    :: !(Maybe Text)
  , _deId      :: !(Maybe Text)
  , _deOptions :: !(Maybe (Map Text Text))
  , _deType    :: !(Maybe Text)
  , _deTarget  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DomainEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deIsAlias' - When @true@ , specifies whether the domain entry is an alias used by the Lightsail load balancer. You can include an alias (A type) record in your request, which points to a load balancer DNS name and routes traffic to your load balancer
--
-- * 'deName' - The name of the domain.
--
-- * 'deId' - The ID of the domain recordset entry.
--
-- * 'deOptions' - (Deprecated) The options for the domain entry.
--
-- * 'deType' - The type of domain entry, such as address (A), canonical name (CNAME), mail exchanger (MX), name server (NS), start of authority (SOA), service locator (SRV), or text (TXT). The following domain entry types can be used:     * @A@      * @CNAME@      * @MX@      * @NS@      * @SOA@      * @SRV@      * @TXT@
--
-- * 'deTarget' - The target AWS name server (e.g., @ns-111.awsdns-22.com.@ ). For Lightsail load balancers, the value looks like @ab1234c56789c6b86aba6fb203d443bc-123456789.us-east-2.elb.amazonaws.com@ . Be sure to also set @isAlias@ to @true@ when setting up an A record for a load balancer.
domainEntry
    :: DomainEntry
domainEntry =
  DomainEntry'
    { _deIsAlias = Nothing
    , _deName = Nothing
    , _deId = Nothing
    , _deOptions = Nothing
    , _deType = Nothing
    , _deTarget = Nothing
    }


-- | When @true@ , specifies whether the domain entry is an alias used by the Lightsail load balancer. You can include an alias (A type) record in your request, which points to a load balancer DNS name and routes traffic to your load balancer
deIsAlias :: Lens' DomainEntry (Maybe Bool)
deIsAlias = lens _deIsAlias (\ s a -> s{_deIsAlias = a})

-- | The name of the domain.
deName :: Lens' DomainEntry (Maybe Text)
deName = lens _deName (\ s a -> s{_deName = a})

-- | The ID of the domain recordset entry.
deId :: Lens' DomainEntry (Maybe Text)
deId = lens _deId (\ s a -> s{_deId = a})

-- | (Deprecated) The options for the domain entry.
deOptions :: Lens' DomainEntry (HashMap Text Text)
deOptions = lens _deOptions (\ s a -> s{_deOptions = a}) . _Default . _Map

-- | The type of domain entry, such as address (A), canonical name (CNAME), mail exchanger (MX), name server (NS), start of authority (SOA), service locator (SRV), or text (TXT). The following domain entry types can be used:     * @A@      * @CNAME@      * @MX@      * @NS@      * @SOA@      * @SRV@      * @TXT@
deType :: Lens' DomainEntry (Maybe Text)
deType = lens _deType (\ s a -> s{_deType = a})

-- | The target AWS name server (e.g., @ns-111.awsdns-22.com.@ ). For Lightsail load balancers, the value looks like @ab1234c56789c6b86aba6fb203d443bc-123456789.us-east-2.elb.amazonaws.com@ . Be sure to also set @isAlias@ to @true@ when setting up an A record for a load balancer.
deTarget :: Lens' DomainEntry (Maybe Text)
deTarget = lens _deTarget (\ s a -> s{_deTarget = a})

instance FromJSON DomainEntry where
        parseJSON
          = withObject "DomainEntry"
              (\ x ->
                 DomainEntry' <$>
                   (x .:? "isAlias") <*> (x .:? "name") <*> (x .:? "id")
                     <*> (x .:? "options" .!= mempty)
                     <*> (x .:? "type")
                     <*> (x .:? "target"))

instance Hashable DomainEntry where

instance NFData DomainEntry where

instance ToJSON DomainEntry where
        toJSON DomainEntry'{..}
          = object
              (catMaybes
                 [("isAlias" .=) <$> _deIsAlias,
                  ("name" .=) <$> _deName, ("id" .=) <$> _deId,
                  ("options" .=) <$> _deOptions,
                  ("type" .=) <$> _deType,
                  ("target" .=) <$> _deTarget])

-- | Describes an export snapshot record.
--
--
--
-- /See:/ 'exportSnapshotRecord' smart constructor.
data ExportSnapshotRecord = ExportSnapshotRecord'
  { _esrState           :: !(Maybe RecordState)
  , _esrDestinationInfo :: !(Maybe DestinationInfo)
  , _esrResourceType    :: !(Maybe ResourceType)
  , _esrArn             :: !(Maybe Text)
  , _esrCreatedAt       :: !(Maybe POSIX)
  , _esrLocation        :: !(Maybe ResourceLocation)
  , _esrName            :: !(Maybe Text)
  , _esrSourceInfo      :: !(Maybe ExportSnapshotRecordSourceInfo)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExportSnapshotRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esrState' - The state of the export snapshot record.
--
-- * 'esrDestinationInfo' - A list of objects describing the destination of the export snapshot record.
--
-- * 'esrResourceType' - The Lightsail resource type (e.g., @ExportSnapshotRecord@ ).
--
-- * 'esrArn' - The Amazon Resource Name (ARN) of the export snapshot record.
--
-- * 'esrCreatedAt' - The date when the export snapshot record was created.
--
-- * 'esrLocation' - The AWS Region and Availability Zone where the export snapshot record is located.
--
-- * 'esrName' - The export snapshot record name.
--
-- * 'esrSourceInfo' - A list of objects describing the source of the export snapshot record.
exportSnapshotRecord
    :: ExportSnapshotRecord
exportSnapshotRecord =
  ExportSnapshotRecord'
    { _esrState = Nothing
    , _esrDestinationInfo = Nothing
    , _esrResourceType = Nothing
    , _esrArn = Nothing
    , _esrCreatedAt = Nothing
    , _esrLocation = Nothing
    , _esrName = Nothing
    , _esrSourceInfo = Nothing
    }


-- | The state of the export snapshot record.
esrState :: Lens' ExportSnapshotRecord (Maybe RecordState)
esrState = lens _esrState (\ s a -> s{_esrState = a})

-- | A list of objects describing the destination of the export snapshot record.
esrDestinationInfo :: Lens' ExportSnapshotRecord (Maybe DestinationInfo)
esrDestinationInfo = lens _esrDestinationInfo (\ s a -> s{_esrDestinationInfo = a})

-- | The Lightsail resource type (e.g., @ExportSnapshotRecord@ ).
esrResourceType :: Lens' ExportSnapshotRecord (Maybe ResourceType)
esrResourceType = lens _esrResourceType (\ s a -> s{_esrResourceType = a})

-- | The Amazon Resource Name (ARN) of the export snapshot record.
esrArn :: Lens' ExportSnapshotRecord (Maybe Text)
esrArn = lens _esrArn (\ s a -> s{_esrArn = a})

-- | The date when the export snapshot record was created.
esrCreatedAt :: Lens' ExportSnapshotRecord (Maybe UTCTime)
esrCreatedAt = lens _esrCreatedAt (\ s a -> s{_esrCreatedAt = a}) . mapping _Time

-- | The AWS Region and Availability Zone where the export snapshot record is located.
esrLocation :: Lens' ExportSnapshotRecord (Maybe ResourceLocation)
esrLocation = lens _esrLocation (\ s a -> s{_esrLocation = a})

-- | The export snapshot record name.
esrName :: Lens' ExportSnapshotRecord (Maybe Text)
esrName = lens _esrName (\ s a -> s{_esrName = a})

-- | A list of objects describing the source of the export snapshot record.
esrSourceInfo :: Lens' ExportSnapshotRecord (Maybe ExportSnapshotRecordSourceInfo)
esrSourceInfo = lens _esrSourceInfo (\ s a -> s{_esrSourceInfo = a})

instance FromJSON ExportSnapshotRecord where
        parseJSON
          = withObject "ExportSnapshotRecord"
              (\ x ->
                 ExportSnapshotRecord' <$>
                   (x .:? "state") <*> (x .:? "destinationInfo") <*>
                     (x .:? "resourceType")
                     <*> (x .:? "arn")
                     <*> (x .:? "createdAt")
                     <*> (x .:? "location")
                     <*> (x .:? "name")
                     <*> (x .:? "sourceInfo"))

instance Hashable ExportSnapshotRecord where

instance NFData ExportSnapshotRecord where

-- | Describes the source of an export snapshot record.
--
--
--
-- /See:/ 'exportSnapshotRecordSourceInfo' smart constructor.
data ExportSnapshotRecordSourceInfo = ExportSnapshotRecordSourceInfo'
  { _esrsiDiskSnapshotInfo     :: !(Maybe DiskSnapshotInfo)
  , _esrsiResourceType         :: !(Maybe ExportSnapshotRecordSourceType)
  , _esrsiArn                  :: !(Maybe Text)
  , _esrsiCreatedAt            :: !(Maybe POSIX)
  , _esrsiFromResourceARN      :: !(Maybe Text)
  , _esrsiName                 :: !(Maybe Text)
  , _esrsiInstanceSnapshotInfo :: !(Maybe InstanceSnapshotInfo)
  , _esrsiFromResourceName     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExportSnapshotRecordSourceInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esrsiDiskSnapshotInfo' - A list of objects describing a disk snapshot.
--
-- * 'esrsiResourceType' - The Lightsail resource type (e.g., @InstanceSnapshot@ or @DiskSnapshot@ ).
--
-- * 'esrsiArn' - The Amazon Resource Name (ARN) of the source instance or disk snapshot.
--
-- * 'esrsiCreatedAt' - The date when the source instance or disk snapshot was created.
--
-- * 'esrsiFromResourceARN' - The Amazon Resource Name (ARN) of the snapshot's source instance or disk.
--
-- * 'esrsiName' - The name of the source instance or disk snapshot.
--
-- * 'esrsiInstanceSnapshotInfo' - A list of objects describing an instance snapshot.
--
-- * 'esrsiFromResourceName' - The name of the snapshot's source instance or disk.
exportSnapshotRecordSourceInfo
    :: ExportSnapshotRecordSourceInfo
exportSnapshotRecordSourceInfo =
  ExportSnapshotRecordSourceInfo'
    { _esrsiDiskSnapshotInfo = Nothing
    , _esrsiResourceType = Nothing
    , _esrsiArn = Nothing
    , _esrsiCreatedAt = Nothing
    , _esrsiFromResourceARN = Nothing
    , _esrsiName = Nothing
    , _esrsiInstanceSnapshotInfo = Nothing
    , _esrsiFromResourceName = Nothing
    }


-- | A list of objects describing a disk snapshot.
esrsiDiskSnapshotInfo :: Lens' ExportSnapshotRecordSourceInfo (Maybe DiskSnapshotInfo)
esrsiDiskSnapshotInfo = lens _esrsiDiskSnapshotInfo (\ s a -> s{_esrsiDiskSnapshotInfo = a})

-- | The Lightsail resource type (e.g., @InstanceSnapshot@ or @DiskSnapshot@ ).
esrsiResourceType :: Lens' ExportSnapshotRecordSourceInfo (Maybe ExportSnapshotRecordSourceType)
esrsiResourceType = lens _esrsiResourceType (\ s a -> s{_esrsiResourceType = a})

-- | The Amazon Resource Name (ARN) of the source instance or disk snapshot.
esrsiArn :: Lens' ExportSnapshotRecordSourceInfo (Maybe Text)
esrsiArn = lens _esrsiArn (\ s a -> s{_esrsiArn = a})

-- | The date when the source instance or disk snapshot was created.
esrsiCreatedAt :: Lens' ExportSnapshotRecordSourceInfo (Maybe UTCTime)
esrsiCreatedAt = lens _esrsiCreatedAt (\ s a -> s{_esrsiCreatedAt = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the snapshot's source instance or disk.
esrsiFromResourceARN :: Lens' ExportSnapshotRecordSourceInfo (Maybe Text)
esrsiFromResourceARN = lens _esrsiFromResourceARN (\ s a -> s{_esrsiFromResourceARN = a})

-- | The name of the source instance or disk snapshot.
esrsiName :: Lens' ExportSnapshotRecordSourceInfo (Maybe Text)
esrsiName = lens _esrsiName (\ s a -> s{_esrsiName = a})

-- | A list of objects describing an instance snapshot.
esrsiInstanceSnapshotInfo :: Lens' ExportSnapshotRecordSourceInfo (Maybe InstanceSnapshotInfo)
esrsiInstanceSnapshotInfo = lens _esrsiInstanceSnapshotInfo (\ s a -> s{_esrsiInstanceSnapshotInfo = a})

-- | The name of the snapshot's source instance or disk.
esrsiFromResourceName :: Lens' ExportSnapshotRecordSourceInfo (Maybe Text)
esrsiFromResourceName = lens _esrsiFromResourceName (\ s a -> s{_esrsiFromResourceName = a})

instance FromJSON ExportSnapshotRecordSourceInfo
         where
        parseJSON
          = withObject "ExportSnapshotRecordSourceInfo"
              (\ x ->
                 ExportSnapshotRecordSourceInfo' <$>
                   (x .:? "diskSnapshotInfo") <*> (x .:? "resourceType")
                     <*> (x .:? "arn")
                     <*> (x .:? "createdAt")
                     <*> (x .:? "fromResourceArn")
                     <*> (x .:? "name")
                     <*> (x .:? "instanceSnapshotInfo")
                     <*> (x .:? "fromResourceName"))

instance Hashable ExportSnapshotRecordSourceInfo
         where

instance NFData ExportSnapshotRecordSourceInfo where

-- | Describes the public SSH host keys or the RDP certificate.
--
--
--
-- /See:/ 'hostKeyAttributes' smart constructor.
data HostKeyAttributes = HostKeyAttributes'
  { _hkaNotValidAfter     :: !(Maybe POSIX)
  , _hkaNotValidBefore    :: !(Maybe POSIX)
  , _hkaFingerprintSHA1   :: !(Maybe Text)
  , _hkaPublicKey         :: !(Maybe Text)
  , _hkaAlgorithm         :: !(Maybe Text)
  , _hkaWitnessedAt       :: !(Maybe POSIX)
  , _hkaFingerprintSHA256 :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HostKeyAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hkaNotValidAfter' - The returned RDP certificate is not valid after this point in time. This value is listed only for RDP certificates.
--
-- * 'hkaNotValidBefore' - The returned RDP certificate is valid after this point in time. This value is listed only for RDP certificates.
--
-- * 'hkaFingerprintSHA1' - The SHA-1 fingerprint of the returned SSH host key or RDP certificate.     * Example of an SHA-1 SSH fingerprint: @SHA1:1CHH6FaAaXjtFOsR/t83vf91SR0@      * Example of an SHA-1 RDP fingerprint: @af:34:51:fe:09:f0:e0:da:b8:4e:56:ca:60:c2:10:ff:38:06:db:45@
--
-- * 'hkaPublicKey' - The public SSH host key or the RDP certificate.
--
-- * 'hkaAlgorithm' - The SSH host key algorithm or the RDP certificate format. For SSH host keys, the algorithm may be @ssh-rsa@ , @ecdsa-sha2-nistp256@ , @ssh-ed25519@ , etc. For RDP certificates, the algorithm is always @x509-cert@ .
--
-- * 'hkaWitnessedAt' - The time that the SSH host key or RDP certificate was recorded by Lightsail.
--
-- * 'hkaFingerprintSHA256' - The SHA-256 fingerprint of the returned SSH host key or RDP certificate.     * Example of an SHA-256 SSH fingerprint: @SHA256:KTsMnRBh1IhD17HpdfsbzeGA4jOijm5tyXsMjKVbB8o@      * Example of an SHA-256 RDP fingerprint: @03:9b:36:9f:4b:de:4e:61:70:fc:7c:c9:78:e7:d2:1a:1c:25:a8:0c:91:f6:7c:e4:d6:a0:85:c8:b4:53:99:68@
hostKeyAttributes
    :: HostKeyAttributes
hostKeyAttributes =
  HostKeyAttributes'
    { _hkaNotValidAfter = Nothing
    , _hkaNotValidBefore = Nothing
    , _hkaFingerprintSHA1 = Nothing
    , _hkaPublicKey = Nothing
    , _hkaAlgorithm = Nothing
    , _hkaWitnessedAt = Nothing
    , _hkaFingerprintSHA256 = Nothing
    }


-- | The returned RDP certificate is not valid after this point in time. This value is listed only for RDP certificates.
hkaNotValidAfter :: Lens' HostKeyAttributes (Maybe UTCTime)
hkaNotValidAfter = lens _hkaNotValidAfter (\ s a -> s{_hkaNotValidAfter = a}) . mapping _Time

-- | The returned RDP certificate is valid after this point in time. This value is listed only for RDP certificates.
hkaNotValidBefore :: Lens' HostKeyAttributes (Maybe UTCTime)
hkaNotValidBefore = lens _hkaNotValidBefore (\ s a -> s{_hkaNotValidBefore = a}) . mapping _Time

-- | The SHA-1 fingerprint of the returned SSH host key or RDP certificate.     * Example of an SHA-1 SSH fingerprint: @SHA1:1CHH6FaAaXjtFOsR/t83vf91SR0@      * Example of an SHA-1 RDP fingerprint: @af:34:51:fe:09:f0:e0:da:b8:4e:56:ca:60:c2:10:ff:38:06:db:45@
hkaFingerprintSHA1 :: Lens' HostKeyAttributes (Maybe Text)
hkaFingerprintSHA1 = lens _hkaFingerprintSHA1 (\ s a -> s{_hkaFingerprintSHA1 = a})

-- | The public SSH host key or the RDP certificate.
hkaPublicKey :: Lens' HostKeyAttributes (Maybe Text)
hkaPublicKey = lens _hkaPublicKey (\ s a -> s{_hkaPublicKey = a})

-- | The SSH host key algorithm or the RDP certificate format. For SSH host keys, the algorithm may be @ssh-rsa@ , @ecdsa-sha2-nistp256@ , @ssh-ed25519@ , etc. For RDP certificates, the algorithm is always @x509-cert@ .
hkaAlgorithm :: Lens' HostKeyAttributes (Maybe Text)
hkaAlgorithm = lens _hkaAlgorithm (\ s a -> s{_hkaAlgorithm = a})

-- | The time that the SSH host key or RDP certificate was recorded by Lightsail.
hkaWitnessedAt :: Lens' HostKeyAttributes (Maybe UTCTime)
hkaWitnessedAt = lens _hkaWitnessedAt (\ s a -> s{_hkaWitnessedAt = a}) . mapping _Time

-- | The SHA-256 fingerprint of the returned SSH host key or RDP certificate.     * Example of an SHA-256 SSH fingerprint: @SHA256:KTsMnRBh1IhD17HpdfsbzeGA4jOijm5tyXsMjKVbB8o@      * Example of an SHA-256 RDP fingerprint: @03:9b:36:9f:4b:de:4e:61:70:fc:7c:c9:78:e7:d2:1a:1c:25:a8:0c:91:f6:7c:e4:d6:a0:85:c8:b4:53:99:68@
hkaFingerprintSHA256 :: Lens' HostKeyAttributes (Maybe Text)
hkaFingerprintSHA256 = lens _hkaFingerprintSHA256 (\ s a -> s{_hkaFingerprintSHA256 = a})

instance FromJSON HostKeyAttributes where
        parseJSON
          = withObject "HostKeyAttributes"
              (\ x ->
                 HostKeyAttributes' <$>
                   (x .:? "notValidAfter") <*> (x .:? "notValidBefore")
                     <*> (x .:? "fingerprintSHA1")
                     <*> (x .:? "publicKey")
                     <*> (x .:? "algorithm")
                     <*> (x .:? "witnessedAt")
                     <*> (x .:? "fingerprintSHA256"))

instance Hashable HostKeyAttributes where

instance NFData HostKeyAttributes where

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
  , _iTags             :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
-- * 'iArn' - The Amazon Resource Name (ARN) of the instance (e.g., @arn:aws:lightsail:us-east-2:123456789101:Instance/244ad76f-8aad-4741-809f-12345EXAMPLE@ ).
--
-- * 'iCreatedAt' - The timestamp when the instance was created (e.g., @1479734909.17@ ).
--
-- * 'iLocation' - The region name and Availability Zone where the instance is located.
--
-- * 'iSshKeyName' - The name of the SSH key being used to connect to the instance (e.g., @LightsailDefaultKeyPair@ ).
--
-- * 'iUsername' - The user name for connecting to the instance (e.g., @ec2-user@ ).
--
-- * 'iNetworking' - Information about the public ports and monthly data transfer rates for the instance.
--
-- * 'iBundleId' - The bundle for the instance (e.g., @micro_1_0@ ).
--
-- * 'iName' - The name the user gave the instance (e.g., @Amazon_Linux-1GB-Ohio-1@ ).
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
--
-- * 'iTags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
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
    , _iTags = Nothing
    }


-- | The status code and the state (e.g., @running@ ) for the instance.
iState :: Lens' Instance (Maybe InstanceState)
iState = lens _iState (\ s a -> s{_iState = a})

-- | The IPv6 address of the instance.
iIpv6Address :: Lens' Instance (Maybe Text)
iIpv6Address = lens _iIpv6Address (\ s a -> s{_iIpv6Address = a})

-- | The type of resource (usually @Instance@ ).
iResourceType :: Lens' Instance (Maybe ResourceType)
iResourceType = lens _iResourceType (\ s a -> s{_iResourceType = a})

-- | The Amazon Resource Name (ARN) of the instance (e.g., @arn:aws:lightsail:us-east-2:123456789101:Instance/244ad76f-8aad-4741-809f-12345EXAMPLE@ ).
iArn :: Lens' Instance (Maybe Text)
iArn = lens _iArn (\ s a -> s{_iArn = a})

-- | The timestamp when the instance was created (e.g., @1479734909.17@ ).
iCreatedAt :: Lens' Instance (Maybe UTCTime)
iCreatedAt = lens _iCreatedAt (\ s a -> s{_iCreatedAt = a}) . mapping _Time

-- | The region name and Availability Zone where the instance is located.
iLocation :: Lens' Instance (Maybe ResourceLocation)
iLocation = lens _iLocation (\ s a -> s{_iLocation = a})

-- | The name of the SSH key being used to connect to the instance (e.g., @LightsailDefaultKeyPair@ ).
iSshKeyName :: Lens' Instance (Maybe Text)
iSshKeyName = lens _iSshKeyName (\ s a -> s{_iSshKeyName = a})

-- | The user name for connecting to the instance (e.g., @ec2-user@ ).
iUsername :: Lens' Instance (Maybe Text)
iUsername = lens _iUsername (\ s a -> s{_iUsername = a})

-- | Information about the public ports and monthly data transfer rates for the instance.
iNetworking :: Lens' Instance (Maybe InstanceNetworking)
iNetworking = lens _iNetworking (\ s a -> s{_iNetworking = a})

-- | The bundle for the instance (e.g., @micro_1_0@ ).
iBundleId :: Lens' Instance (Maybe Text)
iBundleId = lens _iBundleId (\ s a -> s{_iBundleId = a})

-- | The name the user gave the instance (e.g., @Amazon_Linux-1GB-Ohio-1@ ).
iName :: Lens' Instance (Maybe Text)
iName = lens _iName (\ s a -> s{_iName = a})

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
iSupportCode :: Lens' Instance (Maybe Text)
iSupportCode = lens _iSupportCode (\ s a -> s{_iSupportCode = a})

-- | The blueprint ID (e.g., @os_amlinux_2016_03@ ).
iBlueprintId :: Lens' Instance (Maybe Text)
iBlueprintId = lens _iBlueprintId (\ s a -> s{_iBlueprintId = a})

-- | The private IP address of the instance.
iPrivateIPAddress :: Lens' Instance (Maybe Text)
iPrivateIPAddress = lens _iPrivateIPAddress (\ s a -> s{_iPrivateIPAddress = a})

-- | The friendly name of the blueprint (e.g., @Amazon Linux@ ).
iBlueprintName :: Lens' Instance (Maybe Text)
iBlueprintName = lens _iBlueprintName (\ s a -> s{_iBlueprintName = a})

-- | A Boolean value indicating whether this instance has a static IP assigned to it.
iIsStaticIP :: Lens' Instance (Maybe Bool)
iIsStaticIP = lens _iIsStaticIP (\ s a -> s{_iIsStaticIP = a})

-- | The public IP address of the instance.
iPublicIPAddress :: Lens' Instance (Maybe Text)
iPublicIPAddress = lens _iPublicIPAddress (\ s a -> s{_iPublicIPAddress = a})

-- | The size of the vCPU and the amount of RAM for the instance.
iHardware :: Lens' Instance (Maybe InstanceHardware)
iHardware = lens _iHardware (\ s a -> s{_iHardware = a})

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
iTags :: Lens' Instance [Tag]
iTags = lens _iTags (\ s a -> s{_iTags = a}) . _Default . _Coerce

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
                     <*> (x .:? "hardware")
                     <*> (x .:? "tags" .!= mempty))

instance Hashable Instance where

instance NFData Instance where

-- | The parameters for gaining temporary access to one of your Amazon Lightsail instances.
--
--
--
-- /See:/ 'instanceAccessDetails' smart constructor.
data InstanceAccessDetails = InstanceAccessDetails'
  { _iadHostKeys     :: !(Maybe [HostKeyAttributes])
  , _iadCertKey      :: !(Maybe Text)
  , _iadIpAddress    :: !(Maybe Text)
  , _iadPrivateKey   :: !(Maybe Text)
  , _iadExpiresAt    :: !(Maybe POSIX)
  , _iadUsername     :: !(Maybe Text)
  , _iadProtocol     :: !(Maybe InstanceAccessProtocol)
  , _iadPasswordData :: !(Maybe PasswordData)
  , _iadPassword     :: !(Maybe Text)
  , _iadInstanceName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceAccessDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iadHostKeys' - Describes the public SSH host keys or the RDP certificate.
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
-- * 'iadPasswordData' - For a Windows Server-based instance, an object with the data you can use to retrieve your password. This is only needed if @password@ is empty and the instance is not new (and therefore the password is not ready yet). When you create an instance, it can take up to 15 minutes for the instance to be ready.
--
-- * 'iadPassword' - For RDP access, the password for your Amazon Lightsail instance. Password will be an empty string if the password for your new instance is not ready yet. When you create an instance, it can take up to 15 minutes for the instance to be ready.
--
-- * 'iadInstanceName' - The name of this Amazon Lightsail instance.
instanceAccessDetails
    :: InstanceAccessDetails
instanceAccessDetails =
  InstanceAccessDetails'
    { _iadHostKeys = Nothing
    , _iadCertKey = Nothing
    , _iadIpAddress = Nothing
    , _iadPrivateKey = Nothing
    , _iadExpiresAt = Nothing
    , _iadUsername = Nothing
    , _iadProtocol = Nothing
    , _iadPasswordData = Nothing
    , _iadPassword = Nothing
    , _iadInstanceName = Nothing
    }


-- | Describes the public SSH host keys or the RDP certificate.
iadHostKeys :: Lens' InstanceAccessDetails [HostKeyAttributes]
iadHostKeys = lens _iadHostKeys (\ s a -> s{_iadHostKeys = a}) . _Default . _Coerce

-- | For SSH access, the public key to use when accessing your instance For OpenSSH clients (e.g., command line SSH), you should save this value to @tempkey-cert.pub@ .
iadCertKey :: Lens' InstanceAccessDetails (Maybe Text)
iadCertKey = lens _iadCertKey (\ s a -> s{_iadCertKey = a})

-- | The public IP address of the Amazon Lightsail instance.
iadIpAddress :: Lens' InstanceAccessDetails (Maybe Text)
iadIpAddress = lens _iadIpAddress (\ s a -> s{_iadIpAddress = a})

-- | For SSH access, the temporary private key. For OpenSSH clients (e.g., command line SSH), you should save this value to @tempkey@ ).
iadPrivateKey :: Lens' InstanceAccessDetails (Maybe Text)
iadPrivateKey = lens _iadPrivateKey (\ s a -> s{_iadPrivateKey = a})

-- | For SSH access, the date on which the temporary keys expire.
iadExpiresAt :: Lens' InstanceAccessDetails (Maybe UTCTime)
iadExpiresAt = lens _iadExpiresAt (\ s a -> s{_iadExpiresAt = a}) . mapping _Time

-- | The user name to use when logging in to the Amazon Lightsail instance.
iadUsername :: Lens' InstanceAccessDetails (Maybe Text)
iadUsername = lens _iadUsername (\ s a -> s{_iadUsername = a})

-- | The protocol for these Amazon Lightsail instance access details.
iadProtocol :: Lens' InstanceAccessDetails (Maybe InstanceAccessProtocol)
iadProtocol = lens _iadProtocol (\ s a -> s{_iadProtocol = a})

-- | For a Windows Server-based instance, an object with the data you can use to retrieve your password. This is only needed if @password@ is empty and the instance is not new (and therefore the password is not ready yet). When you create an instance, it can take up to 15 minutes for the instance to be ready.
iadPasswordData :: Lens' InstanceAccessDetails (Maybe PasswordData)
iadPasswordData = lens _iadPasswordData (\ s a -> s{_iadPasswordData = a})

-- | For RDP access, the password for your Amazon Lightsail instance. Password will be an empty string if the password for your new instance is not ready yet. When you create an instance, it can take up to 15 minutes for the instance to be ready.
iadPassword :: Lens' InstanceAccessDetails (Maybe Text)
iadPassword = lens _iadPassword (\ s a -> s{_iadPassword = a})

-- | The name of this Amazon Lightsail instance.
iadInstanceName :: Lens' InstanceAccessDetails (Maybe Text)
iadInstanceName = lens _iadInstanceName (\ s a -> s{_iadInstanceName = a})

instance FromJSON InstanceAccessDetails where
        parseJSON
          = withObject "InstanceAccessDetails"
              (\ x ->
                 InstanceAccessDetails' <$>
                   (x .:? "hostKeys" .!= mempty) <*> (x .:? "certKey")
                     <*> (x .:? "ipAddress")
                     <*> (x .:? "privateKey")
                     <*> (x .:? "expiresAt")
                     <*> (x .:? "username")
                     <*> (x .:? "protocol")
                     <*> (x .:? "passwordData")
                     <*> (x .:? "password")
                     <*> (x .:? "instanceName"))

instance Hashable InstanceAccessDetails where

instance NFData InstanceAccessDetails where

-- | Describes the Amazon Elastic Compute Cloud instance and related resources to be created using the @create cloud formation stack@ operation.
--
--
--
-- /See:/ 'instanceEntry' smart constructor.
data InstanceEntry = InstanceEntry'
  { _ieUserData         :: !(Maybe Text)
  , _ieSourceName       :: !Text
  , _ieInstanceType     :: !Text
  , _iePortInfoSource   :: !PortInfoSourceType
  , _ieAvailabilityZone :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ieUserData' - A launch script you can create that configures a server with additional user data. For example, you might want to run @apt-get -y update@ .
--
-- * 'ieSourceName' - The name of the export snapshot record, which contains the exported Lightsail instance snapshot that will be used as the source of the new Amazon EC2 instance. Use the @get export snapshot records@ operation to get a list of export snapshot records that you can use to create a CloudFormation stack.
--
-- * 'ieInstanceType' - The instance type (e.g., @t2.micro@ ) to use for the new Amazon EC2 instance.
--
-- * 'iePortInfoSource' - The port configuration to use for the new Amazon EC2 instance. The following configuration options are available:     * DEFAULT
