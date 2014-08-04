{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AWS Storage Gateway is a service that connects an on-premises software
-- appliance with cloud-based storage to provide seamless and secure
-- integration between your on-premises IT environment and AWS's storage
-- infrastructure.
module Network.AWS.StorageGateway.V2013_06_30.Types where

import Control.Lens.TH
import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2013-06-30@) of the
-- @AWS Storage Gateway@ service.
data StorageGateway deriving (Typeable)

instance AWSService StorageGateway where
    type Sg StorageGateway = V4
    data Er StorageGateway

        = InternalServerError
            { _iseError :: Maybe StorageGatewayError
            , _iseMessage :: Maybe Text
            }

        | InvalidGatewayRequestException
            { _igreError :: Maybe StorageGatewayError
            , _igreMessage :: Maybe Text
            }

        | StorageGatewayClient HttpException
        | StorageGatewaySerializer String
        | StorageGatewayService String
    service = Service
        { _svcEndpoint = Regional
        , _svcPrefix   = "storagegateway"
        , _svcVersion  = "2013-06-30"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er StorageGateway)
deriving instance Generic (Er StorageGateway)

instance AWSError (Er StorageGateway) where
    awsError = const "StorageGatewayError"

instance ServiceError (Er StorageGateway) where
    serviceError    = StorageGatewayService
    clientError     = StorageGatewayClient
    serializerError = StorageGatewaySerializer

instance Exception (Er StorageGateway)

data CachediSCSIVolumeInformation = CachediSCSIVolumeInformation
    { _cscsiviVolumeiSCSIAttributes :: Maybe VolumeiSCSIAttributes
      -- ^ Lists iSCSI information about a volume.
    , _cscsiviVolumeStatus :: Maybe Text
    , _cscsiviSourceSnapshotId :: Maybe Text
    , _cscsiviVolumeARN :: Maybe Text
    , _cscsiviVolumeProgress :: Maybe Double
    , _cscsiviVolumeSizeInBytes :: Maybe Integer
    , _cscsiviVolumeId :: Maybe Text
    , _cscsiviVolumeType :: Maybe Text
    } deriving (Generic)

instance FromJSON CachediSCSIVolumeInformation

-- | Describes Challenge-Handshake Authentication Protocol (CHAP) information
-- that supports authentication between your gateway and iSCSI initiators.
data ChapInfo = ChapInfo
    { _ciTargetARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the volume. Valid Values: 50 to
      -- 500 lowercase letters, numbers, periods (.), and hyphens (-).
    , _ciSecretToAuthenticateInitiator :: Maybe Text
      -- ^ The secret key that the initiator (e.g. Windows client) must
      -- provide to participate in mutual CHAP with the target.
    , _ciInitiatorName :: Maybe Text
      -- ^ The iSCSI initiator that connects to the target.
    , _ciSecretToAuthenticateTarget :: Maybe Text
      -- ^ The secret key that the target must provide to participate in
      -- mutual CHAP with the initiator (e.g. Windows client).
    } deriving (Generic)

instance FromJSON ChapInfo

data DeviceiSCSIAttributes = DeviceiSCSIAttributes
    { _dscsiaTargetARN :: Maybe Text
    , _dscsiaChapEnabled :: Maybe Bool
    , _dscsiaNetworkInterfaceId :: Maybe Text
    , _dscsiaNetworkInterfacePort :: Maybe Integer
    } deriving (Generic)

instance FromJSON DeviceiSCSIAttributes

instance ToJSON DeviceiSCSIAttributes

data DiskInformation = DiskInformation
    { _dwDiskAllocationResource :: Maybe Text
    , _dwDiskAllocationType :: Maybe Text
    , _dwDiskNode :: Maybe Text
    , _dwDiskPath :: Maybe Text
    , _dwDiskSizeInBytes :: Maybe Integer
    , _dwDiskId :: Maybe Text
    } deriving (Generic)

instance FromJSON DiskInformation

data GatewayInformation = GatewayInformation
    { _gjGatewayARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _gjGatewayType :: Maybe Text
    } deriving (Generic)

instance FromJSON GatewayInformation

-- | Describes a gateway's network interface.
data NetworkInterface = NetworkInterface
    { _niIpv6Address :: Maybe Text
      -- ^ The Internet Protocol version 6 (IPv6) address of the interface.
      -- Currently not supported.
    , _niMacAddress :: Maybe Text
      -- ^ The Media Access Control (MAC) address of the interface. This is
      -- currently unsupported and will not be returned in output.
    , _niIpv4Address :: Maybe Text
      -- ^ The Internet Protocol version 4 (IPv4) address of the interface.
    } deriving (Generic)

instance FromJSON NetworkInterface

data StorediSCSIVolumeInformation = StorediSCSIVolumeInformation
    { _sscsiviVolumeiSCSIAttributes :: Maybe VolumeiSCSIAttributes
      -- ^ Lists iSCSI information about a volume.
    , _sscsiviVolumeStatus :: Maybe Text
    , _sscsiviSourceSnapshotId :: Maybe Text
    , _sscsiviPreservedExistingData :: Maybe Bool
    , _sscsiviVolumeARN :: Maybe Text
    , _sscsiviVolumeProgress :: Maybe Double
    , _sscsiviVolumeSizeInBytes :: Maybe Integer
    , _sscsiviVolumeId :: Maybe Text
    , _sscsiviVolumeDiskId :: Maybe Text
    , _sscsiviVolumeType :: Maybe Text
    } deriving (Generic)

instance FromJSON StorediSCSIVolumeInformation

data Tape = Tape
    { _vTapeBarcode :: Maybe Text
    , _vTapeStatus :: Maybe Text
    , _vTapeARN :: Maybe Text
    , _vProgress :: Maybe Double
    , _vTapeSizeInBytes :: Maybe Integer
    , _vVTLDevice :: Maybe Text
    } deriving (Generic)

instance FromJSON Tape

data TapeArchive = TapeArchive
    { _tcTapeBarcode :: Maybe Text
    , _tcTapeStatus :: Maybe Text
    , _tcTapeARN :: Maybe Text
    , _tcTapeSizeInBytes :: Maybe Integer
    , _tcCompletionTime :: Maybe ISO8601
    , _tcRetrievedTo :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    } deriving (Generic)

instance FromJSON TapeArchive

data TapeRecoveryPointInfo = TapeRecoveryPointInfo
    { _trpjTapeRecoveryPointTime :: Maybe ISO8601
    , _trpjTapeARN :: Maybe Text
    , _trpjTapeSizeInBytes :: Maybe Integer
    } deriving (Generic)

instance FromJSON TapeRecoveryPointInfo

data VTLDevice = VTLDevice
    { _vtleDeviceiSCSIAttributes :: Maybe DeviceiSCSIAttributes
    , _vtleVTLDeviceARN :: Maybe Text
    } deriving (Generic)

instance FromJSON VTLDevice

data VolumeInformation = VolumeInformation
    { _vlVolumeARN :: Maybe Text
    , _vlVolumeType :: Maybe Text
    } deriving (Generic)

instance FromJSON VolumeInformation

data VolumeRecoveryPointInfo = VolumeRecoveryPointInfo
    { _vrpjVolumeRecoveryPointTime :: Maybe Text
    , _vrpjVolumeARN :: Maybe Text
    , _vrpjVolumeSizeInBytes :: Maybe Integer
    , _vrpjVolumeUsageInBytes :: Maybe Integer
    } deriving (Generic)

instance FromJSON VolumeRecoveryPointInfo

-- | Lists iSCSI information about a volume.
data VolumeiSCSIAttributes = VolumeiSCSIAttributes
    { _vscsiaLunNumber :: Maybe Integer
      -- ^ The logical disk number.
    , _vscsiaTargetARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the volume target.
    , _vscsiaChapEnabled :: Maybe Bool
      -- ^ Indicates whether mutual CHAP is enabled for the iSCSI target.
    , _vscsiaNetworkInterfaceId :: Maybe Text
      -- ^ The network interface identifier.
    , _vscsiaNetworkInterfacePort :: Maybe Integer
      -- ^ The port used to communicate with iSCSI targets.
    } deriving (Generic)

instance FromJSON VolumeiSCSIAttributes

instance ToJSON VolumeiSCSIAttributes

-- Newtypes

-- Products
makeLenses ''CachediSCSIVolumeInformation
makeLenses ''ChapInfo
makeLenses ''DeviceiSCSIAttributes
makeLenses ''DiskInformation
makeLenses ''GatewayInformation
makeLenses ''NetworkInterface
makeLenses ''StorediSCSIVolumeInformation
makeLenses ''Tape
makeLenses ''TapeArchive
makeLenses ''TapeRecoveryPointInfo
makeLenses ''VTLDevice
makeLenses ''VolumeInformation
makeLenses ''VolumeRecoveryPointInfo
makeLenses ''VolumeiSCSIAttributes
