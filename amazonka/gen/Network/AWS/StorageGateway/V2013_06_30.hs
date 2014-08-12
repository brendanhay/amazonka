-- Module      : Network.AWS.StorageGateway.V2013_06_30
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
module Network.AWS.StorageGateway.V2013_06_30
    ( module Network.AWS.StorageGateway.V2013_06_30.ActivateGateway
    , module Network.AWS.StorageGateway.V2013_06_30.AddCache
    , module Network.AWS.StorageGateway.V2013_06_30.AddUploadBuffer
    , module Network.AWS.StorageGateway.V2013_06_30.AddWorkingStorage
    , module Network.AWS.StorageGateway.V2013_06_30.CancelArchival
    , module Network.AWS.StorageGateway.V2013_06_30.CancelRetrieval
    , module Network.AWS.StorageGateway.V2013_06_30.CreateCachediSCSIVolume
    , module Network.AWS.StorageGateway.V2013_06_30.CreateSnapshot
    , module Network.AWS.StorageGateway.V2013_06_30.CreateSnapshotFromVolumeRecoveryPoint
    , module Network.AWS.StorageGateway.V2013_06_30.CreateStorediSCSIVolume
    , module Network.AWS.StorageGateway.V2013_06_30.CreateTapes
    , module Network.AWS.StorageGateway.V2013_06_30.DeleteBandwidthRateLimit
    , module Network.AWS.StorageGateway.V2013_06_30.DeleteChapCredentials
    , module Network.AWS.StorageGateway.V2013_06_30.DeleteGateway
    , module Network.AWS.StorageGateway.V2013_06_30.DeleteSnapshotSchedule
    , module Network.AWS.StorageGateway.V2013_06_30.DeleteTape
    , module Network.AWS.StorageGateway.V2013_06_30.DeleteTapeArchive
    , module Network.AWS.StorageGateway.V2013_06_30.DeleteVolume
    , module Network.AWS.StorageGateway.V2013_06_30.DescribeBandwidthRateLimit
    , module Network.AWS.StorageGateway.V2013_06_30.DescribeCache
    , module Network.AWS.StorageGateway.V2013_06_30.DescribeCachediSCSIVolumes
    , module Network.AWS.StorageGateway.V2013_06_30.DescribeChapCredentials
    , module Network.AWS.StorageGateway.V2013_06_30.DescribeGatewayInformation
    , module Network.AWS.StorageGateway.V2013_06_30.DescribeMaintenanceStartTime
    , module Network.AWS.StorageGateway.V2013_06_30.DescribeSnapshotSchedule
    , module Network.AWS.StorageGateway.V2013_06_30.DescribeStorediSCSIVolumes
    , module Network.AWS.StorageGateway.V2013_06_30.DescribeTapeArchives
    , module Network.AWS.StorageGateway.V2013_06_30.DescribeTapeRecoveryPoints
    , module Network.AWS.StorageGateway.V2013_06_30.DescribeTapes
    , module Network.AWS.StorageGateway.V2013_06_30.DescribeUploadBuffer
    , module Network.AWS.StorageGateway.V2013_06_30.DescribeVTLDevices
    , module Network.AWS.StorageGateway.V2013_06_30.DescribeWorkingStorage
    , module Network.AWS.StorageGateway.V2013_06_30.DisableGateway
    , module Network.AWS.StorageGateway.V2013_06_30.ListGateways
    , module Network.AWS.StorageGateway.V2013_06_30.ListLocalDisks
    , module Network.AWS.StorageGateway.V2013_06_30.ListVolumeRecoveryPoints
    , module Network.AWS.StorageGateway.V2013_06_30.ListVolumes
    , module Network.AWS.StorageGateway.V2013_06_30.RetrieveTapeArchive
    , module Network.AWS.StorageGateway.V2013_06_30.RetrieveTapeRecoveryPoint
    , module Network.AWS.StorageGateway.V2013_06_30.ShutdownGateway
    , module Network.AWS.StorageGateway.V2013_06_30.StartGateway
    , module Network.AWS.StorageGateway.V2013_06_30.Types
    , module Network.AWS.StorageGateway.V2013_06_30.UpdateBandwidthRateLimit
    , module Network.AWS.StorageGateway.V2013_06_30.UpdateChapCredentials
    , module Network.AWS.StorageGateway.V2013_06_30.UpdateGatewayInformation
    , module Network.AWS.StorageGateway.V2013_06_30.UpdateGatewaySoftwareNow
    , module Network.AWS.StorageGateway.V2013_06_30.UpdateMaintenanceStartTime
    , module Network.AWS.StorageGateway.V2013_06_30.UpdateSnapshotSchedule
    ) where

import Network.AWS.StorageGateway.V2013_06_30.ActivateGateway
import Network.AWS.StorageGateway.V2013_06_30.AddCache
import Network.AWS.StorageGateway.V2013_06_30.AddUploadBuffer
import Network.AWS.StorageGateway.V2013_06_30.AddWorkingStorage
import Network.AWS.StorageGateway.V2013_06_30.CancelArchival
import Network.AWS.StorageGateway.V2013_06_30.CancelRetrieval
import Network.AWS.StorageGateway.V2013_06_30.CreateCachediSCSIVolume
import Network.AWS.StorageGateway.V2013_06_30.CreateSnapshot
import Network.AWS.StorageGateway.V2013_06_30.CreateSnapshotFromVolumeRecoveryPoint
import Network.AWS.StorageGateway.V2013_06_30.CreateStorediSCSIVolume
import Network.AWS.StorageGateway.V2013_06_30.CreateTapes
import Network.AWS.StorageGateway.V2013_06_30.DeleteBandwidthRateLimit
import Network.AWS.StorageGateway.V2013_06_30.DeleteChapCredentials
import Network.AWS.StorageGateway.V2013_06_30.DeleteGateway
import Network.AWS.StorageGateway.V2013_06_30.DeleteSnapshotSchedule
import Network.AWS.StorageGateway.V2013_06_30.DeleteTape
import Network.AWS.StorageGateway.V2013_06_30.DeleteTapeArchive
import Network.AWS.StorageGateway.V2013_06_30.DeleteVolume
import Network.AWS.StorageGateway.V2013_06_30.DescribeBandwidthRateLimit
import Network.AWS.StorageGateway.V2013_06_30.DescribeCache
import Network.AWS.StorageGateway.V2013_06_30.DescribeCachediSCSIVolumes
import Network.AWS.StorageGateway.V2013_06_30.DescribeChapCredentials
import Network.AWS.StorageGateway.V2013_06_30.DescribeGatewayInformation
import Network.AWS.StorageGateway.V2013_06_30.DescribeMaintenanceStartTime
import Network.AWS.StorageGateway.V2013_06_30.DescribeSnapshotSchedule
import Network.AWS.StorageGateway.V2013_06_30.DescribeStorediSCSIVolumes
import Network.AWS.StorageGateway.V2013_06_30.DescribeTapeArchives
import Network.AWS.StorageGateway.V2013_06_30.DescribeTapeRecoveryPoints
import Network.AWS.StorageGateway.V2013_06_30.DescribeTapes
import Network.AWS.StorageGateway.V2013_06_30.DescribeUploadBuffer
import Network.AWS.StorageGateway.V2013_06_30.DescribeVTLDevices
import Network.AWS.StorageGateway.V2013_06_30.DescribeWorkingStorage
import Network.AWS.StorageGateway.V2013_06_30.DisableGateway
import Network.AWS.StorageGateway.V2013_06_30.ListGateways
import Network.AWS.StorageGateway.V2013_06_30.ListLocalDisks
import Network.AWS.StorageGateway.V2013_06_30.ListVolumeRecoveryPoints
import Network.AWS.StorageGateway.V2013_06_30.ListVolumes
import Network.AWS.StorageGateway.V2013_06_30.RetrieveTapeArchive
import Network.AWS.StorageGateway.V2013_06_30.RetrieveTapeRecoveryPoint
import Network.AWS.StorageGateway.V2013_06_30.ShutdownGateway
import Network.AWS.StorageGateway.V2013_06_30.StartGateway
import Network.AWS.StorageGateway.V2013_06_30.Types
import Network.AWS.StorageGateway.V2013_06_30.UpdateBandwidthRateLimit
import Network.AWS.StorageGateway.V2013_06_30.UpdateChapCredentials
import Network.AWS.StorageGateway.V2013_06_30.UpdateGatewayInformation
import Network.AWS.StorageGateway.V2013_06_30.UpdateGatewaySoftwareNow
import Network.AWS.StorageGateway.V2013_06_30.UpdateMaintenanceStartTime
import Network.AWS.StorageGateway.V2013_06_30.UpdateSnapshotSchedule
