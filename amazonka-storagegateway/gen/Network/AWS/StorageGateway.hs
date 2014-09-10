-- Module      : Network.AWS.StorageGateway
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
module Network.AWS.StorageGateway
    ( module Network.AWS.StorageGateway.ActivateGateway
    , module Network.AWS.StorageGateway.AddCache
    , module Network.AWS.StorageGateway.AddUploadBuffer
    , module Network.AWS.StorageGateway.AddWorkingStorage
    , module Network.AWS.StorageGateway.CancelArchival
    , module Network.AWS.StorageGateway.CancelRetrieval
    , module Network.AWS.StorageGateway.CreateCachediSCSIVolume
    , module Network.AWS.StorageGateway.CreateSnapshot
    , module Network.AWS.StorageGateway.CreateSnapshotFromVolumeRecoveryPoint
    , module Network.AWS.StorageGateway.CreateStorediSCSIVolume
    , module Network.AWS.StorageGateway.CreateTapes
    , module Network.AWS.StorageGateway.DeleteBandwidthRateLimit
    , module Network.AWS.StorageGateway.DeleteChapCredentials
    , module Network.AWS.StorageGateway.DeleteGateway
    , module Network.AWS.StorageGateway.DeleteSnapshotSchedule
    , module Network.AWS.StorageGateway.DeleteTape
    , module Network.AWS.StorageGateway.DeleteTapeArchive
    , module Network.AWS.StorageGateway.DeleteVolume
    , module Network.AWS.StorageGateway.DescribeBandwidthRateLimit
    , module Network.AWS.StorageGateway.DescribeCache
    , module Network.AWS.StorageGateway.DescribeCachediSCSIVolumes
    , module Network.AWS.StorageGateway.DescribeChapCredentials
    , module Network.AWS.StorageGateway.DescribeGatewayInformation
    , module Network.AWS.StorageGateway.DescribeMaintenanceStartTime
    , module Network.AWS.StorageGateway.DescribeSnapshotSchedule
    , module Network.AWS.StorageGateway.DescribeStorediSCSIVolumes
    , module Network.AWS.StorageGateway.DescribeTapeArchives
    , module Network.AWS.StorageGateway.DescribeTapeRecoveryPoints
    , module Network.AWS.StorageGateway.DescribeTapes
    , module Network.AWS.StorageGateway.DescribeUploadBuffer
    , module Network.AWS.StorageGateway.DescribeVTLDevices
    , module Network.AWS.StorageGateway.DescribeWorkingStorage
    , module Network.AWS.StorageGateway.DisableGateway
    , module Network.AWS.StorageGateway.ListGateways
    , module Network.AWS.StorageGateway.ListLocalDisks
    , module Network.AWS.StorageGateway.ListVolumeRecoveryPoints
    , module Network.AWS.StorageGateway.ListVolumes
    , module Network.AWS.StorageGateway.Monadic
    , module Network.AWS.StorageGateway.RetrieveTapeArchive
    , module Network.AWS.StorageGateway.RetrieveTapeRecoveryPoint
    , module Network.AWS.StorageGateway.ShutdownGateway
    , module Network.AWS.StorageGateway.StartGateway
    , module Network.AWS.StorageGateway.Types
    , module Network.AWS.StorageGateway.UpdateBandwidthRateLimit
    , module Network.AWS.StorageGateway.UpdateChapCredentials
    , module Network.AWS.StorageGateway.UpdateGatewayInformation
    , module Network.AWS.StorageGateway.UpdateGatewaySoftwareNow
    , module Network.AWS.StorageGateway.UpdateMaintenanceStartTime
    , module Network.AWS.StorageGateway.UpdateSnapshotSchedule
    ) where

import Network.AWS.StorageGateway.ActivateGateway
import Network.AWS.StorageGateway.AddCache
import Network.AWS.StorageGateway.AddUploadBuffer
import Network.AWS.StorageGateway.AddWorkingStorage
import Network.AWS.StorageGateway.CancelArchival
import Network.AWS.StorageGateway.CancelRetrieval
import Network.AWS.StorageGateway.CreateCachediSCSIVolume
import Network.AWS.StorageGateway.CreateSnapshot
import Network.AWS.StorageGateway.CreateSnapshotFromVolumeRecoveryPoint
import Network.AWS.StorageGateway.CreateStorediSCSIVolume
import Network.AWS.StorageGateway.CreateTapes
import Network.AWS.StorageGateway.DeleteBandwidthRateLimit
import Network.AWS.StorageGateway.DeleteChapCredentials
import Network.AWS.StorageGateway.DeleteGateway
import Network.AWS.StorageGateway.DeleteSnapshotSchedule
import Network.AWS.StorageGateway.DeleteTape
import Network.AWS.StorageGateway.DeleteTapeArchive
import Network.AWS.StorageGateway.DeleteVolume
import Network.AWS.StorageGateway.DescribeBandwidthRateLimit
import Network.AWS.StorageGateway.DescribeCache
import Network.AWS.StorageGateway.DescribeCachediSCSIVolumes
import Network.AWS.StorageGateway.DescribeChapCredentials
import Network.AWS.StorageGateway.DescribeGatewayInformation
import Network.AWS.StorageGateway.DescribeMaintenanceStartTime
import Network.AWS.StorageGateway.DescribeSnapshotSchedule
import Network.AWS.StorageGateway.DescribeStorediSCSIVolumes
import Network.AWS.StorageGateway.DescribeTapeArchives
import Network.AWS.StorageGateway.DescribeTapeRecoveryPoints
import Network.AWS.StorageGateway.DescribeTapes
import Network.AWS.StorageGateway.DescribeUploadBuffer
import Network.AWS.StorageGateway.DescribeVTLDevices
import Network.AWS.StorageGateway.DescribeWorkingStorage
import Network.AWS.StorageGateway.DisableGateway
import Network.AWS.StorageGateway.ListGateways
import Network.AWS.StorageGateway.ListLocalDisks
import Network.AWS.StorageGateway.ListVolumeRecoveryPoints
import Network.AWS.StorageGateway.ListVolumes
import Network.AWS.StorageGateway.Monadic
import Network.AWS.StorageGateway.RetrieveTapeArchive
import Network.AWS.StorageGateway.RetrieveTapeRecoveryPoint
import Network.AWS.StorageGateway.ShutdownGateway
import Network.AWS.StorageGateway.StartGateway
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.UpdateBandwidthRateLimit
import Network.AWS.StorageGateway.UpdateChapCredentials
import Network.AWS.StorageGateway.UpdateGatewayInformation
import Network.AWS.StorageGateway.UpdateGatewaySoftwareNow
import Network.AWS.StorageGateway.UpdateMaintenanceStartTime
import Network.AWS.StorageGateway.UpdateSnapshotSchedule
