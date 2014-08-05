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
module Network.AWS.StorageGateway.V2013_06_30 (module Export) where

import Network.AWS.StorageGateway.V2013_06_30.ActivateGateway as Export
import Network.AWS.StorageGateway.V2013_06_30.AddCache as Export
import Network.AWS.StorageGateway.V2013_06_30.AddUploadBuffer as Export
import Network.AWS.StorageGateway.V2013_06_30.AddWorkingStorage as Export
import Network.AWS.StorageGateway.V2013_06_30.CancelArchival as Export
import Network.AWS.StorageGateway.V2013_06_30.CancelRetrieval as Export
import Network.AWS.StorageGateway.V2013_06_30.CreateCachediSCSIVolume as Export
import Network.AWS.StorageGateway.V2013_06_30.CreateSnapshot as Export
import Network.AWS.StorageGateway.V2013_06_30.CreateSnapshotFromVolumeRecoveryPoint as Export
import Network.AWS.StorageGateway.V2013_06_30.CreateStorediSCSIVolume as Export
import Network.AWS.StorageGateway.V2013_06_30.CreateTapes as Export
import Network.AWS.StorageGateway.V2013_06_30.DeleteBandwidthRateLimit as Export
import Network.AWS.StorageGateway.V2013_06_30.DeleteChapCredentials as Export
import Network.AWS.StorageGateway.V2013_06_30.DeleteGateway as Export
import Network.AWS.StorageGateway.V2013_06_30.DeleteSnapshotSchedule as Export
import Network.AWS.StorageGateway.V2013_06_30.DeleteTape as Export
import Network.AWS.StorageGateway.V2013_06_30.DeleteTapeArchive as Export
import Network.AWS.StorageGateway.V2013_06_30.DeleteVolume as Export
import Network.AWS.StorageGateway.V2013_06_30.DescribeBandwidthRateLimit as Export
import Network.AWS.StorageGateway.V2013_06_30.DescribeCache as Export
import Network.AWS.StorageGateway.V2013_06_30.DescribeCachediSCSIVolumes as Export
import Network.AWS.StorageGateway.V2013_06_30.DescribeChapCredentials as Export
import Network.AWS.StorageGateway.V2013_06_30.DescribeGatewayInformation as Export
import Network.AWS.StorageGateway.V2013_06_30.DescribeMaintenanceStartTime as Export
import Network.AWS.StorageGateway.V2013_06_30.DescribeSnapshotSchedule as Export
import Network.AWS.StorageGateway.V2013_06_30.DescribeStorediSCSIVolumes as Export
import Network.AWS.StorageGateway.V2013_06_30.DescribeTapeArchives as Export
import Network.AWS.StorageGateway.V2013_06_30.DescribeTapeRecoveryPoints as Export
import Network.AWS.StorageGateway.V2013_06_30.DescribeTapes as Export
import Network.AWS.StorageGateway.V2013_06_30.DescribeUploadBuffer as Export
import Network.AWS.StorageGateway.V2013_06_30.DescribeVTLDevices as Export
import Network.AWS.StorageGateway.V2013_06_30.DescribeWorkingStorage as Export
import Network.AWS.StorageGateway.V2013_06_30.DisableGateway as Export
import Network.AWS.StorageGateway.V2013_06_30.ListGateways as Export
import Network.AWS.StorageGateway.V2013_06_30.ListLocalDisks as Export
import Network.AWS.StorageGateway.V2013_06_30.ListVolumeRecoveryPoints as Export
import Network.AWS.StorageGateway.V2013_06_30.ListVolumes as Export
import Network.AWS.StorageGateway.V2013_06_30.RetrieveTapeArchive as Export
import Network.AWS.StorageGateway.V2013_06_30.RetrieveTapeRecoveryPoint as Export
import Network.AWS.StorageGateway.V2013_06_30.ShutdownGateway as Export
import Network.AWS.StorageGateway.V2013_06_30.StartGateway as Export
import Network.AWS.StorageGateway.V2013_06_30.Types as Export
import Network.AWS.StorageGateway.V2013_06_30.UpdateBandwidthRateLimit as Export
import Network.AWS.StorageGateway.V2013_06_30.UpdateChapCredentials as Export
import Network.AWS.StorageGateway.V2013_06_30.UpdateGatewayInformation as Export
import Network.AWS.StorageGateway.V2013_06_30.UpdateGatewaySoftwareNow as Export
import Network.AWS.StorageGateway.V2013_06_30.UpdateMaintenanceStartTime as Export
import Network.AWS.StorageGateway.V2013_06_30.UpdateSnapshotSchedule as Export
