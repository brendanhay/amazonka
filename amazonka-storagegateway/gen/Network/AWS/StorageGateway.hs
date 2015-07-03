-- Module      : Network.AWS.StorageGateway
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | AWS Storage Gateway Service
--
-- AWS Storage Gateway is the service that connects an on-premises software
-- appliance with cloud-based storage to provide seamless and secure
-- integration between an organization\'s on-premises IT environment and
-- AWS\'s storage infrastructure. The service enables you to securely
-- upload data to the AWS cloud for cost effective backup and rapid
-- disaster recovery.
--
-- Use the following links to get started using the /AWS Storage Gateway
-- Service API Reference/:
--
-- -   <http://docs.aws.amazon.com/storagegateway/latest/userguide/AWSStorageGatewayHTTPRequestsHeaders.html AWS Storage Gateway Required Request Headers>:
--     Describes the required headers that you must send with every POST
--     request to AWS Storage Gateway.
-- -   <http://docs.aws.amazon.com/storagegateway/latest/userguide/AWSStorageGatewaySigningRequests.html Signing Requests>:
--     AWS Storage Gateway requires that you authenticate every request you
--     send; this topic describes how sign such a request.
-- -   <http://docs.aws.amazon.com/storagegateway/latest/userguide/APIErrorResponses.html Error Responses>:
--     Provides reference information about AWS Storage Gateway errors.
-- -   <http://docs.aws.amazon.com/storagegateway/latest/userguide/AWSStorageGatewayAPIOperations.html Operations in AWS Storage Gateway>:
--     Contains detailed descriptions of all AWS Storage Gateway
--     operations, their request parameters, response elements, possible
--     errors, and examples of requests and responses.
-- -   <http://docs.aws.amazon.com/general/latest/gr/index.html?rande.html AWS Storage Gateway Regions and Endpoints>:
--     Provides a list of each of the regions and endpoints available for
--     use with AWS Storage Gateway.
module Network.AWS.StorageGateway
    ( module Export
    ) where

import           Network.AWS.StorageGateway.ActivateGateway                       as Export
import           Network.AWS.StorageGateway.AddCache                              as Export
import           Network.AWS.StorageGateway.AddUploadBuffer                       as Export
import           Network.AWS.StorageGateway.AddWorkingStorage                     as Export
import           Network.AWS.StorageGateway.CancelArchival                        as Export
import           Network.AWS.StorageGateway.CancelRetrieval                       as Export
import           Network.AWS.StorageGateway.CreateCachediSCSIVolume               as Export
import           Network.AWS.StorageGateway.CreateSnapshot                        as Export
import           Network.AWS.StorageGateway.CreateSnapshotFromVolumeRecoveryPoint as Export
import           Network.AWS.StorageGateway.CreateStorediSCSIVolume               as Export
import           Network.AWS.StorageGateway.CreateTapes                           as Export
import           Network.AWS.StorageGateway.DeleteBandwidthRateLimit              as Export
import           Network.AWS.StorageGateway.DeleteChapCredentials                 as Export
import           Network.AWS.StorageGateway.DeleteGateway                         as Export
import           Network.AWS.StorageGateway.DeleteSnapshotSchedule                as Export
import           Network.AWS.StorageGateway.DeleteTape                            as Export
import           Network.AWS.StorageGateway.DeleteTapeArchive                     as Export
import           Network.AWS.StorageGateway.DeleteVolume                          as Export
import           Network.AWS.StorageGateway.DescribeBandwidthRateLimit            as Export
import           Network.AWS.StorageGateway.DescribeCache                         as Export
import           Network.AWS.StorageGateway.DescribeCachediSCSIVolumes            as Export
import           Network.AWS.StorageGateway.DescribeChapCredentials               as Export
import           Network.AWS.StorageGateway.DescribeGatewayInformation            as Export
import           Network.AWS.StorageGateway.DescribeMaintenanceStartTime          as Export
import           Network.AWS.StorageGateway.DescribeSnapshotSchedule              as Export
import           Network.AWS.StorageGateway.DescribeStorediSCSIVolumes            as Export
import           Network.AWS.StorageGateway.DescribeTapeArchives                  as Export
import           Network.AWS.StorageGateway.DescribeTapeRecoveryPoints            as Export
import           Network.AWS.StorageGateway.DescribeTapes                         as Export
import           Network.AWS.StorageGateway.DescribeUploadBuffer                  as Export
import           Network.AWS.StorageGateway.DescribeVTLDevices                    as Export
import           Network.AWS.StorageGateway.DescribeWorkingStorage                as Export
import           Network.AWS.StorageGateway.DisableGateway                        as Export
import           Network.AWS.StorageGateway.ListGateways                          as Export
import           Network.AWS.StorageGateway.ListLocalDisks                        as Export
import           Network.AWS.StorageGateway.ListVolumeInitiators                  as Export
import           Network.AWS.StorageGateway.ListVolumeRecoveryPoints              as Export
import           Network.AWS.StorageGateway.ListVolumes                           as Export
import           Network.AWS.StorageGateway.ResetCache                            as Export
import           Network.AWS.StorageGateway.RetrieveTapeArchive                   as Export
import           Network.AWS.StorageGateway.RetrieveTapeRecoveryPoint             as Export
import           Network.AWS.StorageGateway.ShutdownGateway                       as Export
import           Network.AWS.StorageGateway.StartGateway                          as Export
import           Network.AWS.StorageGateway.Types                                 as Export
import           Network.AWS.StorageGateway.UpdateBandwidthRateLimit              as Export
import           Network.AWS.StorageGateway.UpdateChapCredentials                 as Export
import           Network.AWS.StorageGateway.UpdateGatewayInformation              as Export
import           Network.AWS.StorageGateway.UpdateGatewaySoftwareNow              as Export
import           Network.AWS.StorageGateway.UpdateMaintenanceStartTime            as Export
import           Network.AWS.StorageGateway.UpdateSnapshotSchedule                as Export
import           Network.AWS.StorageGateway.UpdateVTLDeviceType                   as Export
import           Network.AWS.StorageGateway.Waiters                               as Export
