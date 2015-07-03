-- Module      : Network.AWS.Glacier
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

-- | Amazon Glacier is a storage solution for \"cold data.\"
--
-- Amazon Glacier is an extremely low-cost storage service that provides
-- secure, durable, and easy-to-use storage for data backup and archival.
-- With Amazon Glacier, customers can store their data cost effectively for
-- months, years, or decades. Amazon Glacier also enables customers to
-- offload the administrative burdens of operating and scaling storage to
-- AWS, so they don\'t have to worry about capacity planning, hardware
-- provisioning, data replication, hardware failure and recovery, or
-- time-consuming hardware migrations.
--
-- Amazon Glacier is a great storage choice when low storage cost is
-- paramount, your data is rarely retrieved, and retrieval latency of
-- several hours is acceptable. If your application requires fast or
-- frequent access to your data, consider using Amazon S3. For more
-- information, go to
-- <http://aws.amazon.com/s3/ Amazon Simple Storage Service (Amazon S3)>.
--
-- You can store any kind of data in any format. There is no maximum limit
-- on the total amount of data you can store in Amazon Glacier.
--
-- If you are a first-time user of Amazon Glacier, we recommend that you
-- begin by reading the following sections in the /Amazon Glacier Developer
-- Guide/:
--
-- -   <http://docs.aws.amazon.com/amazonglacier/latest/dev/introduction.html What is Amazon Glacier>
--     - This section of the Developer Guide describes the underlying data
--     model, the operations it supports, and the AWS SDKs that you can use
--     to interact with the service.
--
-- -   <http://docs.aws.amazon.com/amazonglacier/latest/dev/amazon-glacier-getting-started.html Getting Started with Amazon Glacier>
--     - The Getting Started section walks you through the process of
--     creating a vault, uploading archives, creating jobs to download
--     archives, retrieving the job output, and deleting archives.
--
module Network.AWS.Glacier
    ( module Export
    ) where

import           Network.AWS.Glacier.AbortMultipartUpload     as Export
import           Network.AWS.Glacier.AddTagsToVault           as Export
import           Network.AWS.Glacier.CompleteMultipartUpload  as Export
import           Network.AWS.Glacier.CreateVault              as Export
import           Network.AWS.Glacier.DeleteArchive            as Export
import           Network.AWS.Glacier.DeleteVault              as Export
import           Network.AWS.Glacier.DeleteVaultAccessPolicy  as Export
import           Network.AWS.Glacier.DeleteVaultNotifications as Export
import           Network.AWS.Glacier.DescribeJob              as Export
import           Network.AWS.Glacier.DescribeVault            as Export
import           Network.AWS.Glacier.GetDataRetrievalPolicy   as Export
import           Network.AWS.Glacier.GetJobOutput             as Export
import           Network.AWS.Glacier.GetVaultAccessPolicy     as Export
import           Network.AWS.Glacier.GetVaultNotifications    as Export
import           Network.AWS.Glacier.InitiateJob              as Export
import           Network.AWS.Glacier.InitiateMultipartUpload  as Export
import           Network.AWS.Glacier.ListJobs                 as Export
import           Network.AWS.Glacier.ListMultipartUploads     as Export
import           Network.AWS.Glacier.ListParts                as Export
import           Network.AWS.Glacier.ListTagsForVault         as Export
import           Network.AWS.Glacier.ListVaults               as Export
import           Network.AWS.Glacier.RemoveTagsFromVault      as Export
import           Network.AWS.Glacier.SetDataRetrievalPolicy   as Export
import           Network.AWS.Glacier.SetVaultAccessPolicy     as Export
import           Network.AWS.Glacier.SetVaultNotifications    as Export
import           Network.AWS.Glacier.Types                    as Export
import           Network.AWS.Glacier.UploadArchive            as Export
import           Network.AWS.Glacier.UploadMultipartPart      as Export
import           Network.AWS.Glacier.Waiters                  as Export
