-- Module      : Network.AWS.Glacier
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Amazon Glacier is a secure, durable, and extremely low-cost storage service
-- for data archiving and online backup. Customers can reliably store large or
-- small amounts of data for as little as $0.01 per gigabyte per month, a
-- significant savings compared to on-premises solutions. To keep costs low,
-- Amazon Glacier is optimized for infrequently accessed data where a retrieval
-- time of several hours is suitable.
module Network.AWS.Glacier
    ( module Network.AWS.Glacier.AbortMultipartUpload
    , module Network.AWS.Glacier.CompleteMultipartUpload
    , module Network.AWS.Glacier.CreateVault
    , module Network.AWS.Glacier.DeleteArchive
    , module Network.AWS.Glacier.DeleteVault
    , module Network.AWS.Glacier.DeleteVaultNotifications
    , module Network.AWS.Glacier.DescribeJob
    , module Network.AWS.Glacier.DescribeVault
    , module Network.AWS.Glacier.GetDataRetrievalPolicy
    , module Network.AWS.Glacier.GetJobOutput
    , module Network.AWS.Glacier.GetVaultNotifications
    , module Network.AWS.Glacier.InitiateJob
    , module Network.AWS.Glacier.InitiateMultipartUpload
    , module Network.AWS.Glacier.ListJobs
    , module Network.AWS.Glacier.ListMultipartUploads
    , module Network.AWS.Glacier.ListParts
    , module Network.AWS.Glacier.ListVaults
    , module Network.AWS.Glacier.SetDataRetrievalPolicy
    , module Network.AWS.Glacier.SetVaultNotifications
    , module Network.AWS.Glacier.Types
    , module Network.AWS.Glacier.UploadArchive
    , module Network.AWS.Glacier.UploadMultipartPart
    ) where

import Network.AWS.Glacier.AbortMultipartUpload
import Network.AWS.Glacier.CompleteMultipartUpload
import Network.AWS.Glacier.CreateVault
import Network.AWS.Glacier.DeleteArchive
import Network.AWS.Glacier.DeleteVault
import Network.AWS.Glacier.DeleteVaultNotifications
import Network.AWS.Glacier.DescribeJob
import Network.AWS.Glacier.DescribeVault
import Network.AWS.Glacier.GetDataRetrievalPolicy
import Network.AWS.Glacier.GetJobOutput
import Network.AWS.Glacier.GetVaultNotifications
import Network.AWS.Glacier.InitiateJob
import Network.AWS.Glacier.InitiateMultipartUpload
import Network.AWS.Glacier.ListJobs
import Network.AWS.Glacier.ListMultipartUploads
import Network.AWS.Glacier.ListParts
import Network.AWS.Glacier.ListVaults
import Network.AWS.Glacier.SetDataRetrievalPolicy
import Network.AWS.Glacier.SetVaultNotifications
import Network.AWS.Glacier.Types
import Network.AWS.Glacier.UploadArchive
import Network.AWS.Glacier.UploadMultipartPart
