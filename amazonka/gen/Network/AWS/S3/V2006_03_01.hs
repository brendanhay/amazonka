-- Module      : Network.AWS.S3.V2006_03_01
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Simple Storage Service is storage for the Internet. Amazon S3 has a
-- simple web services interface that you can use to store and retrieve any
-- amount of data, at any time, from anywhere on the web. It gives any
-- developer access to the same highly scalable, reliable, fast, inexpensive
-- data storage infrastructure that Amazon uses to run its own global network
-- of web sites. The service aims to maximize benefits of scale and to pass
-- those benefits on to developers.
module Network.AWS.S3.V2006_03_01 (module Export) where

import Network.AWS.S3.V2006_03_01.AbortMultipartUpload as Export
import Network.AWS.S3.V2006_03_01.CompleteMultipartUpload as Export
import Network.AWS.S3.V2006_03_01.CopyObject as Export
import Network.AWS.S3.V2006_03_01.CreateBucket as Export
import Network.AWS.S3.V2006_03_01.CreateMultipartUpload as Export
import Network.AWS.S3.V2006_03_01.DeleteBucket as Export
import Network.AWS.S3.V2006_03_01.DeleteBucketCors as Export
import Network.AWS.S3.V2006_03_01.DeleteBucketLifecycle as Export
import Network.AWS.S3.V2006_03_01.DeleteBucketPolicy as Export
import Network.AWS.S3.V2006_03_01.DeleteBucketTagging as Export
import Network.AWS.S3.V2006_03_01.DeleteBucketWebsite as Export
import Network.AWS.S3.V2006_03_01.DeleteObject as Export
import Network.AWS.S3.V2006_03_01.DeleteObjects as Export
import Network.AWS.S3.V2006_03_01.GetBucketAcl as Export
import Network.AWS.S3.V2006_03_01.GetBucketCors as Export
import Network.AWS.S3.V2006_03_01.GetBucketLifecycle as Export
import Network.AWS.S3.V2006_03_01.GetBucketLocation as Export
import Network.AWS.S3.V2006_03_01.GetBucketLogging as Export
import Network.AWS.S3.V2006_03_01.GetBucketNotification as Export
import Network.AWS.S3.V2006_03_01.GetBucketPolicy as Export
import Network.AWS.S3.V2006_03_01.GetBucketRequestPayment as Export
import Network.AWS.S3.V2006_03_01.GetBucketTagging as Export
import Network.AWS.S3.V2006_03_01.GetBucketVersioning as Export
import Network.AWS.S3.V2006_03_01.GetBucketWebsite as Export
import Network.AWS.S3.V2006_03_01.GetObject as Export
import Network.AWS.S3.V2006_03_01.GetObjectAcl as Export
import Network.AWS.S3.V2006_03_01.GetObjectTorrent as Export
import Network.AWS.S3.V2006_03_01.HeadBucket as Export
import Network.AWS.S3.V2006_03_01.HeadObject as Export
import Network.AWS.S3.V2006_03_01.ListBuckets as Export
import Network.AWS.S3.V2006_03_01.ListMultipartUploads as Export
import Network.AWS.S3.V2006_03_01.ListObjectVersions as Export
import Network.AWS.S3.V2006_03_01.ListObjects as Export
import Network.AWS.S3.V2006_03_01.ListParts as Export
import Network.AWS.S3.V2006_03_01.PutBucketAcl as Export
import Network.AWS.S3.V2006_03_01.PutBucketCors as Export
import Network.AWS.S3.V2006_03_01.PutBucketLifecycle as Export
import Network.AWS.S3.V2006_03_01.PutBucketLogging as Export
import Network.AWS.S3.V2006_03_01.PutBucketNotification as Export
import Network.AWS.S3.V2006_03_01.PutBucketPolicy as Export
import Network.AWS.S3.V2006_03_01.PutBucketRequestPayment as Export
import Network.AWS.S3.V2006_03_01.PutBucketTagging as Export
import Network.AWS.S3.V2006_03_01.PutBucketVersioning as Export
import Network.AWS.S3.V2006_03_01.PutBucketWebsite as Export
import Network.AWS.S3.V2006_03_01.PutObject as Export
import Network.AWS.S3.V2006_03_01.PutObjectAcl as Export
import Network.AWS.S3.V2006_03_01.RestoreObject as Export
import Network.AWS.S3.V2006_03_01.Types as Export
import Network.AWS.S3.V2006_03_01.UploadPart as Export
import Network.AWS.S3.V2006_03_01.UploadPartCopy as Export
