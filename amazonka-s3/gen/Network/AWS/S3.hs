-- Module      : Network.AWS.S3
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

-- | Amazon Simple Storage Service is storage for the Internet. Amazon S3 has a
-- simple web services interface that you can use to store and retrieve any
-- amount of data, at any time, from anywhere on the web. It gives any developer
-- access to the same highly scalable, reliable, fast, inexpensive data storage
-- infrastructure that Amazon uses to run its own global network of web sites.
-- The service aims to maximize benefits of scale and to pass those benefits on
-- to developers.
module Network.AWS.S3
    ( module Network.AWS.S3.AbortMultipartUpload
    , module Network.AWS.S3.CompleteMultipartUpload
    , module Network.AWS.S3.CopyObject
    , module Network.AWS.S3.CreateBucket
    , module Network.AWS.S3.CreateMultipartUpload
    , module Network.AWS.S3.DeleteBucket
    , module Network.AWS.S3.DeleteBucketCors
    , module Network.AWS.S3.DeleteBucketLifecycle
    , module Network.AWS.S3.DeleteBucketPolicy
    , module Network.AWS.S3.DeleteBucketReplication
    , module Network.AWS.S3.DeleteBucketTagging
    , module Network.AWS.S3.DeleteBucketWebsite
    , module Network.AWS.S3.DeleteObject
    , module Network.AWS.S3.DeleteObjects
    , module Network.AWS.S3.GetBucketAcl
    , module Network.AWS.S3.GetBucketCors
    , module Network.AWS.S3.GetBucketLifecycle
    , module Network.AWS.S3.GetBucketLocation
    , module Network.AWS.S3.GetBucketLogging
    , module Network.AWS.S3.GetBucketNotification
    , module Network.AWS.S3.GetBucketNotificationConfiguration
    , module Network.AWS.S3.GetBucketPolicy
    , module Network.AWS.S3.GetBucketReplication
    , module Network.AWS.S3.GetBucketRequestPayment
    , module Network.AWS.S3.GetBucketTagging
    , module Network.AWS.S3.GetBucketVersioning
    , module Network.AWS.S3.GetBucketWebsite
    , module Network.AWS.S3.GetObject
    , module Network.AWS.S3.GetObjectAcl
    , module Network.AWS.S3.GetObjectTorrent
    , module Network.AWS.S3.HeadBucket
    , module Network.AWS.S3.HeadObject
    , module Network.AWS.S3.ListBuckets
    , module Network.AWS.S3.ListMultipartUploads
    , module Network.AWS.S3.ListObjectVersions
    , module Network.AWS.S3.ListObjects
    , module Network.AWS.S3.ListParts
    , module Network.AWS.S3.PutBucketAcl
    , module Network.AWS.S3.PutBucketCors
    , module Network.AWS.S3.PutBucketLifecycle
    , module Network.AWS.S3.PutBucketLogging
    , module Network.AWS.S3.PutBucketNotification
    , module Network.AWS.S3.PutBucketNotificationConfiguration
    , module Network.AWS.S3.PutBucketPolicy
    , module Network.AWS.S3.PutBucketReplication
    , module Network.AWS.S3.PutBucketRequestPayment
    , module Network.AWS.S3.PutBucketTagging
    , module Network.AWS.S3.PutBucketVersioning
    , module Network.AWS.S3.PutBucketWebsite
    , module Network.AWS.S3.PutObject
    , module Network.AWS.S3.PutObjectAcl
    , module Network.AWS.S3.RestoreObject
    , module Network.AWS.S3.Types
    , module Network.AWS.S3.UploadPart
    , module Network.AWS.S3.UploadPartCopy
    , module Network.AWS.S3.Waiters
    ) where

import Network.AWS.S3.AbortMultipartUpload
import Network.AWS.S3.CompleteMultipartUpload
import Network.AWS.S3.CopyObject
import Network.AWS.S3.CreateBucket
import Network.AWS.S3.CreateMultipartUpload
import Network.AWS.S3.DeleteBucket
import Network.AWS.S3.DeleteBucketCors
import Network.AWS.S3.DeleteBucketLifecycle
import Network.AWS.S3.DeleteBucketPolicy
import Network.AWS.S3.DeleteBucketReplication
import Network.AWS.S3.DeleteBucketTagging
import Network.AWS.S3.DeleteBucketWebsite
import Network.AWS.S3.DeleteObject
import Network.AWS.S3.DeleteObjects
import Network.AWS.S3.GetBucketAcl
import Network.AWS.S3.GetBucketCors
import Network.AWS.S3.GetBucketLifecycle
import Network.AWS.S3.GetBucketLocation
import Network.AWS.S3.GetBucketLogging
import Network.AWS.S3.GetBucketNotification
import Network.AWS.S3.GetBucketNotificationConfiguration
import Network.AWS.S3.GetBucketPolicy
import Network.AWS.S3.GetBucketReplication
import Network.AWS.S3.GetBucketRequestPayment
import Network.AWS.S3.GetBucketTagging
import Network.AWS.S3.GetBucketVersioning
import Network.AWS.S3.GetBucketWebsite
import Network.AWS.S3.GetObject
import Network.AWS.S3.GetObjectAcl
import Network.AWS.S3.GetObjectTorrent
import Network.AWS.S3.HeadBucket
import Network.AWS.S3.HeadObject
import Network.AWS.S3.ListBuckets
import Network.AWS.S3.ListMultipartUploads
import Network.AWS.S3.ListObjectVersions
import Network.AWS.S3.ListObjects
import Network.AWS.S3.ListParts
import Network.AWS.S3.PutBucketAcl
import Network.AWS.S3.PutBucketCors
import Network.AWS.S3.PutBucketLifecycle
import Network.AWS.S3.PutBucketLogging
import Network.AWS.S3.PutBucketNotification
import Network.AWS.S3.PutBucketNotificationConfiguration
import Network.AWS.S3.PutBucketPolicy
import Network.AWS.S3.PutBucketReplication
import Network.AWS.S3.PutBucketRequestPayment
import Network.AWS.S3.PutBucketTagging
import Network.AWS.S3.PutBucketVersioning
import Network.AWS.S3.PutBucketWebsite
import Network.AWS.S3.PutObject
import Network.AWS.S3.PutObjectAcl
import Network.AWS.S3.RestoreObject
import Network.AWS.S3.Types
import Network.AWS.S3.UploadPart
import Network.AWS.S3.UploadPartCopy
import Network.AWS.S3.Waiters
