{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.S3.V2006_03_01
    ( module Network.AWS.S3.V2006_03_01.AbortMultipartUpload
    , module Network.AWS.S3.V2006_03_01.CompleteMultipartUpload
    , module Network.AWS.S3.V2006_03_01.CopyObject
    , module Network.AWS.S3.V2006_03_01.CreateBucket
    , module Network.AWS.S3.V2006_03_01.CreateMultipartUpload
    , module Network.AWS.S3.V2006_03_01.DeleteBucket
    , module Network.AWS.S3.V2006_03_01.DeleteBucketCors
    , module Network.AWS.S3.V2006_03_01.DeleteBucketLifecycle
    , module Network.AWS.S3.V2006_03_01.DeleteBucketPolicy
    , module Network.AWS.S3.V2006_03_01.DeleteBucketTagging
    , module Network.AWS.S3.V2006_03_01.DeleteBucketWebsite
    , module Network.AWS.S3.V2006_03_01.DeleteObject
    , module Network.AWS.S3.V2006_03_01.DeleteObjects
    , module Network.AWS.S3.V2006_03_01.GetBucketAcl
    , module Network.AWS.S3.V2006_03_01.GetBucketCors
    , module Network.AWS.S3.V2006_03_01.GetBucketLifecycle
    , module Network.AWS.S3.V2006_03_01.GetBucketLocation
    , module Network.AWS.S3.V2006_03_01.GetBucketLogging
    , module Network.AWS.S3.V2006_03_01.GetBucketNotification
    , module Network.AWS.S3.V2006_03_01.GetBucketPolicy
    , module Network.AWS.S3.V2006_03_01.GetBucketRequestPayment
    , module Network.AWS.S3.V2006_03_01.GetBucketTagging
    , module Network.AWS.S3.V2006_03_01.GetBucketVersioning
    , module Network.AWS.S3.V2006_03_01.GetBucketWebsite
    , module Network.AWS.S3.V2006_03_01.GetObject
    , module Network.AWS.S3.V2006_03_01.GetObjectAcl
    , module Network.AWS.S3.V2006_03_01.GetObjectTorrent
    , module Network.AWS.S3.V2006_03_01.HeadBucket
    , module Network.AWS.S3.V2006_03_01.HeadObject
    , module Network.AWS.S3.V2006_03_01.Lenses
    , module Network.AWS.S3.V2006_03_01.ListBuckets
    , module Network.AWS.S3.V2006_03_01.ListMultipartUploads
    , module Network.AWS.S3.V2006_03_01.ListObjectVersions
    , module Network.AWS.S3.V2006_03_01.ListObjects
    , module Network.AWS.S3.V2006_03_01.ListParts
    , module Network.AWS.S3.V2006_03_01.PutBucketAcl
    , module Network.AWS.S3.V2006_03_01.PutBucketCors
    , module Network.AWS.S3.V2006_03_01.PutBucketLifecycle
    , module Network.AWS.S3.V2006_03_01.PutBucketLogging
    , module Network.AWS.S3.V2006_03_01.PutBucketNotification
    , module Network.AWS.S3.V2006_03_01.PutBucketPolicy
    , module Network.AWS.S3.V2006_03_01.PutBucketRequestPayment
    , module Network.AWS.S3.V2006_03_01.PutBucketTagging
    , module Network.AWS.S3.V2006_03_01.PutBucketVersioning
    , module Network.AWS.S3.V2006_03_01.PutBucketWebsite
    , module Network.AWS.S3.V2006_03_01.PutObject
    , module Network.AWS.S3.V2006_03_01.PutObjectAcl
    , module Network.AWS.S3.V2006_03_01.RestoreObject
    , module Network.AWS.S3.V2006_03_01.Types
    , module Network.AWS.S3.V2006_03_01.UploadPart
    , module Network.AWS.S3.V2006_03_01.UploadPartCopy
    ) where

import Network.AWS.S3.V2006_03_01.AbortMultipartUpload
import Network.AWS.S3.V2006_03_01.CompleteMultipartUpload
import Network.AWS.S3.V2006_03_01.CopyObject
import Network.AWS.S3.V2006_03_01.CreateBucket
import Network.AWS.S3.V2006_03_01.CreateMultipartUpload
import Network.AWS.S3.V2006_03_01.DeleteBucket
import Network.AWS.S3.V2006_03_01.DeleteBucketCors
import Network.AWS.S3.V2006_03_01.DeleteBucketLifecycle
import Network.AWS.S3.V2006_03_01.DeleteBucketPolicy
import Network.AWS.S3.V2006_03_01.DeleteBucketTagging
import Network.AWS.S3.V2006_03_01.DeleteBucketWebsite
import Network.AWS.S3.V2006_03_01.DeleteObject
import Network.AWS.S3.V2006_03_01.DeleteObjects
import Network.AWS.S3.V2006_03_01.GetBucketAcl
import Network.AWS.S3.V2006_03_01.GetBucketCors
import Network.AWS.S3.V2006_03_01.GetBucketLifecycle
import Network.AWS.S3.V2006_03_01.GetBucketLocation
import Network.AWS.S3.V2006_03_01.GetBucketLogging
import Network.AWS.S3.V2006_03_01.GetBucketNotification
import Network.AWS.S3.V2006_03_01.GetBucketPolicy
import Network.AWS.S3.V2006_03_01.GetBucketRequestPayment
import Network.AWS.S3.V2006_03_01.GetBucketTagging
import Network.AWS.S3.V2006_03_01.GetBucketVersioning
import Network.AWS.S3.V2006_03_01.GetBucketWebsite
import Network.AWS.S3.V2006_03_01.GetObject
import Network.AWS.S3.V2006_03_01.GetObjectAcl
import Network.AWS.S3.V2006_03_01.GetObjectTorrent
import Network.AWS.S3.V2006_03_01.HeadBucket
import Network.AWS.S3.V2006_03_01.HeadObject
import Network.AWS.S3.V2006_03_01.Lenses
import Network.AWS.S3.V2006_03_01.ListBuckets
import Network.AWS.S3.V2006_03_01.ListMultipartUploads
import Network.AWS.S3.V2006_03_01.ListObjectVersions
import Network.AWS.S3.V2006_03_01.ListObjects
import Network.AWS.S3.V2006_03_01.ListParts
import Network.AWS.S3.V2006_03_01.PutBucketAcl
import Network.AWS.S3.V2006_03_01.PutBucketCors
import Network.AWS.S3.V2006_03_01.PutBucketLifecycle
import Network.AWS.S3.V2006_03_01.PutBucketLogging
import Network.AWS.S3.V2006_03_01.PutBucketNotification
import Network.AWS.S3.V2006_03_01.PutBucketPolicy
import Network.AWS.S3.V2006_03_01.PutBucketRequestPayment
import Network.AWS.S3.V2006_03_01.PutBucketTagging
import Network.AWS.S3.V2006_03_01.PutBucketVersioning
import Network.AWS.S3.V2006_03_01.PutBucketWebsite
import Network.AWS.S3.V2006_03_01.PutObject
import Network.AWS.S3.V2006_03_01.PutObjectAcl
import Network.AWS.S3.V2006_03_01.RestoreObject
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.S3.V2006_03_01.UploadPart
import Network.AWS.S3.V2006_03_01.UploadPartCopy
