{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Amazon Simple Storage Service is storage for the Internet. Amazon S3 has
-- a simple web services interface that you can use to store and retrieve
-- any amount of data, at any time, from anywhere on the web. It gives any
-- developer access to the same highly scalable, reliable, fast,
-- inexpensive data storage infrastructure that Amazon uses to run its own
-- global network of web sites. The service aims to maximize benefits of
-- scale and to pass those benefits on to developers.
module Network.AWS.S3
    ( module Export
    ) where

import           Network.AWS.S3.AbortMultipartUpload               as Export
import           Network.AWS.S3.CompleteMultipartUpload            as Export
import           Network.AWS.S3.CopyObject                         as Export
import           Network.AWS.S3.CreateBucket                       as Export
import           Network.AWS.S3.CreateMultipartUpload              as Export
import           Network.AWS.S3.DeleteBucket                       as Export
import           Network.AWS.S3.DeleteBucketCORS                   as Export
import           Network.AWS.S3.DeleteBucketLifecycle              as Export
import           Network.AWS.S3.DeleteBucketPolicy                 as Export
import           Network.AWS.S3.DeleteBucketReplication            as Export
import           Network.AWS.S3.DeleteBucketTagging                as Export
import           Network.AWS.S3.DeleteBucketWebsite                as Export
import           Network.AWS.S3.DeleteObject                       as Export
import           Network.AWS.S3.DeleteObjects                      as Export
import           Network.AWS.S3.GetBucketACL                       as Export
import           Network.AWS.S3.GetBucketCORS                      as Export
import           Network.AWS.S3.GetBucketLifecycle                 as Export
import           Network.AWS.S3.GetBucketLocation                  as Export
import           Network.AWS.S3.GetBucketLogging                   as Export
import           Network.AWS.S3.GetBucketNotificationConfiguration as Export
import           Network.AWS.S3.GetBucketPolicy                    as Export
import           Network.AWS.S3.GetBucketReplication               as Export
import           Network.AWS.S3.GetBucketRequestPayment            as Export
import           Network.AWS.S3.GetBucketTagging                   as Export
import           Network.AWS.S3.GetBucketVersioning                as Export
import           Network.AWS.S3.GetBucketWebsite                   as Export
import           Network.AWS.S3.GetObject                          as Export
import           Network.AWS.S3.GetObjectACL                       as Export
import           Network.AWS.S3.GetObjectTorrent                   as Export
import           Network.AWS.S3.HeadBucket                         as Export
import           Network.AWS.S3.HeadObject                         as Export
import           Network.AWS.S3.Internal                           as Export
import           Network.AWS.S3.ListBuckets                        as Export
import           Network.AWS.S3.ListMultipartUploads               as Export
import           Network.AWS.S3.ListObjects                        as Export
import           Network.AWS.S3.ListObjectVersions                 as Export
import           Network.AWS.S3.ListParts                          as Export
import           Network.AWS.S3.PutBucketACL                       as Export
import           Network.AWS.S3.PutBucketCORS                      as Export
import           Network.AWS.S3.PutBucketLifecycle                 as Export
import           Network.AWS.S3.PutBucketLogging                   as Export
import           Network.AWS.S3.PutBucketNotificationConfiguration as Export
import           Network.AWS.S3.PutBucketPolicy                    as Export
import           Network.AWS.S3.PutBucketReplication               as Export
import           Network.AWS.S3.PutBucketRequestPayment            as Export
import           Network.AWS.S3.PutBucketTagging                   as Export
import           Network.AWS.S3.PutBucketVersioning                as Export
import           Network.AWS.S3.PutBucketWebsite                   as Export
import           Network.AWS.S3.PutObject                          as Export
import           Network.AWS.S3.PutObjectACL                       as Export
import           Network.AWS.S3.RestoreObject                      as Export
import           Network.AWS.S3.Types                              as Export
import           Network.AWS.S3.UploadPart                         as Export
import           Network.AWS.S3.UploadPartCopy                     as Export
import           Network.AWS.S3.Waiters                            as Export
