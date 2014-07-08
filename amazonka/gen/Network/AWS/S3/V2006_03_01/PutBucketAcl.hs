{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.PutBucketAcl
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the permissions on a bucket using access control lists (ACL).
module Network.AWS.S3.V2006_03_01.PutBucketAcl where

import           Data.ByteString     (ByteString)
import           Data.Default
import           Data.HashMap.Strict (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Request.RestS3
import           Network.AWS.Types   hiding (Error)
import           Network.AWS.S3.V2006_03_01.Types
import           Prelude             hiding (head)


-- | Default PutBucketAcl request.
putBucketAcl :: BucketName -- ^ 'pbarBucket'
             -> AccessControlPolicy -- ^ 'pbarAccessControlPolicy'
             -> PutBucketAcl
putBucketAcl p1 p2 = PutBucketAcl
    { pbarBucket = p1
    , pbarAccessControlPolicy = p2
    , pbarACL = Nothing
    , pbarContentMD5 = Nothing
    , pbarGrantFullControl = Nothing
    , pbarGrantRead = Nothing
    , pbarGrantReadACP = Nothing
    , pbarGrantWrite = Nothing
    , pbarGrantWriteACP = Nothing
    }

data PutBucketAcl = PutBucketAcl
    { pbarBucket :: BucketName
    , pbarAccessControlPolicy :: AccessControlPolicy
    , pbarACL :: Maybe BucketCannedACL
      -- ^ The canned ACL to apply to the bucket.
    , pbarContentMD5 :: Maybe Text
    , pbarGrantFullControl :: Maybe Text
      -- ^ Allows grantee the read, write, read ACP, and write ACP
      -- permissions on the bucket.
    , pbarGrantRead :: Maybe Text
      -- ^ Allows grantee to list the objects in the bucket.
    , pbarGrantReadACP :: Maybe Text
      -- ^ Allows grantee to read the bucket ACL.
    , pbarGrantWrite :: Maybe Text
      -- ^ Allows grantee to create, overwrite, and delete any object in the
      -- bucket.
    , pbarGrantWriteACP :: Maybe Text
      -- ^ Allows grantee to write the ACL for the applicable bucket.
    } deriving (Eq, Show, Generic)

instance ToPath PutBucketAcl where
    toPath PutBucketAcl{..} = mconcat
        [ "/"
        , toBS pbarBucket
        ]

instance ToQuery PutBucketAcl

instance ToHeaders PutBucketAcl where
    toHeaders PutBucketAcl{..} = concat
        [ "x-amz-acl" =: pbarACL
        , "Content-MD5" =: pbarContentMD5
        , "x-amz-grant-full-control" =: pbarGrantFullControl
        , "x-amz-grant-read" =: pbarGrantRead
        , "x-amz-grant-read-acp" =: pbarGrantReadACP
        , "x-amz-grant-write" =: pbarGrantWrite
        , "x-amz-grant-write-acp" =: pbarGrantWriteACP
        ]

instance ToBody PutBucketAcl where
    toBody = undefined -- toBody . pbarAccessControlPolicy

instance AWSRequest PutBucketAcl where
    type Sv PutBucketAcl = S3

    request  = put
    response =

data instance Rs PutBucketAcl = PutBucketAclResponse
    deriving (Eq, Show, Generic)
