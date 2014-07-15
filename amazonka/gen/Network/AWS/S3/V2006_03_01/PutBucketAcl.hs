{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error)
import           Network.AWS.Request.RestS3
import           Network.AWS.S3.V2006_03_01.Types
import           Network.HTTP.Client  (Response)
import           Prelude              hiding (head)

-- | Default PutBucketAcl request.
putBucketAcl :: BucketName -- ^ '_pbarBucket'
             -> AccessControlPolicy -- ^ '_pbarAccessControlPolicy'
             -> PutBucketAcl
putBucketAcl p1 p2 = PutBucketAcl
    { _pbarBucket = p1
    , _pbarAccessControlPolicy = p2
    , _pbarACL = Nothing
    , _pbarContentMD5 = Nothing
    , _pbarGrantFullControl = Nothing
    , _pbarGrantRead = Nothing
    , _pbarGrantReadACP = Nothing
    , _pbarGrantWrite = Nothing
    , _pbarGrantWriteACP = Nothing
    }

data PutBucketAcl = PutBucketAcl
    { _pbarBucket :: BucketName
    , _pbarAccessControlPolicy :: AccessControlPolicy
    , _pbarACL :: Maybe BucketCannedACL
      -- ^ The canned ACL to apply to the bucket.
    , _pbarContentMD5 :: Maybe Text
    , _pbarGrantFullControl :: Maybe Text
      -- ^ Allows grantee the read, write, read ACP, and write ACP
      -- permissions on the bucket.
    , _pbarGrantRead :: Maybe Text
      -- ^ Allows grantee to list the objects in the bucket.
    , _pbarGrantReadACP :: Maybe Text
      -- ^ Allows grantee to read the bucket ACL.
    , _pbarGrantWrite :: Maybe Text
      -- ^ Allows grantee to create, overwrite, and delete any object in the
      -- bucket.
    , _pbarGrantWriteACP :: Maybe Text
      -- ^ Allows grantee to write the ACL for the applicable bucket.
    } deriving (Show, Generic)

instance ToPath PutBucketAcl where
    toPath PutBucketAcl{..} = mconcat
        [ "/"
        , toBS _pbarBucket
        ]

instance ToQuery PutBucketAcl

instance ToHeaders PutBucketAcl where
    toHeaders PutBucketAcl{..} = concat
        [ "x-amz-acl" =: _pbarACL
        , "Content-MD5" =: _pbarContentMD5
        , "x-amz-grant-full-control" =: _pbarGrantFullControl
        , "x-amz-grant-read" =: _pbarGrantRead
        , "x-amz-grant-read-acp" =: _pbarGrantReadACP
        , "x-amz-grant-write" =: _pbarGrantWrite
        , "x-amz-grant-write-acp" =: _pbarGrantWriteACP
        ]

instance ToBody PutBucketAcl where
    toBody = undefined -- toBody . _pbarAccessControlPolicy

instance AWSRequest PutBucketAcl where
    type Sv PutBucketAcl = S3

    request  = put
    response = headerResponse . const $ Right PutBucketAclResponse

data instance Rs PutBucketAcl = PutBucketAclResponse
    deriving (Eq, Show, Generic)
