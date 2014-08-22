{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'PutBucketAcl' request.
putBucketAcl :: BucketName -- ^ '_pbarBucket'
             -> PutBucketAcl
putBucketAcl p1 = PutBucketAcl
    { _pbarBucket = p1
    , _pbarAccessControlPolicy = Nothing
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
    , _pbarAccessControlPolicy :: Maybe AccessControlPolicy
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

makeLenses ''PutBucketAcl

instance ToPath PutBucketAcl where
    toPath PutBucketAcl{..} = mconcat
        [ "/"
        , toBS _pbarBucket
        ]

instance ToQuery PutBucketAcl where
    toQuery PutBucketAcl{..} = mconcat
        [ "acl"
        ]

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
    toBody = toBody . encodeXML . _pbarAccessControlPolicy

data PutBucketAclResponse = PutBucketAclResponse
    deriving (Eq, Show, Generic)

makeLenses ''PutBucketAclResponse

instance AWSRequest PutBucketAcl where
    type Sv PutBucketAcl = S3
    type Rs PutBucketAcl = PutBucketAclResponse

    request = put
    response _ = nullaryResponse PutBucketAclResponse
