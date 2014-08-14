{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.S3.V2006_03_01.PutObjectAcl
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | uses the acl subresource to set the access control list (ACL) permissions
-- for an object that already exists in a bucket.
module Network.AWS.S3.V2006_03_01.PutObjectAcl where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'PutObjectAcl' request.
putObjectAcl :: BucketName -- ^ '_poarBucket'
             -> ObjectKey -- ^ '_poarKey'
             -> PutObjectAcl
putObjectAcl p1 p2 = PutObjectAcl
    { _poarBucket = p1
    , _poarKey = p2
    , _poarAccessControlPolicy = Nothing
    , _poarContentMD5 = Nothing
    , _poarGrantFullControl = Nothing
    , _poarGrantRead = Nothing
    , _poarGrantReadACP = Nothing
    , _poarGrantWrite = Nothing
    , _poarGrantWriteACP = Nothing
    , _poarACL = Nothing
    }

data PutObjectAcl = PutObjectAcl
    { _poarBucket :: BucketName
    , _poarKey :: ObjectKey
    , _poarAccessControlPolicy :: Maybe AccessControlPolicy
    , _poarContentMD5 :: Maybe Text
    , _poarGrantFullControl :: Maybe Text
      -- ^ Allows grantee the read, write, read ACP, and write ACP
      -- permissions on the bucket.
    , _poarGrantRead :: Maybe Text
      -- ^ Allows grantee to list the objects in the bucket.
    , _poarGrantReadACP :: Maybe Text
      -- ^ Allows grantee to read the bucket ACL.
    , _poarGrantWrite :: Maybe Text
      -- ^ Allows grantee to create, overwrite, and delete any object in the
      -- bucket.
    , _poarGrantWriteACP :: Maybe Text
      -- ^ Allows grantee to write the ACL for the applicable bucket.
    , _poarACL :: Maybe ObjectCannedACL
      -- ^ The canned ACL to apply to the object.
    } deriving (Show, Generic)

makeLenses ''PutObjectAcl

instance ToPath PutObjectAcl where
    toPath PutObjectAcl{..} = mconcat
        [ "/"
        , toBS _poarBucket
        , "/"
        , toBS _poarKey
        ]

instance ToQuery PutObjectAcl where
    toQuery PutObjectAcl{..} = mconcat
        [ "acl"
        ]

instance ToHeaders PutObjectAcl where
    toHeaders PutObjectAcl{..} = concat
        [ "Content-MD5" =: _poarContentMD5
        , "x-amz-grant-full-control" =: _poarGrantFullControl
        , "x-amz-grant-read" =: _poarGrantRead
        , "x-amz-grant-read-acp" =: _poarGrantReadACP
        , "x-amz-grant-write" =: _poarGrantWrite
        , "x-amz-grant-write-acp" =: _poarGrantWriteACP
        , "x-amz-acl" =: _poarACL
        ]

instance ToBody PutObjectAcl where
    toBody = toBody . encodeXML . _poarAccessControlPolicy

data PutObjectAclResponse = PutObjectAclResponse
    deriving (Eq, Show, Generic)

makeLenses ''PutObjectAclResponse

instance AWSRequest PutObjectAcl where
    type Sv PutObjectAcl = S3
    type Rs PutObjectAcl = PutObjectAclResponse

    request = put
    response _ = nullaryResponse PutObjectAclResponse
