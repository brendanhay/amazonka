{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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

import           Control.Applicative
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

-- | Default PutObjectAcl request.
putObjectAcl :: BucketName -- ^ 'poarBucket'
             -> ObjectKey -- ^ 'poarKey'
             -> AccessControlPolicy -- ^ 'poarAccessControlPolicy'
             -> PutObjectAcl
putObjectAcl p1 p2 p3 = PutObjectAcl
    { poarBucket = p1
    , poarKey = p2
    , poarAccessControlPolicy = p3
    , poarContentMD5 = Nothing
    , poarGrantFullControl = Nothing
    , poarGrantRead = Nothing
    , poarGrantReadACP = Nothing
    , poarGrantWrite = Nothing
    , poarGrantWriteACP = Nothing
    , poarACL = Nothing
    }

data PutObjectAcl = PutObjectAcl
    { poarBucket :: BucketName
    , poarKey :: ObjectKey
    , poarAccessControlPolicy :: AccessControlPolicy
    , poarContentMD5 :: Maybe Text
    , poarGrantFullControl :: Maybe Text
      -- ^ Allows grantee the read, write, read ACP, and write ACP
      -- permissions on the bucket.
    , poarGrantRead :: Maybe Text
      -- ^ Allows grantee to list the objects in the bucket.
    , poarGrantReadACP :: Maybe Text
      -- ^ Allows grantee to read the bucket ACL.
    , poarGrantWrite :: Maybe Text
      -- ^ Allows grantee to create, overwrite, and delete any object in the
      -- bucket.
    , poarGrantWriteACP :: Maybe Text
      -- ^ Allows grantee to write the ACL for the applicable bucket.
    , poarACL :: Maybe ObjectCannedACL
      -- ^ The canned ACL to apply to the object.
    } deriving (Eq, Show, Generic)

instance ToPath PutObjectAcl where
    toPath PutObjectAcl{..} = mconcat
        [ "/"
        , toBS poarBucket
        , "/"
        , toBS poarKey
        ]

instance ToQuery PutObjectAcl

instance ToHeaders PutObjectAcl where
    toHeaders PutObjectAcl{..} = concat
        [ "Content-MD5" =: poarContentMD5
        , "x-amz-grant-full-control" =: poarGrantFullControl
        , "x-amz-grant-read" =: poarGrantRead
        , "x-amz-grant-read-acp" =: poarGrantReadACP
        , "x-amz-grant-write" =: poarGrantWrite
        , "x-amz-grant-write-acp" =: poarGrantWriteACP
        , "x-amz-acl" =: poarACL
        ]

instance ToBody PutObjectAcl where
    toBody = undefined -- toBody . poarAccessControlPolicy

instance AWSRequest PutObjectAcl where
    type Sv PutObjectAcl = S3

    request  = put
    response = headerResponse . const $ Right PutObjectAclResponse

data instance Rs PutObjectAcl = PutObjectAclResponse
    deriving (Eq, Show, Generic)

