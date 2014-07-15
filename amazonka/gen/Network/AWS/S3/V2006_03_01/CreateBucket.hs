{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.CreateBucket
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new bucket.
module Network.AWS.S3.V2006_03_01.CreateBucket where

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

type PutBucket = CreateBucket
type PutBucketResponse = Rs CreateBucket

-- | Default CreateBucket request.
createBucket :: BucketName -- ^ '_cbrBucket'
             -> CreateBucketConfiguration -- ^ '_cbrCreateBucketConfiguration'
             -> CreateBucket
createBucket p1 p2 = CreateBucket
    { _cbrBucket = p1
    , _cbrCreateBucketConfiguration = p2
    , _cbrACL = Nothing
    , _cbrGrantFullControl = Nothing
    , _cbrGrantRead = Nothing
    , _cbrGrantReadACP = Nothing
    , _cbrGrantWrite = Nothing
    , _cbrGrantWriteACP = Nothing
    }

data CreateBucket = CreateBucket
    { _cbrBucket :: BucketName
    , _cbrCreateBucketConfiguration :: CreateBucketConfiguration
    , _cbrACL :: Maybe BucketCannedACL
      -- ^ The canned ACL to apply to the bucket.
    , _cbrGrantFullControl :: Maybe Text
      -- ^ Allows grantee the read, write, read ACP, and write ACP
      -- permissions on the bucket.
    , _cbrGrantRead :: Maybe Text
      -- ^ Allows grantee to list the objects in the bucket.
    , _cbrGrantReadACP :: Maybe Text
      -- ^ Allows grantee to read the bucket ACL.
    , _cbrGrantWrite :: Maybe Text
      -- ^ Allows grantee to create, overwrite, and delete any object in the
      -- bucket.
    , _cbrGrantWriteACP :: Maybe Text
      -- ^ Allows grantee to write the ACL for the applicable bucket.
    } deriving (Show, Generic)

instance ToPath CreateBucket where
    toPath CreateBucket{..} = mconcat
        [ "/"
        , toBS _cbrBucket
        ]

instance ToQuery CreateBucket

instance ToHeaders CreateBucket where
    toHeaders CreateBucket{..} = concat
        [ "x-amz-acl" =: _cbrACL
        , "x-amz-grant-full-control" =: _cbrGrantFullControl
        , "x-amz-grant-read" =: _cbrGrantRead
        , "x-amz-grant-read-acp" =: _cbrGrantReadACP
        , "x-amz-grant-write" =: _cbrGrantWrite
        , "x-amz-grant-write-acp" =: _cbrGrantWriteACP
        ]

instance ToBody CreateBucket where
    toBody = undefined -- toBody . _cbrCreateBucketConfiguration

instance AWSRequest CreateBucket where
    type Sv CreateBucket = S3

    request  = put
    response = headerResponse $ \hs ->
        pure CreateBucketResponse
            <*> hs ~:? "Location"

data instance Rs CreateBucket = CreateBucketResponse
    { _cboLocation :: Maybe Text
    } deriving (Show, Generic)
