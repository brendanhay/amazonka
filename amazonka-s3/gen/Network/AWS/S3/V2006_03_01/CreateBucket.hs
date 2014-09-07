{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
module Network.AWS.S3.V2006_03_01.CreateBucket
    (
    -- * Request
      CreateBucket
    -- ** Request alias
    , PutBucket
    -- ** Request constructor
    , mkCreateBucket
    -- ** Request lenses
    , cbACL
    , cbBucket
    , cbCreateBucketConfiguration
    , cbGrantFullControl
    , cbGrantRead
    , cbGrantReadACP
    , cbGrantWrite
    , cbGrantWriteACP

    -- * Response
    , CreateBucketResponse
    -- ** Response lenses
    , cbrsLocation
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

type PutBucket = CreateBucket

data CreateBucket = CreateBucket
    { _cbACL :: Maybe BucketCannedACL
    , _cbBucket :: BucketName
    , _cbCreateBucketConfiguration :: Maybe CreateBucketConfiguration
    , _cbGrantFullControl :: Maybe Text
    , _cbGrantRead :: Maybe Text
    , _cbGrantReadACP :: Maybe Text
    , _cbGrantWrite :: Maybe Text
    , _cbGrantWriteACP :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateBucket' request.
mkCreateBucket :: BucketName -- ^ 'cbBucket'
               -> CreateBucket
mkCreateBucket p2 = CreateBucket
    { _cbACL = Nothing
    , _cbBucket = p2
    , _cbCreateBucketConfiguration = Nothing
    , _cbGrantFullControl = Nothing
    , _cbGrantRead = Nothing
    , _cbGrantReadACP = Nothing
    , _cbGrantWrite = Nothing
    , _cbGrantWriteACP = Nothing
    }

-- | The canned ACL to apply to the bucket.
cbACL :: Lens' CreateBucket (Maybe BucketCannedACL)
cbACL = lens _cbACL (\s a -> s { _cbACL = a })

cbBucket :: Lens' CreateBucket BucketName
cbBucket = lens _cbBucket (\s a -> s { _cbBucket = a })

cbCreateBucketConfiguration :: Lens' CreateBucket (Maybe CreateBucketConfiguration)
cbCreateBucketConfiguration =
    lens _cbCreateBucketConfiguration
         (\s a -> s { _cbCreateBucketConfiguration = a })

-- | Allows grantee the read, write, read ACP, and write ACP permissions on the
-- bucket.
cbGrantFullControl :: Lens' CreateBucket (Maybe Text)
cbGrantFullControl =
    lens _cbGrantFullControl (\s a -> s { _cbGrantFullControl = a })

-- | Allows grantee to list the objects in the bucket.
cbGrantRead :: Lens' CreateBucket (Maybe Text)
cbGrantRead = lens _cbGrantRead (\s a -> s { _cbGrantRead = a })

-- | Allows grantee to read the bucket ACL.
cbGrantReadACP :: Lens' CreateBucket (Maybe Text)
cbGrantReadACP = lens _cbGrantReadACP (\s a -> s { _cbGrantReadACP = a })

-- | Allows grantee to create, overwrite, and delete any object in the bucket.
cbGrantWrite :: Lens' CreateBucket (Maybe Text)
cbGrantWrite = lens _cbGrantWrite (\s a -> s { _cbGrantWrite = a })

-- | Allows grantee to write the ACL for the applicable bucket.
cbGrantWriteACP :: Lens' CreateBucket (Maybe Text)
cbGrantWriteACP = lens _cbGrantWriteACP (\s a -> s { _cbGrantWriteACP = a })

instance ToPath CreateBucket where
    toPath CreateBucket{..} = mconcat
        [ "/"
        , toBS _cbBucket
        ]

instance ToQuery CreateBucket

instance ToHeaders CreateBucket where
    toHeaders CreateBucket{..} = concat
        [ "x-amz-acl" =: _cbACL
        , "x-amz-grant-full-control" =: _cbGrantFullControl
        , "x-amz-grant-read" =: _cbGrantRead
        , "x-amz-grant-read-acp" =: _cbGrantReadACP
        , "x-amz-grant-write" =: _cbGrantWrite
        , "x-amz-grant-write-acp" =: _cbGrantWriteACP
        ]

instance ToBody CreateBucket where
    toBody = toBody . encodeXML . _cbCreateBucketConfiguration

newtype CreateBucketResponse = CreateBucketResponse
    { _cbrsLocation :: Maybe Text
    } deriving (Show, Generic)

cbrsLocation :: Lens' CreateBucketResponse (Maybe Text)
cbrsLocation = lens _cbrsLocation (\s a -> s { _cbrsLocation = a })

instance AWSRequest CreateBucket where
    type Sv CreateBucket = S3
    type Rs CreateBucket = CreateBucketResponse

    request = put
    response _ = headerResponse $ \hs ->
        pure CreateBucketResponse
            <*> hs ~:? "Location"
