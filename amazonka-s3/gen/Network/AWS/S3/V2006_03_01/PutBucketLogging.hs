{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.PutBucketLogging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Set the logging parameters for a bucket and to specify permissions for who
-- can view and modify the logging parameters. To set the logging status of a
-- bucket, you must be the bucket owner.
module Network.AWS.S3.V2006_03_01.PutBucketLogging
    (
    -- * Request
      PutBucketLogging
    -- ** Request constructor
    , mkPutBucketLoggingRequest
    -- ** Request lenses
    , pblsBucket
    , pblsBucketLoggingStatus
    , pblsContentMD5

    -- * Response
    , PutBucketLoggingResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutBucketLogging' request.
mkPutBucketLoggingRequest :: BucketName -- ^ 'pblsBucket'
                          -> BucketLoggingStatus -- ^ 'pblsBucketLoggingStatus'
                          -> PutBucketLogging
mkPutBucketLoggingRequest p1 p2 = PutBucketLogging
    { _pblsBucket = p1
    , _pblsBucketLoggingStatus = p2
    , _pblsContentMD5 = Nothing
    }
{-# INLINE mkPutBucketLoggingRequest #-}

data PutBucketLogging = PutBucketLogging
    { _pblsBucket :: BucketName
    , _pblsBucketLoggingStatus :: BucketLoggingStatus
    , _pblsContentMD5 :: Maybe Text
    } deriving (Show, Generic)

pblsBucket :: Lens' PutBucketLogging (BucketName)
pblsBucket = lens _pblsBucket (\s a -> s { _pblsBucket = a })
{-# INLINE pblsBucket #-}

pblsBucketLoggingStatus :: Lens' PutBucketLogging (BucketLoggingStatus)
pblsBucketLoggingStatus = lens _pblsBucketLoggingStatus (\s a -> s { _pblsBucketLoggingStatus = a })
{-# INLINE pblsBucketLoggingStatus #-}

pblsContentMD5 :: Lens' PutBucketLogging (Maybe Text)
pblsContentMD5 = lens _pblsContentMD5 (\s a -> s { _pblsContentMD5 = a })
{-# INLINE pblsContentMD5 #-}

instance ToPath PutBucketLogging where
    toPath PutBucketLogging{..} = mconcat
        [ "/"
        , toBS _pblsBucket
        ]

instance ToQuery PutBucketLogging where
    toQuery PutBucketLogging{..} = mconcat
        [ "logging"
        ]

instance ToHeaders PutBucketLogging

instance ToBody PutBucketLogging

data PutBucketLoggingResponse = PutBucketLoggingResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutBucketLogging where
    type Sv PutBucketLogging = S3
    type Rs PutBucketLogging = PutBucketLoggingResponse

    request = put
    response _ = nullaryResponse PutBucketLoggingResponse
