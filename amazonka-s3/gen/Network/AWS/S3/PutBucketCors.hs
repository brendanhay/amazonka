{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutBucketCors
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the cors configuration for a bucket.
module Network.AWS.S3.PutBucketCors
    (
    -- * Request
      PutBucketCors
    -- ** Request constructor
    , putBucketCors
    -- ** Request lenses
    , pbcrBucket
    , pbcrCORSConfiguration
    , pbcrContentMD5

    -- * Response
    , Empty
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Xml
import Network.AWS.S3.Types

data PutBucketCors = PutBucketCors
    { _pbcrBucket            :: BucketName
    , _pbcrCORSConfiguration :: Maybe CORSConfiguration
    , _pbcrContentMD5        :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'PutBucketCors' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbcrBucket' @::@ 'BucketName'
--
-- * 'pbcrCORSConfiguration' @::@ 'Maybe' 'CORSConfiguration'
--
-- * 'pbcrContentMD5' @::@ 'Maybe' 'Text'
--
putBucketCors :: BucketName -- ^ 'pbcrBucket'
              -> PutBucketCors
putBucketCors p1 = PutBucketCors
    { _pbcrBucket            = p1
    , _pbcrCORSConfiguration = Nothing
    , _pbcrContentMD5        = Nothing
    }

pbcrBucket :: Lens' PutBucketCors BucketName
pbcrBucket = lens _pbcrBucket (\s a -> s { _pbcrBucket = a })

pbcrCORSConfiguration :: Lens' PutBucketCors (Maybe CORSConfiguration)
pbcrCORSConfiguration =
    lens _pbcrCORSConfiguration (\s a -> s { _pbcrCORSConfiguration = a })

pbcrContentMD5 :: Lens' PutBucketCors (Maybe Text)
pbcrContentMD5 = lens _pbcrContentMD5 (\s a -> s { _pbcrContentMD5 = a })

instance ToPath PutBucketCors where
    toPath PutBucketCors{..} = mconcat
        [ "/"
        , toText _pbcrBucket
        ]

instance ToQuery PutBucketCors where
    toQuery = const "cors"

instance ToHeaders PutBucketCors where
    toHeaders PutBucketCors{..} = mconcat
        [ "Content-MD5" =: _pbcrContentMD5
        ]

instance ToBody PutBucketCors where
    toBody = toBody . encodeXML . _pbcrCORSConfiguration

instance AWSRequest PutBucketCors where
    type Sv PutBucketCors = S3
    type Rs PutBucketCors = Empty

    request  = put
    response = const (nullaryResponse Empty)
