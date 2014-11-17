{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/PutBucketCors.html>
module Network.AWS.S3.PutBucketCors
    (
    -- * Request
      PutBucketCors
    -- ** Request constructor
    , putBucketCors
    -- ** Request lenses
    , pbcBucket
    , pbcCORSConfiguration
    , pbcContentMD5

    -- * Response
    , PutBucketCorsResponse
    -- ** Response constructor
    , putBucketCorsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.S3.Types
import qualified GHC.Exts

data PutBucketCors = PutBucketCors
    { _pbcBucket            :: Text
    , _pbcCORSConfiguration :: Maybe CORSConfiguration
    , _pbcContentMD5        :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'PutBucketCors' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbcBucket' @::@ 'Text'
--
-- * 'pbcCORSConfiguration' @::@ 'Maybe' 'CORSConfiguration'
--
-- * 'pbcContentMD5' @::@ 'Maybe' 'Text'
--
putBucketCors :: Text -- ^ 'pbcBucket'
              -> PutBucketCors
putBucketCors p1 = PutBucketCors
    { _pbcBucket            = p1
    , _pbcCORSConfiguration = Nothing
    , _pbcContentMD5        = Nothing
    }

pbcBucket :: Lens' PutBucketCors Text
pbcBucket = lens _pbcBucket (\s a -> s { _pbcBucket = a })

pbcCORSConfiguration :: Lens' PutBucketCors (Maybe CORSConfiguration)
pbcCORSConfiguration =
    lens _pbcCORSConfiguration (\s a -> s { _pbcCORSConfiguration = a })

pbcContentMD5 :: Lens' PutBucketCors (Maybe Text)
pbcContentMD5 = lens _pbcContentMD5 (\s a -> s { _pbcContentMD5 = a })

data PutBucketCorsResponse = PutBucketCorsResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'PutBucketCorsResponse' constructor.
putBucketCorsResponse :: PutBucketCorsResponse
putBucketCorsResponse = PutBucketCorsResponse

instance ToPath PutBucketCors where
    toPath PutBucketCors{..} = mconcat
        [ "/"
        , toText _pbcBucket
        ]

instance ToQuery PutBucketCors where
    toQuery = const "cors"

instance ToHeaders PutBucketCors where
    toHeaders PutBucketCors{..} = mconcat
        [ "Content-MD5" =: _pbcContentMD5
        ]

instance ToXML PutBucketCors where
    toXML PutBucketCors{..} = node "PutBucketCors"
        [ "CORSConfiguration" .= _pbcCORSConfiguration
        ]

instance AWSRequest PutBucketCors where
    type Sv PutBucketCors = S3
    type Rs PutBucketCors = PutBucketCorsResponse

    request  = put
    response = nullResponse PutBucketCorsResponse
