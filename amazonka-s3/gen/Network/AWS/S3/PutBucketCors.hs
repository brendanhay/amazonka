{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
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

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

data PutBucketCors = PutBucketCors
    { _pbcBucket :: Text
    , _pbcCORSConfiguration :: Maybe CORSConfiguration
    , _pbcContentMD5 :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutBucketCors' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @Text@
--
-- * @CORSConfiguration ::@ @Maybe CORSConfiguration@
--
-- * @ContentMD5 ::@ @Maybe Text@
--
putBucketCors :: Text -- ^ 'pbcBucket'
              -> PutBucketCors
putBucketCors p1 = PutBucketCors
    { _pbcBucket = p1
    , _pbcCORSConfiguration = Nothing
    , _pbcContentMD5 = Nothing
    }

pbcBucket :: Lens' PutBucketCors Text
pbcBucket = lens _pbcBucket (\s a -> s { _pbcBucket = a })

pbcCORSConfiguration :: Lens' PutBucketCors (Maybe CORSConfiguration)
pbcCORSConfiguration =
    lens _pbcCORSConfiguration (\s a -> s { _pbcCORSConfiguration = a })

pbcContentMD5 :: Lens' PutBucketCors (Maybe Text)
pbcContentMD5 = lens _pbcContentMD5 (\s a -> s { _pbcContentMD5 = a })

instance ToPath PutBucketCors

instance ToQuery PutBucketCors

instance ToHeaders PutBucketCors where
    toHeaders PutBucketCors{..} = concat
        [ "Content-MD5" =: _pbcContentMD5
        ]

instance ToBody PutBucketCors where
    toBody = toBody . encodeXML . _pbcCORSConfiguration

data PutBucketCorsResponse = PutBucketCorsResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutBucketCorsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
putBucketCorsResponse :: PutBucketCorsResponse
putBucketCorsResponse = PutBucketCorsResponse

instance AWSRequest PutBucketCors where
    type Sv PutBucketCors = S3
    type Rs PutBucketCors = PutBucketCorsResponse

    request = get
    response _ = nullaryResponse PutBucketCorsResponse
