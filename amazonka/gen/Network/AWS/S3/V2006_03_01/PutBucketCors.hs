{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.S3.V2006_03_01.PutBucketCors
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the cors configuration for a bucket.
module Network.AWS.S3.V2006_03_01.PutBucketCors where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'PutBucketCors' request.
putBucketCors :: BucketName -- ^ '_pbcrBucket'
              -> PutBucketCors
putBucketCors p1 = PutBucketCors
    { _pbcrBucket = p1
    , _pbcrCORSConfiguration = Nothing
    , _pbcrContentMD5 = Nothing
    }

data PutBucketCors = PutBucketCors
    { _pbcrBucket :: BucketName
    , _pbcrCORSConfiguration :: Maybe CORSConfiguration
    , _pbcrContentMD5 :: Maybe Text
    } deriving (Show, Generic)

makeLenses ''PutBucketCors

instance ToPath PutBucketCors where
    toPath PutBucketCors{..} = mconcat
        [ "/"
        , toBS _pbcrBucket
        ]

instance ToQuery PutBucketCors where
    toQuery PutBucketCors{..} = mconcat
        [ "cors"
        ]

instance ToHeaders PutBucketCors where
    toHeaders PutBucketCors{..} = concat
        [ "Content-MD5" =: _pbcrContentMD5
        ]

instance ToBody PutBucketCors where
    toBody = toBody . encodeXML . _pbcrCORSConfiguration

data PutBucketCorsResponse = PutBucketCorsResponse
    deriving (Eq, Show, Generic)

makeLenses ''PutBucketCorsResponse

instance AWSRequest PutBucketCors where
    type Sv PutBucketCors = S3
    type Rs PutBucketCors = PutBucketCorsResponse

    request = put
    response _ _ = return (Right PutBucketCorsResponse)
