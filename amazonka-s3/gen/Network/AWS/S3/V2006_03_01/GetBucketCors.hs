{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetBucketCors
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the cors configuration for the bucket.
module Network.AWS.S3.V2006_03_01.GetBucketCors
    (
    -- * Request
      GetBucketCors
    -- ** Request constructor
    , mkGetBucketCors
    -- ** Request lenses
    , gbcBucket

    -- * Response
    , GetBucketCorsResponse
    -- ** Response lenses
    , gbcrCORSRules
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

newtype GetBucketCors = GetBucketCors
    { _gbcBucket :: BucketName
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketCors' request.
mkGetBucketCors :: BucketName -- ^ 'gbcBucket'
                -> GetBucketCors
mkGetBucketCors p1 = GetBucketCors
    { _gbcBucket = p1
    }

gbcBucket :: Lens' GetBucketCors BucketName
gbcBucket = lens _gbcBucket (\s a -> s { _gbcBucket = a })

instance ToPath GetBucketCors where
    toPath GetBucketCors{..} = mconcat
        [ "/"
        , toBS _gbcBucket
        ]

instance ToQuery GetBucketCors where
    toQuery GetBucketCors{..} = mconcat
        [ "cors"
        ]

instance ToHeaders GetBucketCors

instance ToBody GetBucketCors

newtype GetBucketCorsResponse = GetBucketCorsResponse
    { _gbcrCORSRules :: [CORSRule]
    } deriving (Show, Generic)

gbcrCORSRules :: Lens' GetBucketCorsResponse [CORSRule]
gbcrCORSRules = lens _gbcrCORSRules (\s a -> s { _gbcrCORSRules = a })

instance FromXML GetBucketCorsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketCors where
    type Sv GetBucketCors = S3
    type Rs GetBucketCors = GetBucketCorsResponse

    request = get
    response _ = xmlResponse
