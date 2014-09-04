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
    , mkGetBucketCorsRequest
    -- ** Request lenses
    , gbcrBucket

    -- * Response
    , GetBucketCorsResponse
    -- ** Response lenses
    , gbcoCORSRules
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketCors' request.
mkGetBucketCorsRequest :: BucketName -- ^ 'gbcrBucket'
                       -> GetBucketCors
mkGetBucketCorsRequest p1 = GetBucketCors
    { _gbcrBucket = p1
    }
{-# INLINE mkGetBucketCorsRequest #-}

newtype GetBucketCors = GetBucketCors
    { _gbcrBucket :: BucketName
    } deriving (Show, Generic)

gbcrBucket :: Lens' GetBucketCors (BucketName)
gbcrBucket = lens _gbcrBucket (\s a -> s { _gbcrBucket = a })
{-# INLINE gbcrBucket #-}

instance ToPath GetBucketCors where
    toPath GetBucketCors{..} = mconcat
        [ "/"
        , toBS _gbcrBucket
        ]

instance ToQuery GetBucketCors where
    toQuery GetBucketCors{..} = mconcat
        [ "cors"
        ]

instance ToHeaders GetBucketCors

instance ToBody GetBucketCors

newtype GetBucketCorsResponse = GetBucketCorsResponse
    { _gbcoCORSRules :: [CORSRule]
    } deriving (Show, Generic)

gbcoCORSRules :: Lens' GetBucketCorsResponse ([CORSRule])
gbcoCORSRules = lens _gbcoCORSRules (\s a -> s { _gbcoCORSRules = a })
{-# INLINE gbcoCORSRules #-}

instance FromXML GetBucketCorsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketCors where
    type Sv GetBucketCors = S3
    type Rs GetBucketCors = GetBucketCorsResponse

    request = get
    response _ = xmlResponse
