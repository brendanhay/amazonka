{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketCors
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the cors configuration for the bucket.
module Network.AWS.S3.GetBucketCors
    (
    -- * Request
      GetBucketCors
    -- ** Request constructor
    , getBucketCors
    -- ** Request lenses
    , gbcrBucket

    -- * Response
    , GetBucketCorsOutput
    -- ** Response constructor
    , getBucketCorsOutput
    -- ** Response lenses
    , gbcoCORSRules
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Xml
import Network.AWS.S3.Types

newtype GetBucketCors = GetBucketCors
    { _gbcrBucket :: BucketName
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetBucketCors' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbcrBucket' @::@ 'BucketName'
--
getBucketCors :: BucketName -- ^ 'gbcrBucket'
              -> GetBucketCors
getBucketCors p1 = GetBucketCors
    { _gbcrBucket = p1
    }

gbcrBucket :: Lens' GetBucketCors BucketName
gbcrBucket = lens _gbcrBucket (\s a -> s { _gbcrBucket = a })

instance ToPath GetBucketCors where
    toPath GetBucketCors{..} = mconcat
        [ "/"
        , toText _gbcrBucket
        ]

instance ToQuery GetBucketCors where
    toQuery = const "cors"

instance ToHeaders GetBucketCors

instance ToBody GetBucketCors

newtype GetBucketCorsOutput = GetBucketCorsOutput
    { _gbcoCORSRules :: [CORSRule]
    } deriving (Eq, Ord, Show, Generic, Monoid)

instance AWSRequest GetBucketCors where
    type Sv GetBucketCors = S3
    type Rs GetBucketCors = GetBucketCorsOutput

    request  = get
    response = const . xmlResponse $ \h x ->
        <$> x %| "CORSRule"
