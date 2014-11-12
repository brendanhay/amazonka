{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
    , gbcBucket

    -- * Response
    , GetBucketCorsOutput
    -- ** Response constructor
    , getBucketCorsOutput
    -- ** Response lenses
    , gbcoCORSRules
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

newtype GetBucketCors = GetBucketCors
    { _gbcBucket :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetBucketCors' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbcBucket' @::@ 'Text'
--
getBucketCors :: Text -- ^ 'gbcBucket'
              -> GetBucketCors
getBucketCors p1 = GetBucketCors
    { _gbcBucket = p1
    }

gbcBucket :: Lens' GetBucketCors Text
gbcBucket = lens _gbcBucket (\s a -> s { _gbcBucket = a })

instance ToPath GetBucketCors where
    toPath GetBucketCors{..} = mconcat
        [ "/"
        , toText _gbcBucket
        ]

instance ToQuery GetBucketCors where
    toQuery = const "cors"

instance ToHeaders GetBucketCors

newtype GetBucketCorsOutput = GetBucketCorsOutput
    { _gbcoCORSRules :: [CORSRule]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance IsList GetBucketCorsOutput where
    type Item GetBucketCorsOutput = CORSRule

    fromList = GetBucketCorsOutput . fromList
    toList   = toList . _gbcoCORSRules

-- | 'GetBucketCorsOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbcoCORSRules' @::@ ['CORSRule']
--
getBucketCorsOutput :: GetBucketCorsOutput
getBucketCorsOutput = GetBucketCorsOutput
    { _gbcoCORSRules = mempty
    }

gbcoCORSRules :: Lens' GetBucketCorsOutput [CORSRule]
gbcoCORSRules = lens _gbcoCORSRules (\s a -> s { _gbcoCORSRules = a })

instance FromXML GetBucketCorsOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetBucketCorsOutput"
instance AWSRequest GetBucketCors where
    type Sv GetBucketCors = S3
    type Rs GetBucketCors = GetBucketCorsOutput

    request  = get
    response = xmlResponse $ \h x -> GetBucketCorsOutput
        <$> x %| "CORSRule"
