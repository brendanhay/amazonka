{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <GetBucketCors.html>
module Network.AWS.S3.GetBucketCors
    (
    -- * Request
      GetBucketCors
    -- ** Request constructor
    , getBucketCors
    -- ** Request lenses
    , gbcBucket

    -- * Response
    , GetBucketCorsResponse
    -- ** Response constructor
    , getBucketCorsResponse
    -- ** Response lenses
    , gbcrCORSRules
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.XML
import Network.AWS.S3.Types
import qualified GHC.Exts

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

newtype GetBucketCorsResponse = GetBucketCorsResponse
    { _gbcrCORSRules :: [CORSRule]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList GetBucketCorsResponse where
    type Item GetBucketCorsResponse = CORSRule

    fromList = GetBucketCorsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _gbcrCORSRules

-- | 'GetBucketCorsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbcrCORSRules' @::@ ['CORSRule']
--
getBucketCorsResponse :: GetBucketCorsResponse
getBucketCorsResponse = GetBucketCorsResponse
    { _gbcrCORSRules = mempty
    }

gbcrCORSRules :: Lens' GetBucketCorsResponse [CORSRule]
gbcrCORSRules = lens _gbcrCORSRules (\s a -> s { _gbcrCORSRules = a })

instance AWSRequest GetBucketCors where
    type Sv GetBucketCors = S3
    type Rs GetBucketCors = GetBucketCorsResponse

    request  = get
    response = xmlResponse

instance FromXML GetBucketCorsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetBucketCorsResponse"

instance ToPath GetBucketCors where
    toPath GetBucketCors{..} = mconcat
        [ "/"
        , toText _gbcBucket
        ]

instance ToHeaders GetBucketCors

instance ToQuery GetBucketCors where
    toQuery = const "cors"

instance ToXML GetBucketCors where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetBucketCors"
