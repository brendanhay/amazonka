{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketCors.html>
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
import Network.AWS.Request.RestXML
import Network.AWS.S3.Types
import qualified GHC.Exts

newtype GetBucketCors = GetBucketCors
    { _gbcBucket :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

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
    { _gbcrCORSRules :: List "CORSRule" CORSRule
    } deriving (Eq, Show, Monoid, Semigroup)

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
gbcrCORSRules = lens _gbcrCORSRules (\s a -> s { _gbcrCORSRules = a }) . _List

instance ToPath GetBucketCors where
    toPath GetBucketCors{..} = mconcat
        [ "/"
        , toText _gbcBucket
        ]

instance ToQuery GetBucketCors where
    toQuery = const "cors"

instance ToHeaders GetBucketCors

instance ToXMLRoot GetBucketCors where
    toXMLRoot = const (namespaced ns "GetBucketCors" [])

instance ToXML GetBucketCors

instance AWSRequest GetBucketCors where
    type Sv GetBucketCors = S3
    type Rs GetBucketCors = GetBucketCorsResponse

    request  = get
    response = xmlResponse

instance FromXML GetBucketCorsResponse where
    parseXML x = GetBucketCorsResponse
        <$> parseXML x
