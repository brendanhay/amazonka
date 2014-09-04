{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetBucketLogging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the logging status of a bucket and the permissions users have to
-- view and modify that status. To use GET, you must be the bucket owner.
module Network.AWS.S3.V2006_03_01.GetBucketLogging
    (
    -- * Request
      GetBucketLogging
    -- ** Request constructor
    , getBucketLogging
    -- ** Request lenses
    , gbltBucket

    -- * Response
    , GetBucketLoggingResponse
    -- ** Response lenses
    , gblqLoggingEnabled
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetBucketLogging' request.
getBucketLogging :: BucketName -- ^ 'gbltBucket'
                 -> GetBucketLogging
getBucketLogging p1 = GetBucketLogging
    { _gbltBucket = p1
    }
{-# INLINE getBucketLogging #-}

data GetBucketLogging = GetBucketLogging
    { _gbltBucket :: BucketName
    } deriving (Show, Generic)

gbltBucket :: Lens' GetBucketLogging (BucketName)
gbltBucket f x =
    f (_gbltBucket x)
        <&> \y -> x { _gbltBucket = y }
{-# INLINE gbltBucket #-}

instance ToPath GetBucketLogging where
    toPath GetBucketLogging{..} = mconcat
        [ "/"
        , toBS _gbltBucket
        ]

instance ToQuery GetBucketLogging where
    toQuery GetBucketLogging{..} = mconcat
        [ "logging"
        ]

instance ToHeaders GetBucketLogging

instance ToBody GetBucketLogging

data GetBucketLoggingResponse = GetBucketLoggingResponse
    { _gblqLoggingEnabled :: Maybe LoggingEnabled
    } deriving (Show, Generic)

gblqLoggingEnabled :: Lens' GetBucketLoggingResponse (Maybe LoggingEnabled)
gblqLoggingEnabled f x =
    f (_gblqLoggingEnabled x)
        <&> \y -> x { _gblqLoggingEnabled = y }
{-# INLINE gblqLoggingEnabled #-}

instance FromXML GetBucketLoggingResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketLogging where
    type Sv GetBucketLogging = S3
    type Rs GetBucketLogging = GetBucketLoggingResponse

    request = get
    response _ = xmlResponse
