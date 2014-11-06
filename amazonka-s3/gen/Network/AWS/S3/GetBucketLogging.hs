{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketLogging
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
module Network.AWS.S3.GetBucketLogging
    (
    -- * Request
      GetBucketLogging
    -- ** Request constructor
    , getBucketLogging
    -- ** Request lenses
    , gblr2Bucket

    -- * Response
    , GetBucketLoggingOutput
    -- ** Response constructor
    , getBucketLoggingOutput
    -- ** Response lenses
    , gbloLoggingEnabled
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

newtype GetBucketLogging = GetBucketLogging
    { _gblr2Bucket :: BucketName
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetBucketLogging' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gblr2Bucket' @::@ 'BucketName'
--
getBucketLogging :: BucketName -- ^ 'gblr2Bucket'
                 -> GetBucketLogging
getBucketLogging p1 = GetBucketLogging
    { _gblr2Bucket = p1
    }

gblr2Bucket :: Lens' GetBucketLogging BucketName
gblr2Bucket = lens _gblr2Bucket (\s a -> s { _gblr2Bucket = a })

instance ToPath GetBucketLogging where
    toPath GetBucketLogging{..} = mconcat
        [ "/"
        , toText _gblr2Bucket
        ]

instance ToQuery GetBucketLogging where
    toQuery = const "logging"

instance ToHeaders GetBucketLogging

newtype GetBucketLoggingOutput = GetBucketLoggingOutput
    { _gbloLoggingEnabled :: Maybe LoggingEnabled
    } deriving (Eq, Ord, Show, Generic)

instance AWSRequest GetBucketLogging where
    type Sv GetBucketLogging = S3
    type Rs GetBucketLogging = GetBucketLoggingOutput

    request  = get
    response = const . xmlResponse $ \h x ->
        <$> x %| "LoggingEnabled"
