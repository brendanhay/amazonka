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
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketLogging.html>
module Network.AWS.S3.GetBucketLogging
    (
    -- * Request
      GetBucketLogging
    -- ** Request constructor
    , getBucketLogging
    -- ** Request lenses
    , gbl2Bucket

    -- * Response
    , GetBucketLoggingResponse
    -- ** Response constructor
    , getBucketLoggingResponse
    -- ** Response lenses
    , gblrLoggingEnabled
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.S3.Types
import qualified GHC.Exts

newtype GetBucketLogging = GetBucketLogging
    { _gbl2Bucket :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetBucketLogging' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbl2Bucket' @::@ 'Text'
--
getBucketLogging :: Text -- ^ 'gbl2Bucket'
                 -> GetBucketLogging
getBucketLogging p1 = GetBucketLogging
    { _gbl2Bucket = p1
    }

gbl2Bucket :: Lens' GetBucketLogging Text
gbl2Bucket = lens _gbl2Bucket (\s a -> s { _gbl2Bucket = a })

newtype GetBucketLoggingResponse = GetBucketLoggingResponse
    { _gblrLoggingEnabled :: Maybe LoggingEnabled
    } deriving (Eq, Show, Generic)

-- | 'GetBucketLoggingResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gblrLoggingEnabled' @::@ 'Maybe' 'LoggingEnabled'
--
getBucketLoggingResponse :: GetBucketLoggingResponse
getBucketLoggingResponse = GetBucketLoggingResponse
    { _gblrLoggingEnabled = Nothing
    }

gblrLoggingEnabled :: Lens' GetBucketLoggingResponse (Maybe LoggingEnabled)
gblrLoggingEnabled =
    lens _gblrLoggingEnabled (\s a -> s { _gblrLoggingEnabled = a })

instance ToPath GetBucketLogging where
    toPath GetBucketLogging{..} = mconcat
        [ "/"
        , toText _gbl2Bucket
        ]

instance ToQuery GetBucketLogging where
    toQuery = const "logging"

instance ToHeaders GetBucketLogging

instance ToXMLRoot GetBucketLogging where
    toXMLRoot = const (element "GetBucketLogging" [])

instance ToXML GetBucketLogging

instance AWSRequest GetBucketLogging where
    type Sv GetBucketLogging = S3
    type Rs GetBucketLogging = GetBucketLoggingResponse

    request  = get
    response = xmlResponse

instance FromXML GetBucketLoggingResponse where
    parseXML x = GetBucketLoggingResponse
        <$> x .@? "LoggingEnabled"
