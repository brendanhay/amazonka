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
    , gbl2Bucket

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
    { _gbl2Bucket :: Text
    } (Eq, Ord, Show, Generic, Monoid)

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

instance ToPath GetBucketLogging where
    toPath GetBucketLogging{..} = mconcat
        [ "/"
        , toText _gbl2Bucket
        ]

instance ToQuery GetBucketLogging where
    toQuery = const "logging"

instance ToHeaders GetBucketLogging

newtype GetBucketLoggingOutput = GetBucketLoggingOutput
    { _gbloLoggingEnabled :: Maybe LoggingEnabled
    } (Eq, Show, Generic)

-- | 'GetBucketLoggingOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbloLoggingEnabled' @::@ 'Maybe' 'LoggingEnabled'
--
getBucketLoggingOutput :: GetBucketLoggingOutput
getBucketLoggingOutput = GetBucketLoggingOutput
    { _gbloLoggingEnabled = Nothing
    }

gbloLoggingEnabled :: Lens' GetBucketLoggingOutput (Maybe LoggingEnabled)
gbloLoggingEnabled =
    lens _gbloLoggingEnabled (\s a -> s { _gbloLoggingEnabled = a })

instance FromXML GetBucketLoggingOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetBucketLoggingOutput"
instance AWSRequest GetBucketLogging where
    type Sv GetBucketLogging = S3
    type Rs GetBucketLogging = GetBucketLoggingOutput

    request  = get
    response = xmlResponse $ \h x -> GetBucketLoggingOutput
        <$> x %| "LoggingEnabled"
