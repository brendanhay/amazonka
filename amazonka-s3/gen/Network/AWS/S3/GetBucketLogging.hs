{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
module Network.AWS.S3.GetBucketLogging
    (
    -- * Request
      GetBucketLogging
    -- ** Request constructor
    , mkGetBucketLogging
    -- ** Request lenses
    , gbl2Bucket

    -- * Response
    , GetBucketLoggingResponse
    -- ** Response constructor
    , mkGetBucketLoggingResponse
    -- ** Response lenses
    , gblr1LoggingEnabled
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

newtype GetBucketLogging = GetBucketLogging
    { _gbl2Bucket :: !BucketName
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketLogging' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @BucketName@
--
mkGetBucketLogging :: BucketName -- ^ 'gbl2Bucket'
                   -> GetBucketLogging
mkGetBucketLogging p1 = GetBucketLogging
    { _gbl2Bucket = p1
    }

gbl2Bucket :: Lens' GetBucketLogging BucketName
gbl2Bucket = lens _gbl2Bucket (\s a -> s { _gbl2Bucket = a })

instance ToPath GetBucketLogging

instance ToQuery GetBucketLogging

instance ToHeaders GetBucketLogging

instance ToBody GetBucketLogging

newtype GetBucketLoggingResponse = GetBucketLoggingResponse
    { _gblr1LoggingEnabled :: Maybe LoggingEnabled
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketLoggingResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoggingEnabled ::@ @Maybe LoggingEnabled@
--
mkGetBucketLoggingResponse :: GetBucketLoggingResponse
mkGetBucketLoggingResponse = GetBucketLoggingResponse
    { _gblr1LoggingEnabled = Nothing
    }

gblr1LoggingEnabled :: Lens' GetBucketLoggingResponse (Maybe LoggingEnabled)
gblr1LoggingEnabled =
    lens _gblr1LoggingEnabled (\s a -> s { _gblr1LoggingEnabled = a })

instance FromXML GetBucketLoggingResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketLogging where
    type Sv GetBucketLogging = S3
    type Rs GetBucketLogging = GetBucketLoggingResponse

    request = get
    response _ = xmlResponse
