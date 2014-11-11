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

-- Module      : Network.AWS.S3.PutBucketLogging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Set the logging parameters for a bucket and to specify permissions for who
-- can view and modify the logging parameters. To set the logging status of a
-- bucket, you must be the bucket owner.
module Network.AWS.S3.PutBucketLogging
    (
    -- * Request
      PutBucketLogging
    -- ** Request constructor
    , putBucketLogging
    -- ** Request lenses
    , pbl1Bucket
    , pbl1BucketLoggingStatus
    , pbl1ContentMD5

    -- * Response
    , PutBucketLoggingResponse
    -- ** Response constructor
    , putBucketLoggingResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

data PutBucketLogging = PutBucketLogging
    { _pbl1Bucket              :: Text
    , _pbl1BucketLoggingStatus :: BucketLoggingStatus
    , _pbl1ContentMD5          :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'PutBucketLogging' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbl1Bucket' @::@ 'Text'
--
-- * 'pbl1BucketLoggingStatus' @::@ 'BucketLoggingStatus'
--
-- * 'pbl1ContentMD5' @::@ 'Maybe' 'Text'
--
putBucketLogging :: Text -- ^ 'pbl1Bucket'
                 -> BucketLoggingStatus -- ^ 'pbl1BucketLoggingStatus'
                 -> PutBucketLogging
putBucketLogging p1 p2 = PutBucketLogging
    { _pbl1Bucket              = p1
    , _pbl1BucketLoggingStatus = p2
    , _pbl1ContentMD5          = Nothing
    }

pbl1Bucket :: Lens' PutBucketLogging Text
pbl1Bucket = lens _pbl1Bucket (\s a -> s { _pbl1Bucket = a })

pbl1BucketLoggingStatus :: Lens' PutBucketLogging BucketLoggingStatus
pbl1BucketLoggingStatus =
    lens _pbl1BucketLoggingStatus (\s a -> s { _pbl1BucketLoggingStatus = a })

pbl1ContentMD5 :: Lens' PutBucketLogging (Maybe Text)
pbl1ContentMD5 = lens _pbl1ContentMD5 (\s a -> s { _pbl1ContentMD5 = a })

instance ToPath PutBucketLogging where
    toPath PutBucketLogging{..} = mconcat
        [ "/"
        , toText _pbl1Bucket
        ]

instance ToQuery PutBucketLogging where
    toQuery = const "logging"

instance ToHeaders PutBucketLogging where
    toHeaders PutBucketLogging{..} = mconcat
        [ "Content-MD5" =: _pbl1ContentMD5
        ]

instance ToBody PutBucketLogging where
    toBody = toBody . encodeXML . _pbl1BucketLoggingStatus

data PutBucketLoggingResponse = PutBucketLoggingResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'PutBucketLoggingResponse' constructor.
putBucketLoggingResponse :: PutBucketLoggingResponse
putBucketLoggingResponse = PutBucketLoggingResponse

instance FromXML PutBucketLoggingResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PutBucketLoggingResponse"
instance AWSRequest PutBucketLogging where
    type Sv PutBucketLogging = S3
    type Rs PutBucketLogging = PutBucketLoggingResponse

    request  = put
    response = nullaryResponse PutBucketLoggingResponse
