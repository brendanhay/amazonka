{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

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
    , pblBucket
    , pblBucketLoggingStatus
    , pblContentMD5

    -- * Response
    , PutBucketLoggingResponse
    -- ** Response constructor
    , putBucketLoggingResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types
import qualified GHC.Exts

data PutBucketLogging = PutBucketLogging
    { _pblBucket              :: Text
    , _pblBucketLoggingStatus :: BucketLoggingStatus
    , _pblContentMD5          :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'PutBucketLogging' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pblBucket' @::@ 'Text'
--
-- * 'pblBucketLoggingStatus' @::@ 'BucketLoggingStatus'
--
-- * 'pblContentMD5' @::@ 'Maybe' 'Text'
--
putBucketLogging :: Text -- ^ 'pblBucket'
                 -> BucketLoggingStatus -- ^ 'pblBucketLoggingStatus'
                 -> PutBucketLogging
putBucketLogging p1 p2 = PutBucketLogging
    { _pblBucket              = p1
    , _pblBucketLoggingStatus = p2
    , _pblContentMD5          = Nothing
    }

pblBucket :: Lens' PutBucketLogging Text
pblBucket = lens _pblBucket (\s a -> s { _pblBucket = a })

pblBucketLoggingStatus :: Lens' PutBucketLogging BucketLoggingStatus
pblBucketLoggingStatus =
    lens _pblBucketLoggingStatus (\s a -> s { _pblBucketLoggingStatus = a })

pblContentMD5 :: Lens' PutBucketLogging (Maybe Text)
pblContentMD5 = lens _pblContentMD5 (\s a -> s { _pblContentMD5 = a })

instance ToPath PutBucketLogging where
    toPath PutBucketLogging{..} = mconcat
        [ "/"
        , toText _pblBucket
        ]

instance ToQuery PutBucketLogging where
    toQuery = const "logging"

instance ToHeaders PutBucketLogging where
    toHeaders PutBucketLogging{..} = mconcat
        [ "Content-MD5" =: _pblContentMD5
        ]

instance ToBody PutBucketLogging where
    toBody = toBody . encodeXML . _pblBucketLoggingStatus

data PutBucketLoggingResponse = PutBucketLoggingResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'PutBucketLoggingResponse' constructor.
putBucketLoggingResponse :: PutBucketLoggingResponse
putBucketLoggingResponse = PutBucketLoggingResponse

instance AWSRequest PutBucketLogging where
    type Sv PutBucketLogging = S3
    type Rs PutBucketLogging = PutBucketLoggingResponse

    request  = put
    response = nullaryResponse PutBucketLoggingResponse
