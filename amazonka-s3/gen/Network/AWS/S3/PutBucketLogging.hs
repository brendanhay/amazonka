{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    , pblr1Bucket
    , pblr1BucketLoggingStatus
    , pblr1ContentMD5

    -- * Response
    , Empty
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Xml
import Network.AWS.S3.Types

data PutBucketLogging = PutBucketLogging
    { _pblr1Bucket              :: BucketName
    , _pblr1BucketLoggingStatus :: BucketLoggingStatus
    , _pblr1ContentMD5          :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'PutBucketLogging' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pblr1Bucket' @::@ 'BucketName'
--
-- * 'pblr1BucketLoggingStatus' @::@ 'BucketLoggingStatus'
--
-- * 'pblr1ContentMD5' @::@ 'Maybe' 'Text'
--
putBucketLogging :: BucketName -- ^ 'pblr1Bucket'
                 -> BucketLoggingStatus -- ^ 'pblr1BucketLoggingStatus'
                 -> PutBucketLogging
putBucketLogging p1 p2 = PutBucketLogging
    { _pblr1Bucket              = p1
    , _pblr1BucketLoggingStatus = p2
    , _pblr1ContentMD5          = Nothing
    }

pblr1Bucket :: Lens' PutBucketLogging BucketName
pblr1Bucket = lens _pblr1Bucket (\s a -> s { _pblr1Bucket = a })

pblr1BucketLoggingStatus :: Lens' PutBucketLogging BucketLoggingStatus
pblr1BucketLoggingStatus =
    lens _pblr1BucketLoggingStatus
        (\s a -> s { _pblr1BucketLoggingStatus = a })

pblr1ContentMD5 :: Lens' PutBucketLogging (Maybe Text)
pblr1ContentMD5 = lens _pblr1ContentMD5 (\s a -> s { _pblr1ContentMD5 = a })

instance ToPath PutBucketLogging where
    toPath PutBucketLogging{..} = mconcat
        [ "/"
        , toText _pblr1Bucket
        ]

instance ToQuery PutBucketLogging where
    toQuery = const "logging"

instance ToHeaders PutBucketLogging where
    toHeaders PutBucketLogging{..} = mconcat
        [ "Content-MD5" =: _pblr1ContentMD5
        ]

instance AWSRequest PutBucketLogging where
    type Sv PutBucketLogging = S3
    type Rs PutBucketLogging = Empty

    request  = put
    response = const (nullaryResponse Empty)
