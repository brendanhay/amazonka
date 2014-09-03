{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.PutBucketLogging
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
module Network.AWS.S3.V2006_03_01.PutBucketLogging
    (
    -- * Request
      PutBucketLogging
    -- ** Request constructor
    , putBucketLogging
    -- ** Request lenses
    , pblsBucketLoggingStatus
    , pblsBucket
    , pblsContentMD5

    -- * Response
    , PutBucketLoggingResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'PutBucketLogging' request.
putBucketLogging :: BucketLoggingStatus -- ^ 'pblsBucketLoggingStatus'
                 -> BucketName -- ^ 'pblsBucket'
                 -> PutBucketLogging
putBucketLogging p1 p2 = PutBucketLogging
    { _pblsBucketLoggingStatus = p1
    , _pblsBucket = p2
    , _pblsContentMD5 = Nothing
    }

data PutBucketLogging = PutBucketLogging
    { _pblsBucketLoggingStatus :: BucketLoggingStatus
    , _pblsBucket :: BucketName
    , _pblsContentMD5 :: Maybe Text
    } deriving (Show, Generic)

pblsBucketLoggingStatus
    :: Functor f
    => (BucketLoggingStatus
    -> f (BucketLoggingStatus))
    -> PutBucketLogging
    -> f PutBucketLogging
pblsBucketLoggingStatus f x =
    (\y -> x { _pblsBucketLoggingStatus = y })
       <$> f (_pblsBucketLoggingStatus x)
{-# INLINE pblsBucketLoggingStatus #-}

pblsBucket
    :: Functor f
    => (BucketName
    -> f (BucketName))
    -> PutBucketLogging
    -> f PutBucketLogging
pblsBucket f x =
    (\y -> x { _pblsBucket = y })
       <$> f (_pblsBucket x)
{-# INLINE pblsBucket #-}

pblsContentMD5
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PutBucketLogging
    -> f PutBucketLogging
pblsContentMD5 f x =
    (\y -> x { _pblsContentMD5 = y })
       <$> f (_pblsContentMD5 x)
{-# INLINE pblsContentMD5 #-}

instance ToPath PutBucketLogging where
    toPath PutBucketLogging{..} = mconcat
        [ "/"
        , toBS _pblsBucket
        ]

instance ToQuery PutBucketLogging where
    toQuery PutBucketLogging{..} = mconcat
        [ "logging"
        ]

instance ToHeaders PutBucketLogging where
    toHeaders PutBucketLogging{..} = concat
        [ "Content-MD5" =: _pblsContentMD5
        ]

instance ToBody PutBucketLogging where
    toBody = toBody . encodeXML . _pblsBucketLoggingStatus

data PutBucketLoggingResponse = PutBucketLoggingResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutBucketLogging where
    type Sv PutBucketLogging = S3
    type Rs PutBucketLogging = PutBucketLoggingResponse

    request = put
    response _ = nullaryResponse PutBucketLoggingResponse
