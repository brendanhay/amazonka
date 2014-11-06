{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutBucketPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Replaces a policy on a bucket. If the bucket already has a policy, the one
-- in this request completely replaces it.
module Network.AWS.S3.PutBucketPolicy
    (
    -- * Request
      PutBucketPolicy
    -- ** Request constructor
    , putBucketPolicy
    -- ** Request lenses
    , pbprBucket
    , pbprContentMD5
    , pbprPolicy

    -- * Response
    , Empty
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

data PutBucketPolicy = PutBucketPolicy
    { _pbprBucket     :: BucketName
    , _pbprContentMD5 :: Maybe Text
    , _pbprPolicy     :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'PutBucketPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbprBucket' @::@ 'BucketName'
--
-- * 'pbprContentMD5' @::@ 'Maybe' 'Text'
--
-- * 'pbprPolicy' @::@ 'Text'
--
putBucketPolicy :: BucketName -- ^ 'pbprBucket'
                -> Text -- ^ 'pbprPolicy'
                -> PutBucketPolicy
putBucketPolicy p1 p2 = PutBucketPolicy
    { _pbprBucket     = p1
    , _pbprPolicy     = p2
    , _pbprContentMD5 = Nothing
    }

pbprBucket :: Lens' PutBucketPolicy BucketName
pbprBucket = lens _pbprBucket (\s a -> s { _pbprBucket = a })

pbprContentMD5 :: Lens' PutBucketPolicy (Maybe Text)
pbprContentMD5 = lens _pbprContentMD5 (\s a -> s { _pbprContentMD5 = a })

-- | The bucket policy as a JSON document.
pbprPolicy :: Lens' PutBucketPolicy Text
pbprPolicy = lens _pbprPolicy (\s a -> s { _pbprPolicy = a })

instance ToPath PutBucketPolicy where
    toPath PutBucketPolicy{..} = mconcat
        [ "/"
        , toText _pbprBucket
        ]

instance ToQuery PutBucketPolicy where
    toQuery = const "policy"

instance ToHeaders PutBucketPolicy where
    toHeaders PutBucketPolicy{..} = mconcat
        [ "Content-MD5" =: _pbprContentMD5
        ]

instance ToBody PutBucketPolicy where
    toBody = toBody . encodeXML . _pbprPolicy

instance AWSRequest PutBucketPolicy where
    type Sv PutBucketPolicy = S3
    type Rs PutBucketPolicy = Empty

    request  = put
    response = const (nullaryResponse Empty)
