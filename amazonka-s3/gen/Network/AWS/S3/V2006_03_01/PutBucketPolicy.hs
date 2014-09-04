{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.PutBucketPolicy
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
module Network.AWS.S3.V2006_03_01.PutBucketPolicy
    (
    -- * Request
      PutBucketPolicy
    -- ** Request constructor
    , mkPutBucketPolicyRequest
    -- ** Request lenses
    , pbprBucket
    , pbprContentMD5
    , pbprPolicy

    -- * Response
    , PutBucketPolicyResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutBucketPolicy' request.
mkPutBucketPolicyRequest :: BucketName -- ^ 'pbprBucket'
                         -> Text -- ^ 'pbprPolicy'
                         -> PutBucketPolicy
mkPutBucketPolicyRequest p1 p2 = PutBucketPolicy
    { _pbprBucket = p1
    , _pbprContentMD5 = Nothing
    , _pbprPolicy = p3
    }
{-# INLINE mkPutBucketPolicyRequest #-}

data PutBucketPolicy = PutBucketPolicy
    { _pbprBucket :: BucketName
    , _pbprContentMD5 :: Maybe Text
    , _pbprPolicy :: Text
      -- ^ The bucket policy as a JSON document.
    } deriving (Show, Generic)

pbprBucket :: Lens' PutBucketPolicy (BucketName)
pbprBucket = lens _pbprBucket (\s a -> s { _pbprBucket = a })
{-# INLINE pbprBucket #-}

pbprContentMD5 :: Lens' PutBucketPolicy (Maybe Text)
pbprContentMD5 = lens _pbprContentMD5 (\s a -> s { _pbprContentMD5 = a })
{-# INLINE pbprContentMD5 #-}

-- | The bucket policy as a JSON document.
pbprPolicy :: Lens' PutBucketPolicy (Text)
pbprPolicy = lens _pbprPolicy (\s a -> s { _pbprPolicy = a })
{-# INLINE pbprPolicy #-}

instance ToPath PutBucketPolicy where
    toPath PutBucketPolicy{..} = mconcat
        [ "/"
        , toBS _pbprBucket
        ]

instance ToQuery PutBucketPolicy where
    toQuery PutBucketPolicy{..} = mconcat
        [ "policy"
        ]

instance ToHeaders PutBucketPolicy

instance ToBody PutBucketPolicy

data PutBucketPolicyResponse = PutBucketPolicyResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutBucketPolicy where
    type Sv PutBucketPolicy = S3
    type Rs PutBucketPolicy = PutBucketPolicyResponse

    request = put
    response _ = nullaryResponse PutBucketPolicyResponse
