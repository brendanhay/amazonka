{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.S3.V2006_03_01.PutBucketPolicy where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'PutBucketPolicy' request.
putBucketPolicy :: Text -- ^ '_pbprPolicy'
                -> BucketName -- ^ '_pbprBucket'
                -> PutBucketPolicy
putBucketPolicy p1 p2 = PutBucketPolicy
    { _pbprPolicy = p1
    , _pbprBucket = p2
    , _pbprContentMD5 = Nothing
    }

data PutBucketPolicy = PutBucketPolicy
    { _pbprPolicy :: Text
      -- ^ The bucket policy as a JSON document.
    , _pbprBucket :: BucketName
    , _pbprContentMD5 :: Maybe Text
    } deriving (Show, Generic)

makeLenses ''PutBucketPolicy

instance ToPath PutBucketPolicy where
    toPath PutBucketPolicy{..} = mconcat
        [ "/"
        , toBS _pbprBucket
        ]

instance ToQuery PutBucketPolicy where
    toQuery PutBucketPolicy{..} = mconcat
        [ "policy"
        ]

instance ToHeaders PutBucketPolicy where
    toHeaders PutBucketPolicy{..} = concat
        [ "Content-MD5" =: _pbprContentMD5
        ]

instance ToBody PutBucketPolicy where
    toBody = toBody . encodeXML . _pbprPolicy

data PutBucketPolicyResponse = PutBucketPolicyResponse
    deriving (Eq, Show, Generic)

makeLenses ''PutBucketPolicyResponse

instance AWSRequest PutBucketPolicy where
    type Sv PutBucketPolicy = S3
    type Rs PutBucketPolicy = PutBucketPolicyResponse

    request = put
    response _ = nullaryResponse PutBucketPolicyResponse
