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
    , mkPutBucketPolicy
    -- ** Request lenses
    , pbpBucket
    , pbpContentMD5
    , pbpPolicy

    -- * Response
    , PutBucketPolicyResponse
    -- ** Response constructor
    , mkPutBucketPolicyResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

data PutBucketPolicy = PutBucketPolicy
    { _pbpBucket :: BucketName
    , _pbpContentMD5 :: Maybe Text
    , _pbpPolicy :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutBucketPolicy' request.
mkPutBucketPolicy :: BucketName -- ^ 'pbpBucket'
                  -> Text -- ^ 'pbpPolicy'
                  -> PutBucketPolicy
mkPutBucketPolicy p1 p3 = PutBucketPolicy
    { _pbpBucket = p1
    , _pbpContentMD5 = Nothing
    , _pbpPolicy = p3
    }

pbpBucket :: Lens' PutBucketPolicy BucketName
pbpBucket = lens _pbpBucket (\s a -> s { _pbpBucket = a })

pbpContentMD5 :: Lens' PutBucketPolicy (Maybe Text)
pbpContentMD5 = lens _pbpContentMD5 (\s a -> s { _pbpContentMD5 = a })

-- | The bucket policy as a JSON document.
pbpPolicy :: Lens' PutBucketPolicy Text
pbpPolicy = lens _pbpPolicy (\s a -> s { _pbpPolicy = a })

instance ToPath PutBucketPolicy

instance ToQuery PutBucketPolicy

instance ToHeaders PutBucketPolicy where
    toHeaders PutBucketPolicy{..} = concat
        [ "Content-MD5" =: _pbpContentMD5
        ]

instance ToBody PutBucketPolicy where
    toBody = toBody . encodeXML . _pbpPolicy

data PutBucketPolicyResponse = PutBucketPolicyResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutBucketPolicyResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkPutBucketPolicyResponse :: PutBucketPolicyResponse
mkPutBucketPolicyResponse = PutBucketPolicyResponse

instance AWSRequest PutBucketPolicy where
    type Sv PutBucketPolicy = S3
    type Rs PutBucketPolicy = PutBucketPolicyResponse

    request = get
    response _ = nullaryResponse PutBucketPolicyResponse
