{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/PutBucketPolicy.html>
module Network.AWS.S3.PutBucketPolicy
    (
    -- * Request
      PutBucketPolicy
    -- ** Request constructor
    , putBucketPolicy
    -- ** Request lenses
    , pbpBucket
    , pbpContentMD5
    , pbpPolicy

    -- * Response
    , PutBucketPolicyResponse
    -- ** Response constructor
    , putBucketPolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.S3.Types
import qualified GHC.Exts

data PutBucketPolicy = PutBucketPolicy
    { _pbpBucket     :: Text
    , _pbpContentMD5 :: Maybe Text
    , _pbpPolicy     :: Text
    } deriving (Eq, Ord, Show)

-- | 'PutBucketPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbpBucket' @::@ 'Text'
--
-- * 'pbpContentMD5' @::@ 'Maybe' 'Text'
--
-- * 'pbpPolicy' @::@ 'Text'
--
putBucketPolicy :: Text -- ^ 'pbpBucket'
                -> Text -- ^ 'pbpPolicy'
                -> PutBucketPolicy
putBucketPolicy p1 p2 = PutBucketPolicy
    { _pbpBucket     = p1
    , _pbpPolicy     = p2
    , _pbpContentMD5 = Nothing
    }

pbpBucket :: Lens' PutBucketPolicy Text
pbpBucket = lens _pbpBucket (\s a -> s { _pbpBucket = a })

pbpContentMD5 :: Lens' PutBucketPolicy (Maybe Text)
pbpContentMD5 = lens _pbpContentMD5 (\s a -> s { _pbpContentMD5 = a })

-- | The bucket policy as a JSON document.
pbpPolicy :: Lens' PutBucketPolicy Text
pbpPolicy = lens _pbpPolicy (\s a -> s { _pbpPolicy = a })

data PutBucketPolicyResponse = PutBucketPolicyResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'PutBucketPolicyResponse' constructor.
putBucketPolicyResponse :: PutBucketPolicyResponse
putBucketPolicyResponse = PutBucketPolicyResponse

instance ToPath PutBucketPolicy where
    toPath PutBucketPolicy{..} = mconcat
        [ "/"
        , toText _pbpBucket
        ]

instance ToQuery PutBucketPolicy where
    toQuery = const "policy"

instance ToHeaders PutBucketPolicy where
    toHeaders PutBucketPolicy{..} = mconcat
        [ "Content-MD5" =: _pbpContentMD5
        ]

instance ToXMLRoot PutBucketPolicy where
    toXMLRoot PutBucketPolicy{..} = element "PutBucketPolicy"
        [ "Policy" =@ _pbpPolicy
        ]

instance ToXML PutBucketPolicy

instance AWSRequest PutBucketPolicy where
    type Sv PutBucketPolicy = S3
    type Rs PutBucketPolicy = PutBucketPolicyResponse

    request  = put
    response = nullResponse PutBucketPolicyResponse


Some kind of operator / class to check the types whether to continue?
