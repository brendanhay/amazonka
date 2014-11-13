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

-- Module      : Network.AWS.S3.GetBucketPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the policy of a specified bucket.
module Network.AWS.S3.GetBucketPolicy
    (
    -- * Request
      GetBucketPolicy
    -- ** Request constructor
    , getBucketPolicy
    -- ** Request lenses
    , gbpBucket

    -- * Response
    , GetBucketPolicyResponse
    -- ** Response constructor
    , getBucketPolicyResponse
    -- ** Response lenses
    , gbprPolicy
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

newtype GetBucketPolicy = GetBucketPolicy
    { _gbpBucket :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetBucketPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbpBucket' @::@ 'Text'
--
getBucketPolicy :: Text -- ^ 'gbpBucket'
                -> GetBucketPolicy
getBucketPolicy p1 = GetBucketPolicy
    { _gbpBucket = p1
    }

gbpBucket :: Lens' GetBucketPolicy Text
gbpBucket = lens _gbpBucket (\s a -> s { _gbpBucket = a })

instance ToPath GetBucketPolicy where
    toPath GetBucketPolicy{..} = mconcat
        [ "/"
        , toText _gbpBucket
        ]

instance ToQuery GetBucketPolicy where
    toQuery = const "policy"

instance ToHeaders GetBucketPolicy

newtype GetBucketPolicyResponse = GetBucketPolicyResponse
    { _gbprPolicy :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'GetBucketPolicyResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbprPolicy' @::@ 'Maybe' 'Text'
--
getBucketPolicyResponse :: GetBucketPolicyResponse
getBucketPolicyResponse = GetBucketPolicyResponse
    { _gbprPolicy = Nothing
    }

-- | The bucket policy as a JSON document.
gbprPolicy :: Lens' GetBucketPolicyResponse (Maybe Text)
gbprPolicy = lens _gbprPolicy (\s a -> s { _gbprPolicy = a })

instance AWSRequest GetBucketPolicy where
    type Sv GetBucketPolicy = S3
    type Rs GetBucketPolicy = GetBucketPolicyResponse

    request  = get
    response = xmlResponse $ \h x -> GetBucketPolicyResponse
        <$> x %| "Policy"
