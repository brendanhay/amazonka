{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    , gbprBucket

    -- * Response
    , GetBucketPolicyOutput
    -- ** Response constructor
    , getBucketPolicyOutput
    -- ** Response lenses
    , gbpoPolicy
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

newtype GetBucketPolicy = GetBucketPolicy
    { _gbprBucket :: BucketName
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetBucketPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbprBucket' @::@ 'BucketName'
--
getBucketPolicy :: BucketName -- ^ 'gbprBucket'
                -> GetBucketPolicy
getBucketPolicy p1 = GetBucketPolicy
    { _gbprBucket = p1
    }

gbprBucket :: Lens' GetBucketPolicy BucketName
gbprBucket = lens _gbprBucket (\s a -> s { _gbprBucket = a })

instance ToPath GetBucketPolicy where
    toPath GetBucketPolicy{..} = mconcat
        [ "/"
        , toText _gbprBucket
        ]

instance ToQuery GetBucketPolicy where
    toQuery = const "policy"

instance ToHeaders GetBucketPolicy

newtype GetBucketPolicyOutput = GetBucketPolicyOutput
    { _gbpoPolicy :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'GetBucketPolicyOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbpoPolicy' @::@ 'Maybe' 'Text'
--
getBucketPolicyOutput :: GetBucketPolicyOutput
getBucketPolicyOutput = GetBucketPolicyOutput
    { _gbpoPolicy = Nothing
    }

-- | The bucket policy as a JSON document.
gbpoPolicy :: Lens' GetBucketPolicyOutput (Maybe Text)
gbpoPolicy = lens _gbpoPolicy (\s a -> s { _gbpoPolicy = a })

instance AWSRequest GetBucketPolicy where
    type Sv GetBucketPolicy = S3
    type Rs GetBucketPolicy = GetBucketPolicyOutput

    request  = get'
    response = const . xmlResponse $ \h x -> GetBucketPolicyOutput
        <$> x %| "Policy"
