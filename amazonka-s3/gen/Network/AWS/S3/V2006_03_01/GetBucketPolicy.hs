{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetBucketPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the policy of a specified bucket.
module Network.AWS.S3.V2006_03_01.GetBucketPolicy
    (
    -- * Request
      GetBucketPolicy
    -- ** Request constructor
    , mkGetBucketPolicyRequest
    -- ** Request lenses
    , gbprBucket

    -- * Response
    , GetBucketPolicyResponse
    -- ** Response lenses
    , gbpoPolicy
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketPolicy' request.
mkGetBucketPolicyRequest :: BucketName -- ^ 'gbprBucket'
                         -> GetBucketPolicy
mkGetBucketPolicyRequest p1 = GetBucketPolicy
    { _gbprBucket = p1
    }
{-# INLINE mkGetBucketPolicyRequest #-}

newtype GetBucketPolicy = GetBucketPolicy
    { _gbprBucket :: BucketName
    } deriving (Show, Generic)

gbprBucket :: Lens' GetBucketPolicy (BucketName)
gbprBucket = lens _gbprBucket (\s a -> s { _gbprBucket = a })
{-# INLINE gbprBucket #-}

instance ToPath GetBucketPolicy where
    toPath GetBucketPolicy{..} = mconcat
        [ "/"
        , toBS _gbprBucket
        ]

instance ToQuery GetBucketPolicy where
    toQuery GetBucketPolicy{..} = mconcat
        [ "policy"
        ]

instance ToHeaders GetBucketPolicy

instance ToBody GetBucketPolicy

newtype GetBucketPolicyResponse = GetBucketPolicyResponse
    { _gbpoPolicy :: Maybe Text
      -- ^ The bucket policy as a JSON document.
    } deriving (Show, Generic)

-- | The bucket policy as a JSON document.
gbpoPolicy :: Lens' GetBucketPolicyResponse (Maybe Text)
gbpoPolicy = lens _gbpoPolicy (\s a -> s { _gbpoPolicy = a })
{-# INLINE gbpoPolicy #-}

instance FromXML GetBucketPolicyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketPolicy where
    type Sv GetBucketPolicy = S3
    type Rs GetBucketPolicy = GetBucketPolicyResponse

    request = get
    response _ = xmlResponse
