{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    , mkGetBucketPolicy
    -- ** Request lenses
    , gbpBucket

    -- * Response
    , GetBucketPolicyResponse
    -- ** Response constructor
    , mkGetBucketPolicyResponse
    -- ** Response lenses
    , gbprPolicy
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

newtype GetBucketPolicy = GetBucketPolicy
    { _gbpBucket :: !BucketName
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketPolicy' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @BucketName@
--
mkGetBucketPolicy :: BucketName -- ^ 'gbpBucket'
                  -> GetBucketPolicy
mkGetBucketPolicy p1 = GetBucketPolicy
    { _gbpBucket = p1
    }

gbpBucket :: Lens' GetBucketPolicy BucketName
gbpBucket = lens _gbpBucket (\s a -> s { _gbpBucket = a })

instance ToPath GetBucketPolicy

instance ToQuery GetBucketPolicy

instance ToHeaders GetBucketPolicy

instance ToBody GetBucketPolicy

newtype GetBucketPolicyResponse = GetBucketPolicyResponse
    { _gbprPolicy :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketPolicyResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Policy ::@ @Maybe Text@
--
mkGetBucketPolicyResponse :: GetBucketPolicyResponse
mkGetBucketPolicyResponse = GetBucketPolicyResponse
    { _gbprPolicy = Nothing
    }

-- | The bucket policy as a JSON document.
gbprPolicy :: Lens' GetBucketPolicyResponse (Maybe Text)
gbprPolicy = lens _gbprPolicy (\s a -> s { _gbprPolicy = a })

instance FromXML GetBucketPolicyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketPolicy where
    type Sv GetBucketPolicy = S3
    type Rs GetBucketPolicy = GetBucketPolicyResponse

    request = get
    response _ = xmlResponse
