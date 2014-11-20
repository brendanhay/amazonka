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
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketPolicy.html>
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
import Network.AWS.Request.RestXML
import Network.AWS.S3.Types
import qualified GHC.Exts

newtype GetBucketPolicy = GetBucketPolicy
    { _gbpBucket :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

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

newtype GetBucketPolicyResponse = GetBucketPolicyResponse
    { _gbprPolicy :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

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

instance ToPath GetBucketPolicy where
    toPath GetBucketPolicy{..} = mconcat
        [ "/"
        , toText _gbpBucket
        ]

instance ToQuery GetBucketPolicy where
    toQuery = const "policy"

instance ToHeaders GetBucketPolicy

instance ToXMLRoot GetBucketPolicy where
    toXMLRoot = const (element "GetBucketPolicy" [])

instance ToXML GetBucketPolicy

xml

instance AWSRequest GetBucketPolicy where
    type Sv GetBucketPolicy = S3
    type Rs GetBucketPolicy = GetBucketPolicyResponse

    request  = get
    response = xmlResponse

instance FromXML GetBucketPolicyResponse where
    parseXML x = GetBucketPolicyResponse
        <$> x .@? "Policy"
