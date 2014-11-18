{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketLifecycle
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the lifecycle configuration information set on the bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketLifecycle.html>
module Network.AWS.S3.GetBucketLifecycle
    (
    -- * Request
      GetBucketLifecycle
    -- ** Request constructor
    , getBucketLifecycle
    -- ** Request lenses
    , gbl1Bucket

    -- * Response
    , GetBucketLifecycleResponse
    -- ** Response constructor
    , getBucketLifecycleResponse
    -- ** Response lenses
    , gblrRules
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.S3.Types
import qualified GHC.Exts

newtype GetBucketLifecycle = GetBucketLifecycle
    { _gbl1Bucket :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetBucketLifecycle' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbl1Bucket' @::@ 'Text'
--
getBucketLifecycle :: Text -- ^ 'gbl1Bucket'
                   -> GetBucketLifecycle
getBucketLifecycle p1 = GetBucketLifecycle
    { _gbl1Bucket = p1
    }

gbl1Bucket :: Lens' GetBucketLifecycle Text
gbl1Bucket = lens _gbl1Bucket (\s a -> s { _gbl1Bucket = a })

newtype GetBucketLifecycleResponse = GetBucketLifecycleResponse
    { _gblrRules :: [Rule]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList GetBucketLifecycleResponse where
    type Item GetBucketLifecycleResponse = Rule

    fromList = GetBucketLifecycleResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _gblrRules

-- | 'GetBucketLifecycleResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gblrRules' @::@ ['Rule']
--
getBucketLifecycleResponse :: GetBucketLifecycleResponse
getBucketLifecycleResponse = GetBucketLifecycleResponse
    { _gblrRules = mempty
    }

gblrRules :: Lens' GetBucketLifecycleResponse [Rule]
gblrRules = lens _gblrRules (\s a -> s { _gblrRules = a })

instance ToPath GetBucketLifecycle where
    toPath GetBucketLifecycle{..} = mconcat
        [ "/"
        , toText _gbl1Bucket
        ]

instance ToQuery GetBucketLifecycle where
    toQuery = const "lifecycle"

instance ToHeaders GetBucketLifecycle

instance ToXML GetBucketLifecycle where
    toXML = const (node "GetBucketLifecycle" [])

instance AWSRequest GetBucketLifecycle where
    type Sv GetBucketLifecycle = S3
    type Rs GetBucketLifecycle = GetBucketLifecycleResponse

    request  = get
    response = xmlResponse

instance FromXML GetBucketLifecycleResponse where
    parseXML c = GetBucketLifecycleResponse
        <$> c .: "Rule"
