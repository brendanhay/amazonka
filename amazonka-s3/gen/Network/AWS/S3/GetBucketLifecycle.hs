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
module Network.AWS.S3.GetBucketLifecycle
    (
    -- * Request
      GetBucketLifecycle
    -- ** Request constructor
    , getBucketLifecycle
    -- ** Request lenses
    , gbl1Bucket

    -- * Response
    , GetBucketLifecycleOutput
    -- ** Response constructor
    , getBucketLifecycleResponse
    -- ** Response lenses
    , gbloRules
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

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

instance ToPath GetBucketLifecycle where
    toPath GetBucketLifecycle{..} = mconcat
        [ "/"
        , toText _gbl1Bucket
        ]

instance ToQuery GetBucketLifecycle where
    toQuery = const "lifecycle"

instance ToHeaders GetBucketLifecycle

newtype GetBucketLifecycleOutput = GetBucketLifecycleOutput
    { _gbloRules :: [Rule]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance IsList GetBucketLifecycleOutput where
    type Item GetBucketLifecycleOutput = Rule

    fromList = GetBucketLifecycleOutput . fromList
    toList   = toList . _gbloRules

-- | 'GetBucketLifecycleOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbloRules' @::@ ['Rule']
--
getBucketLifecycleResponse :: GetBucketLifecycleOutput
getBucketLifecycleResponse = GetBucketLifecycleOutput
    { _gbloRules = mempty
    }

gbloRules :: Lens' GetBucketLifecycleOutput [Rule]
gbloRules = lens _gbloRules (\s a -> s { _gbloRules = a })

instance FromXML GetBucketLifecycleOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetBucketLifecycleOutput"
instance AWSRequest GetBucketLifecycle where
    type Sv GetBucketLifecycle = S3
    type Rs GetBucketLifecycle = GetBucketLifecycleOutput

    request  = get
    response = xmlResponse $ \h x -> GetBucketLifecycleOutput
        <$> x %| "Rule"
