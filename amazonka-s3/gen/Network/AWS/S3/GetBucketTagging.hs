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

-- Module      : Network.AWS.S3.GetBucketTagging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the tag set associated with the bucket.
module Network.AWS.S3.GetBucketTagging
    (
    -- * Request
      GetBucketTagging
    -- ** Request constructor
    , getBucketTagging
    -- ** Request lenses
    , gbtBucket

    -- * Response
    , GetBucketTaggingOutput
    -- ** Response constructor
    , getBucketTaggingOutput
    -- ** Response lenses
    , gbtoTagSet
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

newtype GetBucketTagging = GetBucketTagging
    { _gbtBucket :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'GetBucketTagging' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbtBucket' @::@ 'Text'
--
getBucketTagging :: Text -- ^ 'gbtBucket'
                 -> GetBucketTagging
getBucketTagging p1 = GetBucketTagging
    { _gbtBucket = p1
    }

gbtBucket :: Lens' GetBucketTagging Text
gbtBucket = lens _gbtBucket (\s a -> s { _gbtBucket = a })

instance ToPath GetBucketTagging where
    toPath GetBucketTagging{..} = mconcat
        [ "/"
        , toText _gbtBucket
        ]

instance ToQuery GetBucketTagging where
    toQuery = const "tagging"

instance ToHeaders GetBucketTagging

newtype GetBucketTaggingOutput = GetBucketTaggingOutput
    { _gbtoTagSet :: [Tag]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'GetBucketTaggingOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbtoTagSet' @::@ ['Tag']
--
getBucketTaggingOutput :: GetBucketTaggingOutput
getBucketTaggingOutput = GetBucketTaggingOutput
    { _gbtoTagSet = mempty
    }

gbtoTagSet :: Lens' GetBucketTaggingOutput [Tag]
gbtoTagSet = lens _gbtoTagSet (\s a -> s { _gbtoTagSet = a })

instance AWSRequest GetBucketTagging where
    type Sv GetBucketTagging = S3
    type Rs GetBucketTagging = GetBucketTaggingOutput

    request  = get
    response = const . xmlResponse $ \h x -> GetBucketTaggingOutput
newtype
