{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketTagging.html>
module Network.AWS.S3.GetBucketTagging
    (
    -- * Request
      GetBucketTagging
    -- ** Request constructor
    , getBucketTagging
    -- ** Request lenses
    , gbtBucket

    -- * Response
    , GetBucketTaggingResponse
    -- ** Response constructor
    , getBucketTaggingResponse
    -- ** Response lenses
    , gbtrTagSet
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.S3.Types
import qualified GHC.Exts

newtype GetBucketTagging = GetBucketTagging
    { _gbtBucket :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

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

newtype GetBucketTaggingResponse = GetBucketTaggingResponse
    { _gbtrTagSet :: [Tag]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList GetBucketTaggingResponse where
    type Item GetBucketTaggingResponse = Tag

    fromList = GetBucketTaggingResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _gbtrTagSet

-- | 'GetBucketTaggingResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbtrTagSet' @::@ ['Tag']
--
getBucketTaggingResponse :: GetBucketTaggingResponse
getBucketTaggingResponse = GetBucketTaggingResponse
    { _gbtrTagSet = mempty
    }

gbtrTagSet :: Lens' GetBucketTaggingResponse [Tag]
gbtrTagSet = lens _gbtrTagSet (\s a -> s { _gbtrTagSet = a })

instance ToPath GetBucketTagging where
    toPath GetBucketTagging{..} = mconcat
        [ "/"
        , toText _gbtBucket
        ]

instance ToQuery GetBucketTagging where
    toQuery = const "tagging"

instance ToHeaders GetBucketTagging

instance ToXML GetBucketTagging where
    toXML = const (node "GetBucketTagging" [])

instance AWSRequest GetBucketTagging where
    type Sv GetBucketTagging = S3
    type Rs GetBucketTagging = GetBucketTaggingResponse

    request  = get
    response = xmlResponse

instance FromXML GetBucketTaggingResponse where
    parseXML c = GetBucketTaggingResponse
        <$> c .: "TagSet"
