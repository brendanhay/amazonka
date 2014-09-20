{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
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

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

newtype GetBucketTagging = GetBucketTagging
    { _gbtBucket :: BucketName
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketTagging' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @BucketName@
--
getBucketTagging :: BucketName -- ^ 'gbtBucket'
                 -> GetBucketTagging
getBucketTagging p1 = GetBucketTagging
    { _gbtBucket = p1
    }

gbtBucket :: Lens' GetBucketTagging BucketName
gbtBucket = lens _gbtBucket (\s a -> s { _gbtBucket = a })

instance ToPath GetBucketTagging

instance ToQuery GetBucketTagging

instance ToHeaders GetBucketTagging

instance ToBody GetBucketTagging

newtype GetBucketTaggingResponse = GetBucketTaggingResponse
    { _gbtrTagSet :: [Tag]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketTaggingResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TagSet ::@ @[Tag]@
--
getBucketTaggingResponse :: [Tag] -- ^ 'gbtrTagSet'
                         -> GetBucketTaggingResponse
getBucketTaggingResponse p1 = GetBucketTaggingResponse
    { _gbtrTagSet = p1
    }

gbtrTagSet :: Lens' GetBucketTaggingResponse [Tag]
gbtrTagSet = lens _gbtrTagSet (\s a -> s { _gbtrTagSet = a })

instance FromXML GetBucketTaggingResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketTagging where
    type Sv GetBucketTagging = S3
    type Rs GetBucketTagging = GetBucketTaggingResponse

    request = get
    response _ = xmlResponse
