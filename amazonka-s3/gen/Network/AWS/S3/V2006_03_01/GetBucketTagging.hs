{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetBucketTagging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the tag set associated with the bucket.
module Network.AWS.S3.V2006_03_01.GetBucketTagging
    (
    -- * Request
      GetBucketTagging
    -- ** Request constructor
    , mkGetBucketTagging
    -- ** Request lenses
    , gbtBucket

    -- * Response
    , GetBucketTaggingResponse
    -- ** Response lenses
    , gbtrsTagSet
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

newtype GetBucketTagging = GetBucketTagging
    { _gbtBucket :: BucketName
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketTagging' request.
mkGetBucketTagging :: BucketName -- ^ 'gbtBucket'
                   -> GetBucketTagging
mkGetBucketTagging p1 = GetBucketTagging
    { _gbtBucket = p1
    }

gbtBucket :: Lens' GetBucketTagging BucketName
gbtBucket = lens _gbtBucket (\s a -> s { _gbtBucket = a })

instance ToPath GetBucketTagging where
    toPath GetBucketTagging{..} = mconcat
        [ "/"
        , toBS _gbtBucket
        ]

instance ToQuery GetBucketTagging where
    toQuery GetBucketTagging{..} = mconcat
        [ "tagging"
        ]

instance ToHeaders GetBucketTagging

instance ToBody GetBucketTagging

newtype GetBucketTaggingResponse = GetBucketTaggingResponse
    { _gbtrsTagSet :: [Tag]
    } deriving (Show, Generic)

gbtrsTagSet :: Lens' GetBucketTaggingResponse [Tag]
gbtrsTagSet = lens _gbtrsTagSet (\s a -> s { _gbtrsTagSet = a })

instance FromXML GetBucketTaggingResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketTagging where
    type Sv GetBucketTagging = S3
    type Rs GetBucketTagging = GetBucketTaggingResponse

    request = get
    response _ = xmlResponse
