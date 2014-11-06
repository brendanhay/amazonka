{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketLocation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the region the bucket resides in.
module Network.AWS.S3.GetBucketLocation
    (
    -- * Request
      GetBucketLocation
    -- ** Request constructor
    , getBucketLocation
    -- ** Request lenses
    , gblrBucket

    -- * Response
    , GetBucketLocationOutput
    -- ** Response constructor
    , getBucketLocationOutput
    -- ** Response lenses
    , gbloLocationConstraint
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.XML
import Network.AWS.S3.Types

newtype GetBucketLocation = GetBucketLocation
    { _gblrBucket :: BucketName
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetBucketLocation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gblrBucket' @::@ 'BucketName'
--
getBucketLocation :: BucketName -- ^ 'gblrBucket'
                  -> GetBucketLocation
getBucketLocation p1 = GetBucketLocation
    { _gblrBucket = p1
    }

gblrBucket :: Lens' GetBucketLocation BucketName
gblrBucket = lens _gblrBucket (\s a -> s { _gblrBucket = a })

instance ToPath GetBucketLocation where
    toPath GetBucketLocation{..} = mconcat
        [ "/"
        , toText _gblrBucket
        ]

instance ToQuery GetBucketLocation where
    toQuery = const "location"

instance ToHeaders GetBucketLocation

newtype GetBucketLocationOutput = GetBucketLocationOutput
    { _gbloLocationConstraint :: Maybe Region
    } deriving (Eq, Ord, Show, Generic)

instance AWSRequest GetBucketLocation where
    type Sv GetBucketLocation = S3
    type Rs GetBucketLocation = GetBucketLocationOutput

    request  = get
    response = const . xmlResponse $ \h x ->
        <$> x %| "LocationConstraint"
