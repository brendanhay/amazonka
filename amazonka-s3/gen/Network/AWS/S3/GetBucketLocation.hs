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
    , gblBucket

    -- * Response
    , GetBucketLocationOutput
    -- ** Response constructor
    , getBucketLocationResponse
    -- ** Response lenses
    , gbloLocationConstraint
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

newtype GetBucketLocation = GetBucketLocation
    { _gblBucket :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'GetBucketLocation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gblBucket' @::@ 'Text'
--
getBucketLocation :: Text -- ^ 'gblBucket'
                  -> GetBucketLocation
getBucketLocation p1 = GetBucketLocation
    { _gblBucket = p1
    }

gblBucket :: Lens' GetBucketLocation Text
gblBucket = lens _gblBucket (\s a -> s { _gblBucket = a })

instance ToPath GetBucketLocation where
    toPath GetBucketLocation{..} = mconcat
        [ "/"
        , toText _gblBucket
        ]

instance ToQuery GetBucketLocation where
    toQuery = const "location"

instance ToHeaders GetBucketLocation

newtype GetBucketLocationOutput = GetBucketLocationOutput
    { _gbloLocationConstraint :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetBucketLocationOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbloLocationConstraint' @::@ 'Maybe' 'Text'
--
getBucketLocationResponse :: GetBucketLocationOutput
getBucketLocationResponse = GetBucketLocationOutput
    { _gbloLocationConstraint = Nothing
    }

gbloLocationConstraint :: Lens' GetBucketLocationOutput (Maybe Text)
gbloLocationConstraint =
    lens _gbloLocationConstraint (\s a -> s { _gbloLocationConstraint = a })

instance AWSRequest GetBucketLocation where
    type Sv GetBucketLocation = S3
    type Rs GetBucketLocation = GetBucketLocationOutput

    request  = get
    response = xmlResponse $ \h x -> GetBucketLocationOutput
        <$> x %| "LocationConstraint"
