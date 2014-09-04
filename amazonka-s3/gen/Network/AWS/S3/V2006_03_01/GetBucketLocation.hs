{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetBucketLocation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the region the bucket resides in.
module Network.AWS.S3.V2006_03_01.GetBucketLocation
    (
    -- * Request
      GetBucketLocation
    -- ** Request constructor
    , getBucketLocation
    -- ** Request lenses
    , gblsBucket

    -- * Response
    , GetBucketLocationResponse
    -- ** Response lenses
    , gblpLocationConstraint
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetBucketLocation' request.
getBucketLocation :: BucketName -- ^ 'gblsBucket'
                  -> GetBucketLocation
getBucketLocation p1 = GetBucketLocation
    { _gblsBucket = p1
    }
{-# INLINE getBucketLocation #-}

data GetBucketLocation = GetBucketLocation
    { _gblsBucket :: BucketName
    } deriving (Show, Generic)

gblsBucket :: Lens' GetBucketLocation (BucketName)
gblsBucket f x =
    f (_gblsBucket x)
        <&> \y -> x { _gblsBucket = y }
{-# INLINE gblsBucket #-}

instance ToPath GetBucketLocation where
    toPath GetBucketLocation{..} = mconcat
        [ "/"
        , toBS _gblsBucket
        ]

instance ToQuery GetBucketLocation where
    toQuery GetBucketLocation{..} = mconcat
        [ "location"
        ]

instance ToHeaders GetBucketLocation

instance ToBody GetBucketLocation

data GetBucketLocationResponse = GetBucketLocationResponse
    { _gblpLocationConstraint :: Maybe BucketLocationConstraint
    } deriving (Show, Generic)

gblpLocationConstraint :: Lens' GetBucketLocationResponse (Maybe BucketLocationConstraint)
gblpLocationConstraint f x =
    f (_gblpLocationConstraint x)
        <&> \y -> x { _gblpLocationConstraint = y }
{-# INLINE gblpLocationConstraint #-}

instance FromXML GetBucketLocationResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketLocation where
    type Sv GetBucketLocation = S3
    type Rs GetBucketLocation = GetBucketLocationResponse

    request = get
    response _ = xmlResponse
