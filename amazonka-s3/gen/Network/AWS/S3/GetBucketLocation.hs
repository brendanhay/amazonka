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

-- Module      : Network.AWS.S3.GetBucketLocation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns the region the bucket resides in.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketLocation.html>
module Network.AWS.S3.GetBucketLocation
    (
    -- * Request
      GetBucketLocation
    -- ** Request constructor
    , getBucketLocation
    -- ** Request lenses
    , gblBucket

    -- * Response
    , GetBucketLocationResponse
    -- ** Response constructor
    , getBucketLocationResponse
    -- ** Response lenses
    , gblrLocationConstraint
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.S3
import Network.AWS.S3.Types
import qualified GHC.Exts

newtype GetBucketLocation = GetBucketLocation
    { _gblBucket :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

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

newtype GetBucketLocationResponse = GetBucketLocationResponse
    { _gblrLocationConstraint :: Maybe Region
    } deriving (Eq, Read, Show)

-- | 'GetBucketLocationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gblrLocationConstraint' @::@ 'Maybe' 'Region'
--
getBucketLocationResponse :: GetBucketLocationResponse
getBucketLocationResponse = GetBucketLocationResponse
    { _gblrLocationConstraint = Nothing
    }

gblrLocationConstraint :: Lens' GetBucketLocationResponse (Maybe Region)
gblrLocationConstraint =
    lens _gblrLocationConstraint (\s a -> s { _gblrLocationConstraint = a })

instance ToPath GetBucketLocation where
    toPath GetBucketLocation{..} = mconcat
        [ "/"
        , toText _gblBucket
        ]

instance ToQuery GetBucketLocation where
    toQuery = const "location"

instance ToHeaders GetBucketLocation

instance ToXMLRoot GetBucketLocation where
    toXMLRoot = const (namespaced ns "GetBucketLocation" [])

instance ToXML GetBucketLocation

instance AWSRequest GetBucketLocation where
    type Sv GetBucketLocation = S3
    type Rs GetBucketLocation = GetBucketLocationResponse

    request  = get
    response = xmlResponse

instance FromXML GetBucketLocationResponse where
    parseXML x = GetBucketLocationResponse
        <$> x .@? "LocationConstraint"
