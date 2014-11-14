{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.CloudTrail.DescribeTrails
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves settings for the trail associated with the current region for
-- your account.
module Network.AWS.CloudTrail.DescribeTrails
    (
    -- * Request
      DescribeTrails
    -- ** Request constructor
    , describeTrails
    -- ** Request lenses
    , dtTrailNameList

    -- * Response
    , DescribeTrailsResponse
    -- ** Response constructor
    , describeTrailsResponse
    -- ** Response lenses
    , dtrTrailList
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudTrail.Types

newtype DescribeTrails = DescribeTrails
    { _dtTrailNameList :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeTrails where
    type Item DescribeTrails = Text

    fromList = DescribeTrails . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dtTrailNameList

-- | 'DescribeTrails' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtTrailNameList' @::@ ['Text']
--
describeTrails :: DescribeTrails
describeTrails = DescribeTrails
    { _dtTrailNameList = mempty
    }

-- | The trail returned.
dtTrailNameList :: Lens' DescribeTrails [Text]
dtTrailNameList = lens _dtTrailNameList (\s a -> s { _dtTrailNameList = a })

instance ToPath DescribeTrails where
    toPath = const "/"

instance ToQuery DescribeTrails where
    toQuery = const mempty

instance ToHeaders DescribeTrails

instance ToBody DescribeTrails where
    toBody = toBody . encode . _dtTrailNameList

newtype DescribeTrailsResponse = DescribeTrailsResponse
    { _dtrTrailList :: [Trail]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeTrailsResponse where
    type Item DescribeTrailsResponse = Trail

    fromList = DescribeTrailsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dtrTrailList

-- | 'DescribeTrailsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrTrailList' @::@ ['Trail']
--
describeTrailsResponse :: DescribeTrailsResponse
describeTrailsResponse = DescribeTrailsResponse
    { _dtrTrailList = mempty
    }

-- | The list of trails.
dtrTrailList :: Lens' DescribeTrailsResponse [Trail]
dtrTrailList = lens _dtrTrailList (\s a -> s { _dtrTrailList = a })

instance AWSRequest DescribeTrails where
    type Sv DescribeTrails = CloudTrail
    type Rs DescribeTrails = DescribeTrailsResponse

    request  = post
    response = jsonResponse $ \h o -> DescribeTrailsResponse
        <$> o .: "trailList"
