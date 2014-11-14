{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.DirectConnect.DescribeLocations
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the list of AWS Direct Connect locations in the current AWS region.
-- These are the locations that may be selected when calling CreateConnection
-- or CreateInterconnect.
module Network.AWS.DirectConnect.DescribeLocations
    (
    -- * Request
      DescribeLocations
    -- ** Request constructor
    , describeLocations

    -- * Response
    , DescribeLocationsResponse
    -- ** Response constructor
    , describeLocationsResponse
    -- ** Response lenses
    , dlrLocations
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.DirectConnect.Types

data DescribeLocations = DescribeLocations
    deriving (Eq, Ord, Show, Generic)

-- | 'DescribeLocations' constructor.
describeLocations :: DescribeLocations
describeLocations = DescribeLocations

instance ToPath DescribeLocations where
    toPath = const "/"

instance ToQuery DescribeLocations where
    toQuery = const mempty

instance ToHeaders DescribeLocations

instance ToBody DescribeLocations

newtype DescribeLocationsResponse = DescribeLocationsResponse
    { _dlrLocations :: [Location]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeLocationsResponse where
    type Item DescribeLocationsResponse = Location

    fromList = DescribeLocationsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dlrLocations

-- | 'DescribeLocationsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlrLocations' @::@ ['Location']
--
describeLocationsResponse :: DescribeLocationsResponse
describeLocationsResponse = DescribeLocationsResponse
    { _dlrLocations = mempty
    }

dlrLocations :: Lens' DescribeLocationsResponse [Location]
dlrLocations = lens _dlrLocations (\s a -> s { _dlrLocations = a })

instance AWSRequest DescribeLocations where
    type Sv DescribeLocations = DirectConnect
    type Rs DescribeLocations = DescribeLocationsResponse

    request  = post
    response = jsonResponse $ \h o -> DescribeLocationsResponse
        <$> o .: "locations"
