{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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

import Network.AWS.DirectConnect.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data DescribeLocations = DescribeLocations
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeLocations' request.
describeLocations :: DescribeLocations
describeLocations = DescribeLocations

instance ToPath DescribeLocations

instance ToQuery DescribeLocations

instance ToHeaders DescribeLocations

instance ToJSON DescribeLocations

newtype DescribeLocationsResponse = DescribeLocationsResponse
    { _dlrLocations :: [Location]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeLocationsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Locations ::@ @[Location]@
--
describeLocationsResponse :: DescribeLocationsResponse
describeLocationsResponse = DescribeLocationsResponse
    { _dlrLocations = mempty
    }

dlrLocations :: Lens' DescribeLocationsResponse [Location]
dlrLocations = lens _dlrLocations (\s a -> s { _dlrLocations = a })

instance FromJSON DescribeLocationsResponse

instance AWSRequest DescribeLocations where
    type Sv DescribeLocations = DirectConnect
    type Rs DescribeLocations = DescribeLocationsResponse

    request = get
    response _ = jsonResponse
