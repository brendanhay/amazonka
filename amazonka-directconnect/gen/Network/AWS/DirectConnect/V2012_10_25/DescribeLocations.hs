{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.V2012_10_25.DescribeLocations
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
module Network.AWS.DirectConnect.V2012_10_25.DescribeLocations
    (
    -- * Request
      DescribeLocations
    -- ** Request constructor
    , describeLocations
    -- * Response
    , DescribeLocationsResponse
    -- ** Response lenses
    , lsLocations
    ) where

import           Network.AWS.DirectConnect.V2012_10_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DescribeLocations' request.
describeLocations :: DescribeLocations
describeLocations = DescribeLocations
{-# INLINE describeLocations #-}

data DescribeLocations = DescribeLocations
    deriving (Eq, Show, Generic)

instance ToPath DescribeLocations

instance ToQuery DescribeLocations

instance ToHeaders DescribeLocations

instance ToJSON DescribeLocations

data DescribeLocationsResponse = DescribeLocationsResponse
    { _lsLocations :: [Location]
    } deriving (Show, Generic)

lsLocations :: Lens' DescribeLocationsResponse ([Location])
lsLocations f x =
    f (_lsLocations x)
        <&> \y -> x { _lsLocations = y }
{-# INLINE lsLocations #-}

instance FromJSON DescribeLocationsResponse

instance AWSRequest DescribeLocations where
    type Sv DescribeLocations = DirectConnect
    type Rs DescribeLocations = DescribeLocationsResponse

    request = get
    response _ = jsonResponse
