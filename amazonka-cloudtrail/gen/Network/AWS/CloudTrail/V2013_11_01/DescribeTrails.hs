{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudTrail.V2013_11_01.DescribeTrails
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
module Network.AWS.CloudTrail.V2013_11_01.DescribeTrails
    (
    -- * Request
      DescribeTrails
    -- ** Request constructor
    , mkDescribeTrails
    -- ** Request lenses
    , dt1TrailNameList

    -- * Response
    , DescribeTrailsResponse
    -- ** Response lenses
    , dtrrTrailList
    ) where

import Network.AWS.CloudTrail.V2013_11_01.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Returns information about the trail.
newtype DescribeTrails = DescribeTrails
    { _dt1TrailNameList :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTrails' request.
mkDescribeTrails :: DescribeTrails
mkDescribeTrails = DescribeTrails
    { _dt1TrailNameList = mempty
    }

-- | The trail returned.
dt1TrailNameList :: Lens' DescribeTrails [Text]
dt1TrailNameList =
    lens _dt1TrailNameList (\s a -> s { _dt1TrailNameList = a })

instance ToPath DescribeTrails

instance ToQuery DescribeTrails

instance ToHeaders DescribeTrails

instance ToJSON DescribeTrails

-- | Returns the objects or data listed below if successful. Otherwise, returns
-- an error.
newtype DescribeTrailsResponse = DescribeTrailsResponse
    { _dtrrTrailList :: [Trail]
    } deriving (Show, Generic)

-- | The list of trails.
dtrrTrailList :: Lens' DescribeTrailsResponse [Trail]
dtrrTrailList = lens _dtrrTrailList (\s a -> s { _dtrrTrailList = a })

instance FromJSON DescribeTrailsResponse

instance AWSRequest DescribeTrails where
    type Sv DescribeTrails = CloudTrail
    type Rs DescribeTrails = DescribeTrailsResponse

    request = get
    response _ = jsonResponse
